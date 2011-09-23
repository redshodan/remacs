#!/usr/bin/python
#
# Copyright (C) 2009 Chris Newton <redshodan@gmail.com>
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#
#
# Author: Chris Newton <redshodan@gmail.com>
# $Revision$
#

import os, sys, pty, termios, socket, struct, fcntl
import errno

from remacs import log, dom, toxml
from remacs.pipebuff import PipeBuff
from remacs.ttymanager import TTYManager


class Server(object):
    def __init__(self, options):
        self.options = options
        self.fdin = None
        self.fdout = None
        self.sock = None
        self.emacs_pid = None

    def setupInOut(self):
        self.fdin = sys.stdin
        self.fdout = sys.stdout

    def setupSock(self):
        self.sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self.sock.connect("/tmp/remacs%d/remacs" % os.geteuid())
        log.info("connected to emacs server")
        self.sock.setblocking(0)
        log.debug("sock fd=%d" % self.sock.fileno())

    def setupTTY(self):
        (self.tty, self.slave) = pty.openpty()
        self.tty = os.fdopen(self.tty, "r+", 0)
        fcntl.ioctl(self.tty, termios.TIOCSWINSZ,
                    struct.pack("HHHH", 24, 80, 0, 0))
        
    def run(self):
        try:
            self.setupInOut()
            self.setupSock()
            self.setupTTY()
            self.mgr = TTYManager(self.fdin, self.fdout, self.tty, self.cmd_cb,
                                  [self.sock], self.sock_cb)
            self.mgr.run()
        except Exception, e:
            log.exception("Main loop exception", e)
            print "Main loop exception: %s %s" % (type(e), str(e))
            import traceback
            traceback.print_exc()
        finally:
            self.mgr.close()

    def sendToEmacs(self, cmd):
        buff = cmd + "\000"
        log.verb("sendToEmacs:" + buff)
        self.sock.send(buff)

    def receivedFromEmacs(self, buff):
        log.verb("Received from emacs: %s" % buff)
        d = dom.parseString(buff)
        elem = d.firstChild
        if elem.nodeName in ["query"]:
            if elem.firstChild.nodeName == "emacs":
                self.emacs_pid = elem.firstChild.getAttribute("pid")
                log.info("Emacs PID=%s" % self.emacs_pid)
            else:
                self.mgr.sendCmd(PipeBuff.CMD_CMD, buff)
        else:
            log.error("Invalid command from emacs")
        d.unlink()

    def sock_cb(self, fd):
        log.debug("sock_cb")
        buff = None
        try:
            buff = self.sock.recv(4096)
            log.debug("sock_cb: read: %d: %s" % (len(buff), buff))
        except OSError, e:
            if e.errno == errno.EAGAIN:
                log.debug("sock_cb: EAGAIN")
                return
            else:
                log.exception("sock_cb: EXCEPT: %s" % e)
                raise
        if buff:
            for line in buff.split("\000"):
                if line and len(line):
                    self.receivedFromEmacs(line)
        else:
            raise Exception("Lost connection to emacs")
        
    def cmd_cb(self, cmd, data):
        log.verb("SERVER CMD: %s DATA: %s" % (cmd, data))
        if cmd == PipeBuff.CMD_TTY:
            return data
        elif cmd == PipeBuff.CMD_CMD:
            d = dom.parseString(data)
            elem = d.firstChild
            if elem.nodeName in ["query"]:
                if elem.firstChild.nodeName == "setup":
                    tty = elem.firstChild.getElementsByTagName("tty")
                    if tty:
                        tty = tty[0]
                        row = int(tty.getAttribute("row"))
                        col = int(tty.getAttribute("col"))
                        buff = struct.pack("HHHH", row, col, 0, 0)
                        log.verb("Setting winsize: %s %s" % (row, col))
                        fcntl.ioctl(self.tty, termios.TIOCSWINSZ, buff)
                        tty.setAttribute("name", os.ttyname(self.slave))
                self.sendToEmacs(toxml(elem))
            else:
                log.error("Unkown command: " + data)
            d.unlink()
