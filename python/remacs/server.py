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
        self.started = False
        self.sock = None
        self.pipe = None
        self.emacs_pid = None
    
    def run(self):
        try:
            self.sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            self.sock.connect("/tmp/remacs%d/remacs" % os.geteuid())
            log.info("connected to emacs server")
            self.sock.setblocking(0)
            log.debug("sock fd=%d" % self.sock.fileno())
            self.pipe = self.sock.fileno()
            log.debug("sock pipe fd: %d" % self.pipe)
            (self.tty, self.slave) = pty.openpty()
            fcntl.ioctl(self.tty, termios.TIOCSWINSZ,
                        struct.pack("HHHH", 24, 80, 0, 0))
            self.mgr = TTYManager(sys.stdin.fileno(), sys.stdout.fileno(),
                                  self.tty, self.cmd_cb,
                                  [self.pipe], self.pipe_cb)
            self.mgr.run()
        except Exception, e:
            print "Main loop exception: %s %s" % (type(e), str(e))
            import traceback
            traceback.print_exc()
        finally:
            self.mgr.close()

    def sendToEmacs(self, cmd):
        buff = cmd + "\000"
        log.verb("sendToEmacs:" + buff)
        os.write(self.pipe, buff)

    def receivedFromEmacs(self, buff):
        log.verb("Received from emacs: %s" % buff)
        d = dom.parseString(buff)
        elem = d.firstChild
        if elem.nodeName == "emacs":
            self.emacs_pid = elem.getAttribute("pid")
            log.info("Emacs PID=%s" % self.emacs_pid)
        elif elem.nodeName in ["error", "notify", "suspend"]:
            self.mgr.sendCmd(PipeBuff.CMD_CMD, buff)
        else:
            log.error("Invalid command from emacs")
        d.unlink()

    def pipe_cb(self, fd):
        log.debug("pipe_cb")
        buff = None
        try:
            buff = os.read(self.pipe, 4096)
            log.debug("pipe_cb: read: %d: %s" % (len(buff), buff))
        except OSError, e:
            if e.errno == errno.EAGAIN:
                log.debug("pipe_cb: EAGAIN")
                return
            else:
                log.exception("pipe_cb: EXCEPT: %s" % e)
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
            if elem.nodeName == "setup":
                tty = elem.getElementsByTagName("tty")
                if tty:
                    tty = tty[0]
                    row = int(tty.getAttribute("row"))
                    col = int(tty.getAttribute("col"))
                    buff = struct.pack("HHHH", row, col, 0, 0)
                    log.verb("Setting winsize: %s %s" % (row, col))
                    fcntl.ioctl(self.tty, termios.TIOCSWINSZ, buff)
                    tty.setAttribute("name", os.ttyname(self.slave))
                if not self.started:
                    self.sendToEmacs(toxml(d))
                    self.started = True
            elif elem.nodeName == "notify":
                self.sendToEmacs(data)
            elif elem.nodeName == "resume":
                self.sendToEmacs(data)
            else:
                log.err("Unkown command: " + data)
            d.unlink()
