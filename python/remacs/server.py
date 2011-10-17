# Copyright (C) 2009 Chris Newton <redshodan@gmail.com>
#
# This file is part of remacs.
#
# remacs is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# remacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with remacs.  If not, see <http://www.gnu.org/licenses/>.
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
from remacs.protocolbase import ProtocolBase


class Server(ProtocolBase):
    def __init__(self, options):
        super(Server, self).__init__(options)
        self.options = options
        self.fdin = None
        self.fdout = None
        self.sock = None
        self.emacs_pid = None
        self.name = None
        self.tty = None
        self.slave = None

    def setupInOut(self):
        self.fdin = sys.stdin
        self.fdout = sys.stdout

    def setupSock(self):
        self.sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self.sock.connect("/tmp/remacs%d/remacs" % os.geteuid())
        log.info("connected to emacs server")
        self.sock.setblocking(0)
        log.debug("sock fd=%d" % self.sock.fileno())
        self.mgr.setExtras([self.sock], self.sock_cb)

    def setupTTY(self):
        (self.tty, self.slave) = pty.openpty()
        self.tty = os.fdopen(self.tty, "r+", 0)
        fcntl.ioctl(self.tty, termios.TIOCSWINSZ,
                    struct.pack("HHHH", 24, 80, 0, 0))

    def reset(self):
        log.debug("Reseting session %s", self.name)
        self.setupSock()
        self.setupTTY()
        self.mgr.setTTY(self.tty)
        
    def resume(self, acked, old):
        if not old:
            log.debug("Got resume on a non-resumable stream, resetting instead")
            self.reset()
            return False
        log.debug("Resuming session %s with acked=%s", self.name, acked)
        # Take over the resources from the old server
        self.sock = old.sock
        old.sock = None
        self.emacs_pid = old.emacs_pid
        self.tty = old.tty
        old.tty = None
        self.slave = old.slave
        old.slave = None
        self.mgr.inacker = old.mgr.inacker
        old.mgr.inacker = None
        self.mgr.inacker.outpipe = self.mgr.outpipe
        self.mgr.outacker = old.mgr.outacker
        old.mgr.outacker = None
        self.mgr.setTTY(self.tty)
        self.mgr.outacker.resume(acked)
        return True
    
    def run(self):
        try:
            self.setupInOut()
            # self.setupSock()
            # self.setupTTY()
            self.setMgr(TTYManager(self.fdin, self.fdout, self.tty, self.cmd_cb))
            self.mgr.run()
        except Exception, e:
            log.exception("Main loop exception", e)
            print "Main loop exception: %s %s" % (type(e), str(e))
            import traceback
            traceback.print_exc()
        finally:
            self.mgr.close()

    def quit(self):
        self.mgr.quit()

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
                self.sendCmd(buff)
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
                sess_attr = None
                if elem.firstChild.nodeName == "setup":
                    session = elem.firstChild.getElementsByTagName("session")
                    if session:
                        session = session[0]
                        name = session.getAttribute("name")
                        acked = session.getAttribute("acked")
                        action = session.getAttribute("action")
                        if not self.name:
                            self.name = name
                            if action == "reset":
                                self.reset()
                                sess_attr = "reset"
                            elif action == "resume":
                                if self.resume(acked, None):
                                    sess_attr = "resumed"
                                else:
                                    sess_attr = "reset"
                        if not self.name or not self.sock or not sess_attr:
                            raise Exception("Invalid setup command from client")
                    tty = elem.firstChild.getElementsByTagName("tty")
                    if tty:
                        tty = tty[0]
                        if tty.getAttribute("row"):
                            self.row = int(tty.getAttribute("row"))
                            self.col = int(tty.getAttribute("col"))
                            buff = struct.pack("HHHH", self.row, self.col, 0, 0)
                            log.verb("Setting winsize: %s %s" %
                                     (self.row, self.col))
                            fcntl.ioctl(self.tty, termios.TIOCSWINSZ, buff)
                        tty.setAttribute("name", os.ttyname(self.slave))
                # Send upward to Emacs for its info, Emacs won't respond. I love
                # this split server logic.
                self.sendToEmacs(toxml(elem))
                # Now respond
                if sess_attr:
                    elem.setAttribute("session", sess_attr)
                self.respondQuery(elem)
            else:
                log.error("Unkown command: " + data)
            d.unlink()
