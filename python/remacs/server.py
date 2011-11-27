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
from remacs.protocolserver import ProtocolServer


class Server(ProtocolServer):
    def __init__(self, options):
        super(Server, self).__init__(options)
        self.fdin = None
        self.fdout = None
        self.sock = None
        self.emacs_pid = None
        self.name = None
        self.tty = None
        self.slave = None
        self.servermgr = None

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
        
    def resume(self, old, acked):
        log.debug("Resuming session %s with acked=%s", self.name, acked)
        # Take over the resources from the old server
        self.sock = old.sock
        old.sock = None
        self.emacs_pid = old.emacs_pid
        self.tty = old.tty
        old.tty = None
        self.slave = old.slave
        old.slave = None
        self.mgr.resume(old.mgr, tty, acked)
        return True
    
    def run(self):
        try:
            self.setupInOut()
            self.setMgr(TTYManager(self.fdin, self.fdout, self.tty, self.cmdCB))
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
