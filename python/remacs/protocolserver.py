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

class ProtocolServer(ProtocolBase):
    def __init__(self, options):
        super(ProtocolServer, self).__init__(options)
    
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

    def cmdCB(self, cmd, data):
        log.verb("SERVER CMD: %s DATA: %s" % (cmd, data))
        if cmd == PipeBuff.CMD_CMD:
            d = dom.parseString(data)
            try:
                elem = d.firstChild
                if elem.nodeName in ["query"]:
                    self.handleQuery(elem)
                else:
                    log.error("Unkown command: " + data)
            finally:
                d.unlink()

    def handleQuery(self, elem):
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
                        if not self.servermgr:
                            log.warn("Got resume on a non-resumable stream, " +
                                     "resetting instead")
                            self.reset()
                            sess_attr = "reset"
                        elif self.servermgr.resume(self, acked):
                            sess_attr = "resumed"
                        else:
                            self.servermgr.reset(self)
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
    
