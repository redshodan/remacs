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

import os, sys, termios, struct, fcntl
import signal, subprocess
 
from remacs import log, dom, toxml
from remacs.pipebuff import PipeBuff
from remacs.ttymanager import TTYManager
from remacs.protocolbase import ProtocolBase


class Client(ProtocolBase):
    def __init__(self, options):
        super(Client, self).__init__(options)
        self.options = options
        self.fdin = None
        self.fdout = None
        self.mgr = None
        self.tray = None
        self.first_setup = True
        self.resuming = False
        self.emacs_suspended = False
        self.cmd = (self.options.transport + " " + self.options.host +
                    " remacs --server")
        log.info("cmd: " + self.cmd)
        if "TERM" in os.environ:
            self.term = os.environ["TERM"]
        else:
            self.term = "xterm"
        self.tty = None
        if self.options.no_x:
            log.info("User disabled X")
        else:
            self.initX()
        if self.tty is None:
            log.info("No VTE setup, continuing on stdin")
            self.tty = open(os.ttyname(sys.stdin.fileno()), "r+", 0)

    def initX(self):
        try:
            if not len(os.environ["DISPLAY"]):
                raise Exception("No DISPLAY variable set")
            from remacs import systray
            self.tray = systray.SysTray(self)
            log.info("Setup systray")
            self.tty = self.tray.startVTE()
            self.tty = os.fdopen(self.tty, "r+", 0)
            log.info("Setup VTE, tty: %s" % self.tty)
            self.tray.start()
            log.info("Setup X")
        except Exception, e:
            log.exception("Failed to setup X interface: " + str(e))

    def setupInOut(self):
        self.pipe = subprocess.Popen(self.cmd, bufsize=0, stdin=subprocess.PIPE,
                                     stdout=subprocess.PIPE,
                                     stderr=subprocess.PIPE, shell=True)
        log.debug("pipe.stdin: %d %s" % (self.pipe.stdin.fileno(),
                                         str(self.pipe.stdin)))
        log.debug("pipe.stdout: %d %s" % (self.pipe.stdout.fileno(),
                                          str(self.pipe.stdout)))
        self.fdin = self.pipe.stdout
        self.fdout = self.pipe.stdin

    def run(self):
        try:
            self.setupInOut()
            signal.signal(signal.SIGWINCH, self.sigWINCH)
            log.debug("self.fdin: %s" % self.fdin.fileno())
            log.debug("self.fdout: %s" % self.fdout.fileno())
            self.setMgr(TTYManager(self.fdin, self.fdout, self.tty, self.cmdCB))
            self.sigWINCH()
            self.mgr.run()
        except Exception, e:
            log.exception("Main loop exception: %s" % str(e))
            import traceback
            traceback.print_exc()
        finally:
            if self.mgr:
                self.mgr.close()
            if self.tray:
                self.tray.stop()

    def quit(self):
        log.info("Client.quit")
        self.mgr.quit()

    def sendSetup(self):
        tty = ("<tty term='%s' row='%d' col='%d'/>" %
               (self.term, self.row, self.col))
        if self.first_setup:
            action = "action='reset'"
            self.first_setup = False
        elif self.resuming:
            action = "action='resume'"
            self.resuming = False
        else:
            action = None
        if action:
            session = ("<session name='%s' acked='%d' %s/>" %
                       (self.options.id, self.mgr.getAcked(), action))
        else:
            session = ""
        self.sendCmd("<query from='%s' type='set'><setup>%s%s</setup></query>" %
                     (self.options.id, tty, session))

    def sigWINCH(self, signum=None, frame=None):
        try:
            buff = 'abcdefgh'
            buff = fcntl.ioctl(self.tty, termios.TIOCGWINSZ, buff)
            (self.row, self.col, x, y) = struct.unpack("HHHH", buff)
            log.verb("winsize %d: %s" % (len(buff), buff))
            self.sendSetup()
        except Exception, e:
            log.exception("sigWINCH: %s" % str(e))
            import traceback
            traceback.print_exc()
            print e

    def cmdCB(self, cmd, data):
        log.verb("CLIENT CMD: %d DATA: %s" % (cmd, data))
        if cmd == PipeBuff.CMD_CMD:
            d = dom.parseString(data)
            elem = d.firstChild
            if elem.nodeName == "query":
                self.handleQuery(cmd, data, elem)
            else:
                log.error("Unkown command: " + data)
            d.unlink()

    def handleQuery(self, cmd, data, elem):
        if elem.getAttribute("type") == "error":
            log.verb("Dropping errored query: %s" % data)
            return
        resp = "result"
        child = elem.firstChild
        if child.nodeName == "error":
            error = child.firstChild.data
            log.verbose("Got error: %s" % error)
            self.tray.error(error)
        elif child.nodeName == "notify":
            if elem.getAttribute("type") == "set":
                id = child.getAttribute("id")
                title = child.firstChild.firstChild.data
                body = child.firstChild.nextSibling.firstChild.data
                msg = title + " : " + body
                log.verb("Got notification: id=%s msg=%s" % (id, msg))
                self.tray.notify(id, msg)
            elif elem.getAttribute("type") == "result":
                id = child.getAttribute("id")
                log.verb("Clearing notification: %s" % id)
                self.tray.clearNotify(id)
        elif child.nodeName == "suspend":
            log.verb("Suspending")
            self.emacs_suspended = True
            self.tray.iconify()
        elif child.nodeName == "unidle":
            if self.tray:
                log.verb("Unidling")
                self.tray.xidler.unidle()
            else:
                log.verb("Ignoring unidle, on console")
        else:
            log.error("Unkown command: " + data)
            resp = "error"
        
    def invokeNotif(self, id):
        log.info("Sending notification invoked: %s" % id)
        self.sendCmd(("<query><notify id='%s' type='result'><invoke/></notify>" +
                      "</query>") % id)

    def readNotif(self, id):
        log.info("Sending notification read: %s" % id)
        self.sendCmd("<query><notify id='%s' type='result'/></query>" % id)

    def resumeEmacs(self):
        if self.emacs_suspended:
            self.emacs_suspended = False
            self.sendCmd("<query><resume/></query>")

    def sendUnidle(self):
        self.sendCmd("<query type='single'><unidle/></query>")
