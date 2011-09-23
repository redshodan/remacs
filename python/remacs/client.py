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

import os, sys, termios, struct, fcntl
import signal, subprocess
 
from remacs import log, dom, toxml
from remacs.pipebuff import PipeBuff
from remacs.ttymanager import TTYManager


class Client(object):
    def __init__(self, options):
        self.options = options
        self.fdin = None
        self.fdout = None
        self.emacs_suspended = False
        self.cmd = (self.options.transport + " -T " + self.options.host +
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
            self.mgr = TTYManager(self.fdin, self.fdout, self.tty, self.cmdCB)
            self.sigWINCH()
            self.mgr.run()
        except Exception, e:
            log.exception("Main loop exception: %s" % str(e))
            import traceback
            traceback.print_exc()
        finally:
            self.mgr.close()

    def quit(self):
        log.info("Client.quit")
        self.mgr.quit()

    def sigWINCH(self, signum=None, frame=None):
        try:
            buff = 'abcdefgh'
            buff = fcntl.ioctl(self.tty, termios.TIOCGWINSZ, buff)
            (row, col, x, y) = struct.unpack("HHHH", buff)
            log.verb("winsize %d: %s" % (len(buff), buff))
            self.mgr.sendCmd(
                PipeBuff.CMD_CMD,
                ("<query><setup><tty term='%s' row='%d' col='%d'/>" +
                     "<id name='%s'/></setup></query>") %
                (self.term, row, col, self.options.id))
        except Exception, e:
            log.exception("sigWINCH: %s" % str(e))
            import traceback
            traceback.print_exc()
            print e

    def cmdCB(self, cmd, data):
        log.verb("client.cmdCB: %s" % data)
        if cmd == PipeBuff.CMD_CMD:
            d = dom.parseString(data)
            elem = d.firstChild
            if elem.nodeName == "query":
                child = elem.firstChild
                if child.nodeName == "error":
                    error = child.firstChild.data
                    self.tray.error(error)
                elif child.nodeName == "notify":
                    if elem.getAttribute("type") == "set":
                        title = child.firstChild.firstChild.data
                        body = child.firstChild.nextSibling.firstChild.data
                        self.tray.notify(child.getAttribute("id"),
                                         title + " : " + body)
                    elif elem.getAttribute("type") == "result":
                        self.tray.clearNotify(child.getAttribute("id"))
                elif child.nodeName == "suspend":
                    self.emacs_suspended = True
                    self.tray.iconify()
                elif child.nodeName == "unidle":
                    self.tray.xidler.unidle()
                else:
                    log.error("Unkown command: " + data)
            else:
                log.error("Unkown command: " + data)
            d.unlink()
        else:
            return None

    def invokeNotif(self, id):
        log.info("invokeNotif: %s" % id)
        self.mgr.sendCmd(
            PipeBuff.CMD_CMD,
            ("<query><notify id='%s' type='result'><invoke/></notify>" +
             "</query>") % id)

    def readNotif(self, id):
        log.info("readNotif: %s" % id)
        self.mgr.sendCmd(PipeBuff.CMD_CMD,
                         "<query><notify id='%s' type='result'/></query>" % id)

    def resumeEmacs(self):
        if self.emacs_suspended:
            self.emacs_suspended = False
            self.mgr.sendCmd(PipeBuff.CMD_CMD, "<query><resume/></query>")

    def sendUnidle(self):
        log.info("sendUnidle")
        self.mgr.sendCmd(PipeBuff.CMD_CMD, "<query><unidle/></query>")
