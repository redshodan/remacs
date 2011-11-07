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
from remacs.protocolclient import ProtocolClient, ClientListener


class TTYListener(ClientListener):
    def __init__(self):
        ClientListener.__init__(self)
    
    def start(self):
        pass

    def stop(self):
        pass

    def setupTTY(self):
        return open(os.ttyname(sys.stdin.fileno()), "r+", 0)
    
    def error(self, error):
        pass

    def notify(self, id, msg):
        pass

    def clearNotify(self, id):
        pass

    def suspend(self):
        pass

    def unidle(self):
        pass


class Client(ProtocolClient):
    def __init__(self, options):
        super(Client, self).__init__(options)
        self.fdin = None
        self.fdout = None
        self.listener = None
        self.cmd = (self.options.transport + " " + self.options.host +
                    " remacs --server")
        log.info("cmd: " + self.cmd)
        self.tty = None
        if self.options.no_x:
            log.info("User disabled X")
        else:
            self.initX()
        if self.listener is None:
            log.info("No systray setup, continuing on stdin")
            self.listener = TTYListener()
            self.tty = self.listener.setupTTY()
        if self.tty is None:
            log.info("No VTE setup, continuing on stdin")
            self.tty = open(os.ttyname(sys.stdin.fileno()), "r+", 0)

    def initX(self):
        try:
            if not len(os.environ["DISPLAY"]):
                raise Exception("No DISPLAY variable set")
            from remacs import systray
            self.listener = systray.SysTray(self)
            log.info("Setup systray")
            if not self.options.no_vte:
                self.tty = self.listener.setupTTY()
            self.listener.start()
            log.info("Setup X")
        except Exception, e:
            log.exception(e, "Failed to setup X interface")
            if self.listener:
                try:
                    self.listener.stop()
                except:
                    pass
            self.listener = None
            self.tty = None

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
            self.listener.stop()

    def sigWINCH(self, signum=None, frame=None):
        try:
            buff = 'abcdefgh'
            buff = fcntl.ioctl(self.tty, termios.TIOCGWINSZ, buff)
            (self.options.row, self.options.col, x, y) = \
                struct.unpack("HHHH", buff)
            log.verb("winsize %d: %s" % (len(buff), buff))
            self.sendSetup()
        except Exception, e:
            log.exception("sigWINCH: %s" % str(e))
            import traceback
            traceback.print_exc()
            print e
