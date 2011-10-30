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

import os, threading, socket

import remacs
from remacs import log, ttymanager
from remacs.pipebuff import PipeBuff
from utils import RemacsTestCase, DefaultOpts


SOCKET = "/tmp/remacs.test.sock"
royal_we = None


class TTYMgrRunner(threading.Thread):
    def __init__(self, isserver, cb, tc):
        threading.Thread.__init__(self)
        self.isserver = isserver
        self.cb = cb
        self.tc = tc
    
    def run(self):
        if self.isserver:
            sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            try:
                os.remove(SOCKET)
            except OSError:
                pass
            sock.bind(SOCKET)
            sock.listen(1)
            self.tc.poke()
            self.sock, addr = sock.accept()
            self.mgr = ttymanager.TTYManager(self.sock, self.sock, None, self.cb)
        else:
            self.sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            self.sock.connect(SOCKET)
            self.mgr = ttymanager.TTYManager(self.sock, self.sock, None, self.cb)
            self.tc.poke()
        log.debug("tty isserver=%s: %s" %
                  (str(self.isserver), str(id(self.mgr))))
        self.mgr.run()


class BaseProtocolTestCase(RemacsTestCase):
    def __init__(self, *args, **kwargs):
        RemacsTestCase.__init__(self, *args, **kwargs)
        self.excpt = None
        self.cond = threading.Condition()
        self.done = False

    def setUp(self):
        RemacsTestCase.setUp(self)
        global royal_we
        royal_we = self

    def tearDown(self):
        self.tty1.mgr.quit()
        self.tty2.mgr.quit()
        if self.excpt:
            raise self.excpt
        RemacsTestCase.tearDown(self)
    
    def init(self, cb):
        self.resetDone()
        self.tty1 = TTYMgrRunner(True, cb, self)
        self.tty1.start()
        self.waitDone()
        self.resetDone()
        self.tty2 = TTYMgrRunner(False, cb, self)
        self.tty2.start()
        self.waitDone()

    def resetDone(self):
        self.cond.acquire()
        self.done = False
        self.cond.release()
        
    def poke(self):
        self.cond.acquire()
        self.done = True
        self.cond.notify()
        self.cond.release()
    
    def waitDone(self):
        self.cond.acquire()
        while not self.done:
            self.cond.wait()
        self.cond.release()


def callback(func):
    def newfunc(*args, **kwargs):
        try:
            func(*args, **kwargs)
        except Exception, e:
            royal_we.excpt = e
        finally:
            royal_we.poke()
    return newfunc


from .basic import *
