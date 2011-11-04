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

import os, threading, socket, time

import remacs
from remacs import log, ttymanager
from remacs.pipebuff import PipeBuff
from utils import RemacsTestCase, DefaultOpts, ModuleRef


SOCKET = "/tmp/remacs.test.sock"
royal_we = ModuleRef(None)


class TTYMgrRunner(threading.Thread):
    def __init__(self, isserver, cb, all_cb, tc):
        threading.Thread.__init__(self)
        self.isserver = isserver
        self.cb = cb
        self.all_cb = all_cb
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
            self.mgr.inpipe.buff.all_cb = self.all_cb
        else:
            self.sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            self.sock.connect(SOCKET)
            self.mgr = ttymanager.TTYManager(self.sock, self.sock, None, self.cb)
            self.mgr.inpipe.buff.all_cb = self.all_cb
            self.tc.poke()
        log.debug("tty isserver=%s: %s" %
                  (str(self.isserver), str(id(self.mgr))))
        try:
            self.mgr.run()
        except Exception, e:
            log.exception(e)
            royal_we.excpt.append(e)


class BaseProtocolTestCase(RemacsTestCase):
    def __init__(self, *args, **kwargs):
        RemacsTestCase.__init__(self, *args, **kwargs)
        self.excpt = []
        self.cond = threading.Condition()
        self.done = False

    def setUp(self):
        RemacsTestCase.setUp(self)
        royal_we.__setobj__(self)

    def tearDown(self):
        self.tty1.mgr.quit()
        self.tty2.mgr.quit()
        self.tty1.join()
        self.tty2.join()
        if len(self.excpt):
            raise Exception(str(self.excpt))
        RemacsTestCase.tearDown(self)
    
    def init(self, cb1, cb2, all1=None, all2=None):
        self.resetDone()
        self.tty1 = TTYMgrRunner(True, cb1, all1, self)
        self.tty1.start()
        self.waitDone()
        self.resetDone()
        self.tty2 = TTYMgrRunner(False, cb2, all2, self)
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
    
    def waitDone(self, timeout=5):
        self.cond.acquire()
        end = time.time() + 5
        while not self.done:
            self.cond.wait(timeout)
            if end < time.time():
                raise Exception("waitDone timedout: "  + str(self.excpt))
                break
        self.cond.release()


def callback(func):
    def newfunc(*args, **kwargs):
        try:
            func(*args, **kwargs)
        except Exception, e:
            log.exception(e)
            royal_we.excpt.append(e)
        finally:
            royal_we.poke()
    return newfunc

def callbackN(func):
    def newfunc(*args, **kwargs):
        try:
            func(*args, **kwargs)
        except Exception, e:
            log.exception(e)
            royal_we.excpt.append(e)
    return newfunc


from .basic import *
