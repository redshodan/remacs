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


import remacs
from remacs import log
from remacs.pipebuff import PipeBuff
from remacs.acker import DEFAULT_WINDOW
from . import BaseProtocolTestCase, callback, callbackN, royal_we
from utils import *


class BasicProtocolTests(BaseProtocolTestCase):
    def setUp(self):
        BaseProtocolTestCase.setUp(self)
        self.c_opts.cert = CLIENT1
        self.c_opts.cacert = CACERT1
        self.c_opts.crl = None
        self.s_opts.cert = SERVER1
        self.s_opts.cacert = CACERT1
        self.s_opts.crl = None
        
    def test_basicClient(self):
        @callback
        def cb(cmd, data):
            self.assertEquals(PipeBuff.CMD_CMD, cmd)
            self.assertEquals(self.cmd, data)
        self.init(cb, cb)
        self.cmd = "<query/>"
        self.resetDone()
        self.tty2.mgr.sendCmd(PipeBuff.CMD_CMD, self.cmd)
        self.waitDone()

    def test_basicAcking(self):
        self.cmd = "<query id='%d'/>"
        self.count1 = 0
        self.count2 = 0
        @callbackN
        def cb1(cmd, data):
            royal_we.count1 = royal_we.count1 + 1
            self.assertEquals(PipeBuff.CMD_CMD, cmd)
            self.assertEquals(self.cmd % royal_we.count1, data)
        @callback
        def cb2(cmd, data, buff):
            royal_we.count2 = royal_we.count2 + 1
            self.assertEquals(PipeBuff.CMD_ACK, cmd)
            self.assertEquals(None, data)
            expected = DEFAULT_WINDOW * (royal_we.i + 1)
            self.assertEquals(expected, buff.outacker.ack_cur)
            self.assertEquals(expected, buff.outacker.pkt_count)
            self.assertEquals(0, len(buff.outacker.buffer))
            self.assertEquals(royal_we.count1, 5 * (royal_we.i + 1))
            self.assertEquals(royal_we.count2, 1 + royal_we.i)
        self.init(cb1, None, None, cb2)
        for self.i in range(DEFAULT_WINDOW):
            self.resetDone()
            for j in range(DEFAULT_WINDOW):
                count = self.i * DEFAULT_WINDOW + j + 1
                self.tty2.mgr.sendCmd(PipeBuff.CMD_CMD, self.cmd % count)
            self.waitDone()
