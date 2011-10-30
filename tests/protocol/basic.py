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
from . import BaseProtocolTestCase, callback


class BasicProtocolTests(BaseProtocolTestCase):
    def test_basicClient(self):
        @callback
        def cb(cmd, data):
            self.assertEquals(PipeBuff.CMD_CMD, cmd)
            self.assertEquals(self.cmd, data)
        self.init(cb)
        self.cmd = "<query/>"
        self.resetDone()
        self.tty2.mgr.sendCmd(PipeBuff.CMD_CMD, self.cmd)
        self.waitDone()
