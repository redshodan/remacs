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

import types

from remacs import log, dom, toxml
from remacs.pipebuff import PipeBuff


class ProtocolBase(object):
    def __init__(self, options):
        self.options = options
        self.mgr = None

    def setMgr(self, mgr):
        self.mgr = mgr

    def sendCmd(self, data):
        if not isinstance(data, types.StringTypes):
            data = toxml(data).encode("utf-8")
        log.verb("Sending cmd: %s" % data)
        self.mgr.sendCmd(PipeBuff.CMD_CMD, data)
    
    def respondQuery(self, query, result=True):
        if ((query.getAttribute("type") == "single") or
            (query.getAttribute("type") == "error")):
            return
        if result is True:
            result = "result"
        elif result is False:
            result = "error"
        query.setAttribute("type", result)
        frm = query.getAttribute("from")
        if frm:
            query.setAttribute("to", frm)
        query.setAttribute("from", self.options.id)
        self.mgr.sendCmd(PipeBuff.CMD_CMD, toxml(query).encode("utf-8"))

