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

import sys, socket


from remacs import log
from remacs.client import Client
from remacs import sslutil


class SSLClient(Client):
    def __init__(self, options):
        super(SSLClient, self).__init__(options)
        self.util = sslutil.SSLUtil(False, options.cacert, options.cert)

    def setupInOut(self):
        self.util.makeSock()
        self.util.sock.connect((self.options.host, int(self.options.sslport)))
        self.fdin = self.util.sock
        self.fdout = self.util.sock
