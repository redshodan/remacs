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

import sys

from M2Crypto import SSL
from M2Crypto.SSL.SSLServer import ThreadingSSLServer
from M2Crypto.SSL import SSLError

from remacs import log
from remacs.server import Server


class SSLServerPort(ThreadingSSLServer):
    def __init__(self, options):
        self.options = options
        self.ctx = SSL.Context("tlsv1")
        ThreadingSSLServer.__init__(self, ("0.0.0.0", int(self.options.sslport)),
                                    SSLServer, self.ctx)
        # Workaround for python >= 2.5
        self._handle_request_noblock = self.handle_request

    def handle_error(self, request, client_address):
        (foo, e, bar) = sys.exc_info()
        if (request == None) and hasattr(e, "sock"):
            request = "fileno=%d" % e.sock.fileno()
        if (client_address == None) and hasattr(e, "addr"):
            client_address = e.addr
        log.exception(e, "Error from '%s': %s", client_address, request)

    def run(self):
        self.serve_forever()


class SSLServer(object):
    def __init__(self, request, client_address, server):
        print request, client_address, server

    # def __init__(self, options, ):
    #     super(SSLServer, self).__init__(options)

    def setupInOut(self):
        pass
