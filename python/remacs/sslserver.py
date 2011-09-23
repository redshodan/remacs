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

from M2Crypto.SSL.SSLServer import ThreadingSSLServer
from M2Crypto.SSL import SSLError

from remacs import log
from remacs.server import Server
from remacs import sslutil


class SSLServerPort(ThreadingSSLServer):
    def __init__(self, options):
        self.options = options
        self.ctx = sslutil.create_ctx(options)
        ThreadingSSLServer.__init__(self,
                                    (self.options.host,
                                     int(self.options.sslport)),
                                    SSLServer, self.ctx)
        self._shutdown = False

    def handle_error(self, request, client_address):
        (foo, e, bar) = sys.exc_info()
        if (request == None) and hasattr(e, "sock"):
            request = "fileno=%d" % e.sock.fileno()
        if (client_address == None) and hasattr(e, "addr"):
            client_address = e.addr
        log.error("Error '%s': %s : %s", str(e), client_address, request)

    def run(self):
        while not self._shutdown:
            try:
                self.serve_forever()
            except SSLError:
                self.handle_error(None, None)

    def shutdown(self):
        self._shutdown = True
        ThreadingSSLServer.shutdown(self)

class SSLServer(Server):
    def __init__(self, request, client_address, server):
        super(SSLServer, self).__init__(server.options)
        self.request = request
        self.client_address= client_address
        self.server = server
        log.info("SSLServer: %s - %s - %s", request, client_address, server)
        log.info("Connection from: %s:%s" %
                 (client_address[0], client_address[1]))
        self.run()

    def setupInOut(self):
        self.fdin = self.request
        self.fdout = self.request
