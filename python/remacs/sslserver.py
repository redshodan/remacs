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

import sys, signal, os

from M2Crypto.SSL.SSLServer import ThreadingSSLServer
from M2Crypto.SSL import SSLError

from remacs import log
from remacs.server import Server
from remacs.servermgr import ServerMgr
from remacs import sslutil

def log_accept(sock, addr):
    log.info("TCP connection from: %s:%s" % (str(addr[0]), str(addr[1])))

from M2Crypto.SSL import _Connection
_Connection.log_accept = log_accept

class SSLServerMgr(ThreadingSSLServer, ServerMgr):
    def __init__(self, options):
        ServerMgr.__init__(self)
        self.options = options
        self.util = sslutil.SSLUtil(True, options.cacert, options.cert)
        ThreadingSSLServer.__init__(self,
                                    (self.options.host,
                                     int(self.options.sslport)),
                                    SSLServer, self.util.ctx)
        self.socket.postConnectionCheck = self.util.postConnectionCheck
        self.util.sock = self.socket
        self._shutdown = False

    def sighandler(self, signum, frame):
        log.info("Caught signal, shutting down")
        signal.signal(signal.SIGINT, signal.SIG_DFL)
        os.kill(os.getpid(), signal.SIGINT)
        self.shutdown()

    def run(self):
        signal.signal(signal.SIGINT, self.sighandler)
        while not self._shutdown:
            try:
                self.serve_forever()
            except SSLError:
                self.handle_error(None, None)
            except KeyboardInterrupt:
                pass

    def shutdown(self):
        self._shutdown = True
        ServerMgr.shutdown(self)
        ThreadingSSLServer.shutdown(self)

    #
    # ThreadingSSLServer overrides
    #
    def handle_error(self, request, client_address):
        (foo, e, bar) = sys.exc_info()
        if (request == None) and hasattr(e, "sock"):
            request = "fileno=%d" % e.sock.fileno()
        if (client_address == None) and hasattr(e, "addr"):
            client_address = e.addr
        log.error("Error '%s': %s : %s", str(e), client_address, request)


class SSLServer(Server):
    def __init__(self, request, client_address, servermgr):
        super(SSLServer, self).__init__(servermgr.options)
        self.request = request
        self.client_address = client_address
        self.servermgr = servermgr
        log.info("Accepted SSL connection from: %s:%s", str(client_address[0]),
                 str(client_address[1]))
        self.run()

    def setupInOut(self):
        self.fdin = self.request
        self.fdout = self.request
