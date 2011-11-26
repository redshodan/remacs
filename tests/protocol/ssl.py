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

from M2Crypto import X509, m2
from M2Crypto.SSL import SSLError

import remacs
from remacs import log
from remacs.pipebuff import PipeBuff
from remacs.acker import DEFAULT_WINDOW
from . import BaseProtocolTestCase, callback, callbackN, royal_we
from utils import *


class SSLProtocolTests(BaseProtocolTestCase):
    def _connect(self):
        @callback
        def cb(cmd, data):
            self.assertEquals(PipeBuff.CMD_CMD, cmd)
            self.assertEquals(self.cmd, data)
        self.assertTrue(self.init(cb, cb), "Failed to connect")
        self.cmd = "<query/>"
        self.resetDone()
        self.tty2.mgr.sendCmd(PipeBuff.CMD_CMD, self.cmd)
        self.waitDone()

    def test_connect(self):
        self.c_opts.cert = CLIENT1
        self.c_opts.cacert = CACERT1
        self.c_opts.crl = None
        self.s_opts.cert = SERVER1
        self.s_opts.cacert = CACERT1
        self.s_opts.crl = None
        self._connect()

    def test_connectWithCRL(self):
        self.c_opts.cert = CLIENT1
        self.c_opts.cacert = CACERT1
        self.c_opts.crl = CRL1
        self.s_opts.cert = SERVER1
        self.s_opts.cacert = CACERT1
        self.s_opts.crl = CRL1
        self._connect()

    ##########################################################################

    def test_connectRevokedClientWithCRL(self):
        self.c_opts.cert = REVOKED_CLIENT1
        self.c_opts.cacert = CACERT1
        self.c_opts.crl = CRL1
        self.s_opts.cert = SERVER1
        self.s_opts.cacert = CACERT1
        self.s_opts.crl = CRL1
        self.init(None, None)
        self.stop()
        self.assertEquals(len(self.excpt), 1, "More than one side failed")
        self.assertEquals(type(self.excpt[0]), X509.X509Error,
                          "Both sides did not fail")
        self.assertEquals(self.excpt[0].args[1], m2.X509_V_ERR_CERT_REVOKED,
                          "Wrong type of failure")
        self.excpt = []

    def test_connectRevokedClientWithCRL2(self):
        self.c_opts.cert = REVOKED_CLIENT2
        self.c_opts.cacert = CACERT2
        self.c_opts.crl = CRL2
        self.s_opts.cert = SERVER2
        self.s_opts.cacert = CACERT2
        self.s_opts.crl = CRL2
        self.init(None, None)
        self.stop()
        self.assertEquals(len(self.excpt), 1, "More than one side failed")
        self.assertEquals(type(self.excpt[0]), X509.X509Error,
                          "Both sides did not fail")
        self.assertEquals(self.excpt[0].args[1], m2.X509_V_ERR_CERT_REVOKED,
                          "Wrong type of failure")
        self.excpt = []

    ##########################################################################

    def test_connectRevokedServerWithCRL(self):
        self.c_opts.cert = CLIENT1
        self.c_opts.cacert = CACERT1
        self.c_opts.crl = CRL1
        self.s_opts.cert = REVOKED_SERVER1
        self.s_opts.cacert = CACERT1
        self.s_opts.crl = CRL1
        self.init(None, None)
        self.stop()
        self.assertEquals(len(self.excpt), 1, "More than one side failed")
        self.assertEquals(type(self.excpt[0]), X509.X509Error,
                          "Both sides did not fail")
        self.assertEquals(self.excpt[0].args[1], m2.X509_V_ERR_CERT_REVOKED,
                          "Wrong type of failure")
        self.excpt = []

    def test_connectRevokedServerWithCRL2(self):
        self.c_opts.cert = CLIENT2
        self.c_opts.cacert = CACERT2
        self.c_opts.crl = CRL2
        self.s_opts.cert = REVOKED_SERVER2
        self.s_opts.cacert = CACERT2
        self.s_opts.crl = CRL2
        self.init(None, None)
        self.stop()
        self.assertEquals(len(self.excpt), 1, "More than one side failed")
        self.assertEquals(type(self.excpt[0]), X509.X509Error,
                          "Both sides did not fail")
        self.assertEquals(self.excpt[0].args[1], m2.X509_V_ERR_CERT_REVOKED,
                          "Wrong type of failure")
        self.excpt = []

    ##########################################################################

    def test_connectRevokedClientWithCRLBothCAs(self):
        self.c_opts.cert = REVOKED_CLIENT1
        self.c_opts.cacert = CACERTBOTH
        self.c_opts.crl = CRLBOTH
        self.s_opts.cert = SERVER1
        self.s_opts.cacert = CACERTBOTH
        self.s_opts.crl = CRLBOTH
        self.init(None, None)
        self.stop()
        self.assertEquals(len(self.excpt), 1, "More than one side failed")
        self.assertEquals(type(self.excpt[0]), X509.X509Error,
                          "Both sides did not fail")
        self.assertEquals(self.excpt[0].args[1], m2.X509_V_ERR_CERT_REVOKED,
                          "Wrong type of failure")
        self.excpt = []

    def test_connectRevokedClientWithCRLBothCAs2(self):
        self.c_opts.cert = REVOKED_CLIENT1
        self.c_opts.cacert = CACERTBOTH
        self.c_opts.crl = CRLBOTH
        self.s_opts.cert = SERVER2
        self.s_opts.cacert = CACERTBOTH
        self.s_opts.crl = CRLBOTH
        self.init(None, None)
        self.stop()
        self.assertEquals(len(self.excpt), 1, "More than one side failed")
        self.assertEquals(type(self.excpt[0]), X509.X509Error,
                          "Both sides did not fail")
        self.assertEquals(self.excpt[0].args[1], m2.X509_V_ERR_CERT_REVOKED,
                          "Wrong type of failure")
        self.excpt = []

    def test_connectRevokedClientWithCRLBothCAs3(self):
        self.c_opts.cert = REVOKED_CLIENT2
        self.c_opts.cacert = CACERTBOTH
        self.c_opts.crl = CRLBOTH
        self.s_opts.cert = SERVER1
        self.s_opts.cacert = CACERTBOTH
        self.s_opts.crl = CRLBOTH
        self.init(None, None)
        self.stop()
        self.assertEquals(len(self.excpt), 1, "More than one side failed")
        self.assertEquals(type(self.excpt[0]), X509.X509Error,
                          "Both sides did not fail")
        self.assertEquals(self.excpt[0].args[1], m2.X509_V_ERR_CERT_REVOKED,
                          "Wrong type of failure")
        self.excpt = []

    def test_connectRevokedClientWithCRLBothCAs4(self):
        self.c_opts.cert = REVOKED_CLIENT2
        self.c_opts.cacert = CACERTBOTH
        self.c_opts.crl = CRLBOTH
        self.s_opts.cert = SERVER2
        self.s_opts.cacert = CACERTBOTH
        self.s_opts.crl = CRLBOTH
        self.init(None, None)
        self.stop()
        self.assertEquals(len(self.excpt), 1, "More than one side failed")
        self.assertEquals(type(self.excpt[0]), X509.X509Error,
                          "Both sides did not fail")
        self.assertEquals(self.excpt[0].args[1], m2.X509_V_ERR_CERT_REVOKED,
                          "Wrong type of failure")
        self.excpt = []

    ##########################################################################

    def test_connectRevokedServerWithCRLBothCAs(self):
        self.c_opts.cert = CLIENT1
        self.c_opts.cacert = CACERTBOTH
        self.c_opts.crl = CRLBOTH
        self.s_opts.cert = REVOKED_SERVER1
        self.s_opts.cacert = CACERTBOTH
        self.s_opts.crl = CRLBOTH
        self.init(None, None)
        self.stop()
        self.assertEquals(len(self.excpt), 1, "More than one side failed")
        self.assertEquals(type(self.excpt[0]), X509.X509Error,
                          "Both sides did not fail")
        self.assertEquals(self.excpt[0].args[1], m2.X509_V_ERR_CERT_REVOKED,
                          "Wrong type of failure")
        self.excpt = []

    def test_connectRevokedServerWithCRLBothCAs2(self):
        self.c_opts.cert = CLIENT2
        self.c_opts.cacert = CACERTBOTH
        self.c_opts.crl = CRLBOTH
        self.s_opts.cert = REVOKED_SERVER1
        self.s_opts.cacert = CACERTBOTH
        self.s_opts.crl = CRLBOTH
        self.init(None, None)
        self.stop()
        self.assertEquals(len(self.excpt), 1, "More than one side failed")
        self.assertEquals(type(self.excpt[0]), X509.X509Error,
                          "Both sides did not fail")
        self.assertEquals(self.excpt[0].args[1], m2.X509_V_ERR_CERT_REVOKED,
                          "Wrong type of failure")
        self.excpt = []

    def test_connectRevokedServerWithCRLBothCAs3(self):
        self.c_opts.cert = CLIENT1
        self.c_opts.cacert = CACERTBOTH
        self.c_opts.crl = CRLBOTH
        self.s_opts.cert = REVOKED_SERVER2
        self.s_opts.cacert = CACERTBOTH
        self.s_opts.crl = CRLBOTH
        self.init(None, None)
        self.stop()
        self.assertEquals(len(self.excpt), 1, "More than one side failed")
        self.assertEquals(type(self.excpt[0]), X509.X509Error,
                          "Both sides did not fail")
        self.assertEquals(self.excpt[0].args[1], m2.X509_V_ERR_CERT_REVOKED,
                          "Wrong type of failure")
        self.excpt = []

    def test_connectRevokedServerWithCRLBothCAs4(self):
        self.c_opts.cert = CLIENT2
        self.c_opts.cacert = CACERTBOTH
        self.c_opts.crl = CRLBOTH
        self.s_opts.cert = REVOKED_SERVER2
        self.s_opts.cacert = CACERTBOTH
        self.s_opts.crl = CRLBOTH
        self.init(None, None)
        self.stop()
        self.assertEquals(len(self.excpt), 1, "More than one side failed")
        self.assertEquals(type(self.excpt[0]), X509.X509Error,
                          "Both sides did not fail")
        self.assertEquals(self.excpt[0].args[1], m2.X509_V_ERR_CERT_REVOKED,
                          "Wrong type of failure")
        self.excpt = []

    ##########################################################################

    def test_connectRevokedClientWithOutCRL(self):
        self.c_opts.cert = REVOKED_CLIENT1
        self.c_opts.cacert = CACERT1
        self.c_opts.crl = None
        self.s_opts.cert = SERVER1
        self.s_opts.cacert = CACERT1
        self.s_opts.crl = None
        self._connect()

    def test_connectRevokedClientWithOutCRL2(self):
        self.c_opts.cert = REVOKED_CLIENT2
        self.c_opts.cacert = CACERT2
        self.c_opts.crl = None
        self.s_opts.cert = SERVER2
        self.s_opts.cacert = CACERT2
        self.s_opts.crl = None
        self._connect()

    ##########################################################################

    def test_connectRevokedServerWithOutCRL(self):
        self.c_opts.cert = CLIENT1
        self.c_opts.cacert = CACERT1
        self.c_opts.crl = None
        self.s_opts.cert = REVOKED_SERVER1
        self.s_opts.cacert = CACERT1
        self.s_opts.crl = None
        self._connect()
    
    def test_connectRevokedServerWithOutCRL2(self):
        self.c_opts.cert = CLIENT2
        self.c_opts.cacert = CACERT2
        self.c_opts.crl = None
        self.s_opts.cert = REVOKED_SERVER2
        self.s_opts.cacert = CACERT2
        self.s_opts.crl = None
        self._connect()
    
    ##########################################################################

    def test_connectWrongCAs(self):
        self.c_opts.cert = CLIENT1
        self.c_opts.cacert = CACERT1
        self.c_opts.crl = CRL1
        self.s_opts.cert = SERVER2
        self.s_opts.cacert = CACERT2
        self.s_opts.crl = CRL2
        self.init(None, None)
        self.stop()
        self.assertEquals(len(self.excpt), 2, "Both sides did not fail")
        self.assertEquals(type(self.excpt[0]), SSLError,
                          "Both sides did not fail")
        self.assertEquals(type(self.excpt[1]), SSLError,
                          "Both sides did not fail")
        self.excpt = []
        
    def test_connectWrongCAs2(self):
        self.c_opts.cert = CLIENT2
        self.c_opts.cacert = CACERT2
        self.c_opts.crl = CRL2
        self.s_opts.cert = SERVER1
        self.s_opts.cacert = CACERT1
        self.s_opts.crl = CRL1
        self.init(None, None)
        self.stop()
        self.assertEquals(len(self.excpt), 2, "Both sides did not fail")
        self.assertEquals(type(self.excpt[0]), SSLError,
                          "Both sides did not fail")
        self.assertEquals(type(self.excpt[1]), SSLError,
                          "Both sides did not fail")
        self.excpt = []
        
    ##########################################################################

    def test_connectWrongClientCert(self):
        self.c_opts.cert = CLIENT2
        self.c_opts.cacert = CACERT1
        self.c_opts.crl = CRL1
        self.s_opts.cert = SERVER1
        self.s_opts.cacert = CACERT1
        self.s_opts.crl = CRL1
        self.init(None, None)
        self.stop()
        self.assertEquals(len(self.excpt), 2, "Both sides did not fail")
        self.assertEquals(type(self.excpt[0]), SSLError,
                          "Both sides did not fail")
        self.assertEquals(type(self.excpt[1]), SSLError,
                          "Both sides did not fail")
        self.excpt = []
        
    def test_connectWrongClientCert(self):
        self.c_opts.cert = CLIENT1
        self.c_opts.cacert = CACERT2
        self.c_opts.crl = CRL2
        self.s_opts.cert = SERVER1
        self.s_opts.cacert = CACERT1
        self.s_opts.crl = CRL1
        self.init(None, None)
        self.stop()
        self.assertEquals(len(self.excpt), 2, "Both sides did not fail")
        self.assertEquals(type(self.excpt[0]), SSLError,
                          "Both sides did not fail")
        self.assertEquals(type(self.excpt[1]), SSLError,
                          "Both sides did not fail")
        self.excpt = []
        
    ##########################################################################

    def test_connectWrongServerCert(self):
        self.c_opts.cert = CLIENT1
        self.c_opts.cacert = CACERT1
        self.c_opts.crl = CRL1
        self.s_opts.cert = SERVER2
        self.s_opts.cacert = CACERT1
        self.s_opts.crl = CRL1
        self.init(None, None)
        self.stop()
        self.assertEquals(len(self.excpt), 2, "Both sides did not fail")
        self.assertEquals(type(self.excpt[0]), SSLError,
                          "Both sides did not fail")
        self.assertEquals(type(self.excpt[1]), SSLError,
                          "Both sides did not fail")
        self.excpt = []

    def test_connectWrongServerCert(self):
        self.c_opts.cert = CLIENT1
        self.c_opts.cacert = CACERT1
        self.c_opts.crl = CRL1
        self.s_opts.cert = SERVER1
        self.s_opts.cacert = CACERT2
        self.s_opts.crl = CRL2
        self.init(None, None)
        self.stop()
        self.assertEquals(len(self.excpt), 2, "Both sides did not fail")
        self.assertEquals(type(self.excpt[0]), SSLError,
                          "Both sides did not fail")
        self.assertEquals(type(self.excpt[1]), SSLError,
                          "Both sides did not fail")
        self.excpt = []

    ##########################################################################

    def test_connectBothCAs(self):
        self.c_opts.cert = CLIENT1
        self.c_opts.cacert = CACERTBOTH
        self.c_opts.crl = None
        self.s_opts.cert = SERVER1
        self.s_opts.cacert = CACERTBOTH
        self.s_opts.crl = None
        self._connect()
        
    def test_connectBothCAs2(self):
        self.c_opts.cert = CLIENT2
        self.c_opts.cacert = CACERTBOTH
        self.c_opts.crl = None
        self.s_opts.cert = SERVER1
        self.s_opts.cacert = CACERTBOTH
        self.s_opts.crl = None
        self._connect()
        
    def test_connectBothCAs3(self):
        self.c_opts.cert = CLIENT1
        self.c_opts.cacert = CACERTBOTH
        self.c_opts.crl = None
        self.s_opts.cert = SERVER2
        self.s_opts.cacert = CACERTBOTH
        self.s_opts.crl = None
        self._connect()
        
    def test_connectBothCAs4(self):
        self.c_opts.cert = CLIENT2
        self.c_opts.cacert = CACERTBOTH
        self.c_opts.crl = None
        self.s_opts.cert = SERVER2
        self.s_opts.cacert = CACERTBOTH
        self.s_opts.crl = None
        self._connect()

    ##########################################################################

    def test_connectExpiredClient(self):
        self.c_opts.cert = EXPIRED_CLIENT1
        self.c_opts.cacert = CACERT1
        self.c_opts.crl = None
        self.s_opts.cert = SERVER1
        self.s_opts.cacert = CACERT1
        self.s_opts.crl = None
        self.init(None, None)
        self.stop()
        self.assertEquals(len(self.excpt), 2, "Both sides did not fail")
        self.assertEquals(type(self.excpt[0]), SSLError,
                          "Both sides did not fail")
        self.assertEquals(type(self.excpt[1]), SSLError,
                          "Both sides did not fail")
        self.excpt = []

    def test_connectExpiredClient2(self):
        self.c_opts.cert = EXPIRED_CLIENT2
        self.c_opts.cacert = CACERT2
        self.c_opts.crl = None
        self.s_opts.cert = SERVER2
        self.s_opts.cacert = CACERT2
        self.s_opts.crl = None
        self.init(None, None)
        self.stop()
        self.assertEquals(len(self.excpt), 2, "Both sides did not fail")
        self.assertEquals(type(self.excpt[0]), SSLError,
                          "Both sides did not fail")
        self.assertEquals(type(self.excpt[1]), SSLError,
                          "Both sides did not fail")
        self.excpt = []

    ##########################################################################

    def test_connectExpiredClientBothCAs(self):
        self.c_opts.cert = EXPIRED_CLIENT1
        self.c_opts.cacert = CACERTBOTH
        self.c_opts.crl = None
        self.s_opts.cert = SERVER1
        self.s_opts.cacert = CACERTBOTH
        self.s_opts.crl = None
        self.init(None, None)
        self.stop()
        self.assertEquals(len(self.excpt), 2, "Both sides did not fail")
        self.assertEquals(type(self.excpt[0]), SSLError,
                          "Both sides did not fail")
        self.assertEquals(type(self.excpt[1]), SSLError,
                          "Both sides did not fail")
        self.excpt = []

    def test_connectExpiredClientBothCAs2(self):
        self.c_opts.cert = EXPIRED_CLIENT1
        self.c_opts.cacert = CACERTBOTH
        self.c_opts.crl = None
        self.s_opts.cert = SERVER2
        self.s_opts.cacert = CACERTBOTH
        self.s_opts.crl = None
        self.init(None, None)
        self.stop()
        self.assertEquals(len(self.excpt), 2, "Both sides did not fail")
        self.assertEquals(type(self.excpt[0]), SSLError,
                          "Both sides did not fail")
        self.assertEquals(type(self.excpt[1]), SSLError,
                          "Both sides did not fail")
        self.excpt = []

    def test_connectExpiredClientBothCAs3(self):
        self.c_opts.cert = EXPIRED_CLIENT2
        self.c_opts.cacert = CACERTBOTH
        self.c_opts.crl = None
        self.s_opts.cert = SERVER1
        self.s_opts.cacert = CACERTBOTH
        self.s_opts.crl = None
        self.init(None, None)
        self.stop()
        self.assertEquals(len(self.excpt), 2, "Both sides did not fail")
        self.assertEquals(type(self.excpt[0]), SSLError,
                          "Both sides did not fail")
        self.assertEquals(type(self.excpt[1]), SSLError,
                          "Both sides did not fail")
        self.excpt = []

    def test_connectExpiredClientBothCAs4(self):
        self.c_opts.cert = EXPIRED_CLIENT2
        self.c_opts.cacert = CACERTBOTH
        self.c_opts.crl = None
        self.s_opts.cert = SERVER2
        self.s_opts.cacert = CACERTBOTH
        self.s_opts.crl = None
        self.init(None, None)
        self.stop()
        self.assertEquals(len(self.excpt), 2, "Both sides did not fail")
        self.assertEquals(type(self.excpt[0]), SSLError,
                          "Both sides did not fail")
        self.assertEquals(type(self.excpt[1]), SSLError,
                          "Both sides did not fail")
        self.excpt = []

    ##########################################################################

    def test_connectExpiredServer(self):
        self.c_opts.cert = CLIENT1
        self.c_opts.cacert = CACERT1
        self.c_opts.crl = None
        self.s_opts.cert = EXPIRED_SERVER1
        self.s_opts.cacert = CACERT1
        self.s_opts.crl = None
        self.init(None, None)
        self.stop()
        self.assertEquals(len(self.excpt), 2, "Both sides did not fail")
        self.assertEquals(type(self.excpt[0]), SSLError,
                          "Both sides did not fail")
        self.assertEquals(type(self.excpt[1]), SSLError,
                          "Both sides did not fail")
        self.excpt = []

    def test_connectExpiredServer2(self):
        self.c_opts.cert = CLIENT2
        self.c_opts.cacert = CACERT2
        self.c_opts.crl = None
        self.s_opts.cert = EXPIRED_SERVER2
        self.s_opts.cacert = CACERT2
        self.s_opts.crl = None
        self.init(None, None)
        self.stop()
        self.assertEquals(len(self.excpt), 2, "Both sides did not fail")
        self.assertEquals(type(self.excpt[0]), SSLError,
                          "Both sides did not fail")
        self.assertEquals(type(self.excpt[1]), SSLError,
                          "Both sides did not fail")
        self.excpt = []

    ##########################################################################

    def test_connectExpiredServerBothCAs(self):
        self.c_opts.cert = CLIENT1
        self.c_opts.cacert = CACERTBOTH
        self.c_opts.crl = None
        self.s_opts.cert = EXPIRED_SERVER1
        self.s_opts.cacert = CACERTBOTH
        self.s_opts.crl = None
        self.init(None, None)
        self.stop()
        self.assertEquals(len(self.excpt), 2, "Both sides did not fail")
        self.assertEquals(type(self.excpt[0]), SSLError,
                          "Both sides did not fail")
        self.assertEquals(type(self.excpt[1]), SSLError,
                          "Both sides did not fail")
        self.excpt = []

    def test_connectExpiredServerBothCAs2(self):
        self.c_opts.cert = CLIENT1
        self.c_opts.cacert = CACERTBOTH
        self.c_opts.crl = None
        self.s_opts.cert = EXPIRED_SERVER2
        self.s_opts.cacert = CACERTBOTH
        self.s_opts.crl = None
        self.init(None, None)
        self.stop()
        self.assertEquals(len(self.excpt), 2, "Both sides did not fail")
        self.assertEquals(type(self.excpt[0]), SSLError,
                          "Both sides did not fail")
        self.assertEquals(type(self.excpt[1]), SSLError,
                          "Both sides did not fail")
        self.excpt = []

    def test_connectExpiredServerBothCAs3(self):
        self.c_opts.cert = CLIENT2
        self.c_opts.cacert = CACERTBOTH
        self.c_opts.crl = None
        self.s_opts.cert = EXPIRED_SERVER1
        self.s_opts.cacert = CACERTBOTH
        self.s_opts.crl = None
        self.init(None, None)
        self.stop()
        self.assertEquals(len(self.excpt), 2, "Both sides did not fail")
        self.assertEquals(type(self.excpt[0]), SSLError,
                          "Both sides did not fail")
        self.assertEquals(type(self.excpt[1]), SSLError,
                          "Both sides did not fail")
        self.excpt = []

    def test_connectExpiredServerBothCAs4(self):
        self.c_opts.cert = CLIENT2
        self.c_opts.cacert = CACERTBOTH
        self.c_opts.crl = None
        self.s_opts.cert = EXPIRED_SERVER2
        self.s_opts.cacert = CACERTBOTH
        self.s_opts.crl = None
        self.init(None, None)
        self.stop()
        self.assertEquals(len(self.excpt), 2, "Both sides did not fail")
        self.assertEquals(type(self.excpt[0]), SSLError,
                          "Both sides did not fail")
        self.assertEquals(type(self.excpt[1]), SSLError,
                          "Both sides did not fail")
        self.excpt = []
