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

from M2Crypto import X509, EVP, RSA, Rand, ASN1, m2, util, BIO


import remacs
from remacs import sslutil
from utils import RemacsTestCase


CLIENT1 = "../../tests/certs/client1.pem"
EXPIRED_CLIENT1 = "../../tests/certs/expired1.pem"
REVOKED_CLIENT1 = "../../tests/certs/revoked1.pem"
CACERT1 = "../../tests/certs/cacert1.pem"
CRL1 = "../../tests/certs/crl1.pem"

CLIENT2 = "../../tests/certs/client2.pem"
EXPIRED_CLIENT2 = "../../tests/certs/expired2.pem"
REVOKED_CLIENT2 = "../../tests/certs/revoked2.pem"
CACERT2 = "../../tests/certs/cacert2.pem"
CRL2 = "../../tests/certs/crl2.pem"

CACERTBOTH = "../../tests/certs/cacert-both.pem"


class SSLUtilTests(RemacsTestCase):
    def test_verifyCert(self):
        cacerts = sslutil.loadCertStack(CACERT1)
        client1 = X509.load_cert(CLIENT1)
        self.assertTrue(m2.verify_cert(cacerts.stack, client1.x509,
                                       X509.CRL().crl, 0),
                         "verified incorrectly")

    def test_verifyCAIdentity(self):
        cacerts = sslutil.loadCertStack(CACERT1)
        client1 = X509.load_cert(CACERT1)
        self.assertTrue(m2.verify_cert(cacerts.stack, client1.x509,
                                       X509.CRL().crl, 0), 
                        "verified incorrectly")

    def test_verifyCertWrongCA(self):
        cacerts = sslutil.loadCertStack(CACERT2)
        client1 = X509.load_cert(CLIENT1)
        self.assertRaises(X509.X509Error, m2.verify_cert, cacerts.stack,
                          client1.x509, X509.CRL().crl, 0)
        
    def test_verifyCertBothCAs1(self):
        cacerts = sslutil.loadCertStack(CACERTBOTH)
        client1 = X509.load_cert(CLIENT1)
        self.assertTrue(m2.verify_cert(cacerts.stack, client1.x509,
                                       X509.CRL().crl, 0),
                        "verified incorrectly")
        
    def test_verifyCertBothCAs2(self):
        cacerts = sslutil.loadCertStack(CACERTBOTH)
        client1 = X509.load_cert(CLIENT2)
        self.assertTrue(m2.verify_cert(cacerts.stack, client1.x509,
                                       X509.CRL().crl, 0),
                        "verified incorrectly")

    def test_verifyCertWithCRL(self):
        cacerts = sslutil.loadCertStack(CACERT1)
        client1 = X509.load_cert(CLIENT1)
        crl = X509.load_crl(CRL1)
        self.assertTrue(m2.verify_cert(cacerts.stack, client1.x509, crl.crl, 1),
                        "verified incorrectly")
    
    def test_verifyRevokedCertWithCRL(self):
        cacerts = sslutil.loadCertStack(CACERT1)
        client1 = X509.load_cert(REVOKED_CLIENT1)
        crl = X509.load_crl(CRL1)
        self.assertRaises(X509.X509Error, m2.verify_cert, cacerts.stack,
                          client1.x509, crl.crl, 1)

    def test_verifyRevokedCertWithoutCRL(self):
        cacerts = sslutil.loadCertStack(CACERT1)
        client1 = X509.load_cert(REVOKED_CLIENT1)
        self.assertTrue(m2.verify_cert(cacerts.stack, client1.x509,
                                       X509.CRL().crl, 0),
                        "verified incorrectly")
        
    def test_verifyExpiredCert(self):
        cacerts = sslutil.loadCertStack(CACERT1)
        client1 = X509.load_cert(EXPIRED_CLIENT1)
        self.assertRaises(X509.X509Error, m2.verify_cert, cacerts.stack,
                          client1.x509, X509.CRL().crl, 0)
