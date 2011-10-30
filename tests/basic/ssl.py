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
CLIENT2 = "../../tests/certs/client2.pem"
CACERT1 = "../../tests/certs/cacert1.pem"
CACERT2 = "../../tests/certs/cacert2.pem"
CACERTBOTH = "../../tests/certs/cacert-both.pem"

class SSLTests(RemacsTestCase):
    def test_verifyCert(self):
        cacert1 = X509.load_cert(CACERT1)
        client1 = X509.load_cert(CLIENT1)
        tstack = X509.X509_Stack()
        tstack.push(cacert1)
        ret = m2.verify_cert(tstack.stack, client1.x509)
        assert ret == 1, "verified incorrectly"

    def test_verifyCertWrongCA(self):
        cacert1 = X509.load_cert(CACERT2)
        client1 = X509.load_cert(CLIENT1)
        tstack = X509.X509_Stack()
        tstack.push(cacert1)
        ret = m2.verify_cert(tstack.stack, client1.x509)
        assert ret == 0, "verified incorrectly"
        
    def test_verifyCertBothCAs1(self):
        cacerts = sslutil.loadCertStack(CACERTBOTH)
        client1 = X509.load_cert(CLIENT1)
        ret = m2.verify_cert(cacerts.stack, client1.x509)
        assert ret == 1, "verified incorrectly"
        
    def test_verifyCertBothCAs2(self):
        cacerts = sslutil.loadCertStack(CACERTBOTH)
        client1 = X509.load_cert(CLIENT2)
        ret = m2.verify_cert(cacerts.stack, client1.x509)
        assert ret == 1, "verified incorrectly"
