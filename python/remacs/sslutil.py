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

import sys

from M2Crypto import SSL, m2, BIO, Err, X509
from M2Crypto.SSL import SSLError, Connection

from remacs import log


LOG = "SSL connection:"


class SSLUtil(object):

    def __init__(self, isserver, cacert, cert):
        self.loadContext(isserver, cacert, cert)
        self.cacerts = loadAllCerts(cacert)

    def makeSock(self):
        self.sock = Connection(self.ctx)
        self.sock.postConnectionCheck = self.postConnectionCheck

    def loadContext(self, isserver, cacert, cert):
        self.ctx = SSL.Context("tlsv1")
        self.ctx.set_verify(SSL.verify_peer | SSL.verify_fail_if_no_peer_cert,
                            10)
        self.ctx.set_allow_unknown_ca(False)
        self.ctx.set_cipher_list('ALL:!ADH:!LOW:!EXP:!MD5:@STRENGTH')
        self.ctx.load_verify_locations(cafile=cacert, capath=None)
        self.ctx.load_cert_chain(cert)
        self.ctx.set_info_callback(sslInfoCallback)
    
    def postConnectionCheck(self, peer_cert, peer_addr):
        log.info("%s checking: %s, %s",
                 LOG, peer_addr, peer_cert.get_subject())
        # Must have a cert
        if peer_cert is None:
            raise Exception("%s Peer did not return certificate" % LOG)
        # Check openssl's verification
        ret = self.sock.get_verify_result()
        if ret != m2.X509_V_OK:
            msg = ("% Peers certificate did not verify: %s - %s" %
                   (LOG, Err.get_error(), Err.get_error_reason(ret)))
            log.warn(msg)
            raise Exception(msg)
        # Compare the peers cacert to our cacerts, just to make sure
        stack = self.sock.get_peer_cert_chain()
        if stack:
            for ca in self.cacerts:
                cakey = ca.get_pubkey().get_rsa().pub()
                der = ca.as_der()
                for peer in stack:
                    if ((cakey == peer.get_pubkey().get_rsa().pub()) and
                        (der == peer.as_der())):
                        log.info("%s peer signed by: %s", LOG,
                                 ca.get_subject())
                        return True
        else:
            # This is sslserver case where there was no cacert given. Find a
            # cacert in our trusted list that matches this client cert
            for ca in self.cacerts:
                tstack = X509.X509_Stack()
                tstack.push(ca)
                ret = m2.verify_cert(tstack.stack, peer_cert.x509)
                log.verb("Verify result %d, against %s" %
                         (ret, ca.get_subject()))
                if ret == 1:
                    return True
        raise Exception("%s No trusted CA Cert found" % LOG)


# Re-cribbed from M2Crypto's SSL/cb.py
# Cribbed from OpenSSL's apps/s_cb.c.
def sslInfoCallback(where, ret, ssl_ptr):
    w = where & ~m2.SSL_ST_MASK
    if (w & m2.SSL_ST_CONNECT):
        state = "SSL connect"
    elif (w & m2.SSL_ST_ACCEPT):
        state = "SSL accept"
    else:
        state = "SSL state unknown"
        
    # if (where & m2.SSL_CB_LOOP):
    #     log.verb("LOOP: %s: %s" % (state, m2.ssl_get_state_v(ssl_ptr)))
    if (where & m2.SSL_CB_EXIT):
        if not ret:
            log.warn("%s FAILED: %s: %s" %
                     (LOG, state, m2.ssl_get_state_v(ssl_ptr)))
        else:
            log.verb("%s %s: %s" %
                     (LOG, state, m2.ssl_get_state_v(ssl_ptr)))
    elif (where & m2.SSL_CB_ALERT):
        if (where & m2.SSL_CB_READ):
            w = 'read'
        else:
            w = 'write'
        desc = m2.ssl_get_alert_desc_v(ret)
        msg = ("%s ALERT: %s: %s: %s" %
               (LOG, w, m2.ssl_get_alert_type_v(ret), desc))
        if desc == "close notify":
            log.verb(msg)
        else:
            log.warn(msg)

# Borrowed from M2Crypto.X509 and modified
def loadAllCerts(file, format=X509.FORMAT_PEM):
    cacerts = []
    bio = BIO.openfile(file)
    try:
        while True:
            if format == X509.FORMAT_PEM:
                cacerts.append(X509.load_cert_bio(bio))
            elif format == X509.FORMAT_DER:
                cptr = m2.d2i_x509(bio._ptr())
                if cptr is None:
                    raise X509.X509Error(Err.get_error())
                cacerts.append(X509.X509(cptr, _pyfree=1))
            else:
                raise ValueError(
                    "Unknown format. Must be either FORMAT_DER or FORMAT_PEM")
    except X509.X509Error, e:
        if not len(cacerts):
            raise
    if not len(cacerts):
        raise Exception("No cacert was found")
    return cacerts

def loadCertStack(file, format=X509.FORMAT_PEM):
    cacerts = loadAllCerts(file, format)
    stack = X509.X509_Stack()
    for cacert in cacerts:
        stack.push(cacert)
    return stack
