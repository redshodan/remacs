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

    def __init__(self, isserver, cacert, cert, crls=None):
        self.isserver = isserver
        self.crls = []
        if crls:
            for crl in crls:
                self.crls.append(X509.load_crl(crl))
            self.use_crl = 1
        else:
            self.crls.append(X509.CRL())
            self.use_crl = 0
        self.loadContext(cacert, cert)
        self.cacerts = loadAllCerts(cacert)

    def makeSock(self, sock=None):
        self.sock = Connection(self.ctx, sock=sock)
        self.sock.postConnectionCheck = self.postConnectionCheck

    def loadContext(self, cacert, cert):
        self.ctx = SSL.Context("tlsv1")
        self.ctx.set_verify(SSL.verify_peer | SSL.verify_fail_if_no_peer_cert,
                            10)
        self.ctx.set_allow_unknown_ca(False)
        self.ctx.set_cipher_list('ALL:!ADH:!LOW:!EXP:!MD5:@STRENGTH')
        self.ctx.load_verify_locations(cafile=cacert, capath=None)
        if self.isserver:
            # Advertise the CAs we'll accept in server mode
            self.ctx.set_client_CA_list_from_file(cacert)
        self.ctx.load_cert_chain(cert)
        self.ctx.set_info_callback(sslInfoCallback)
    
    def postConnectionCheck(self, peer_cert, peer_addr):
        log.info("%s checking: %s, %s",
                 LOG, peer_addr, peer_cert.get_subject())
        # Must have a cert
        if peer_cert is None:
            raise X509.X509Error("%s Peer did not return certificate" % LOG)
        # Check openssl's verification
        ret = self.sock.get_verify_result()
        if ret != m2.X509_V_OK:
            msg = ("% Peers certificate did not verify: %s - %s" %
                   (LOG, Err.get_error(), Err.get_error_reason(ret)))
            log.warn(msg)
            raise X509.X509Error(msg)
        # Verify the peers cert against our explicit cacerts and perform a CRL
        # check, just to make sure. The intent is to only trust our configured
        # certs, never anything signed by a 'real' CA since we are not doing
        # any kind of DNS assertions. This is trusting that the ssl stack has
        # enforced that the peers cert chain is being used correctly.
        tstack = X509.X509_Stack()
        for ca in self.cacerts:
            tstack.push(ca)
        # FIXME: implement STACK_OF(X509_CRL) class so don't have to loop this.
        ret = False
        for crl in self.crls:
            try:
                ret = m2.verify_cert(tstack.stack, peer_cert.x509,
                                     crl.crl, self.use_crl)
            except X509.X509Error, e:
                # Don't stop looking if this CRL did not match the peers cert
                if e.args[1] != m2.X509_V_ERR_UNABLE_TO_GET_CRL:
                    raise
            log.verb("Verify result %s, %s against %s" %
                     (ret, peer_cert.get_subject(), ca.get_subject()))
        if ret:
            return True
        raise X509.X509Error("%s No trusted CA Cert found" % LOG)


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
        raise X509.X509Error("No cacert was found")
    return cacerts

def loadCertStack(file, format=X509.FORMAT_PEM):
    cacerts = loadAllCerts(file, format)
    stack = X509.X509_Stack()
    for cacert in cacerts:
        stack.push(cacert)
    return stack
