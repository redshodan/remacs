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

from M2Crypto import SSL, m2

from remacs import log


# Re-cribbed from M2Crypto's SSL/cb.py
# Cribbed from OpenSSL's apps/s_cb.c.
def ssl_info_callback(where, ret, ssl_ptr):
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
            log.warn("FAILED: %s: %s" % (state, m2.ssl_get_state_v(ssl_ptr)))
        else:
            log.verb("%s: %s" % (state, m2.ssl_get_state_v(ssl_ptr)))
    elif (where & m2.SSL_CB_ALERT):
        if (where & m2.SSL_CB_READ):
            w = 'read'
        else:
            w = 'write'
        desc = m2.ssl_get_alert_desc_v(ret)
        msg = "ALERT: %s: %s: %s" % (w, m2.ssl_get_alert_type_v(ret), desc)
        if desc == "close notify":
            log.verb(msg)
        else:
            log.warn(msg)


def create_ctx(options):
    ctx = SSL.Context("tlsv1")
    ctx.set_verify(SSL.verify_peer | SSL.verify_fail_if_no_peer_cert, 10)
    ctx.set_allow_unknown_ca(False)
    ctx.set_cipher_list('ALL:!ADH:!LOW:!EXP:!MD5:@STRENGTH')
    ctx.load_verify_info(cafile=options.cacert, capath=None)
    ctx.load_cert_chain(options.cert)
    ctx.set_info_callback(ssl_info_callback)
    return ctx
