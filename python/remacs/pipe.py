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

import os, errno

from M2Crypto import m2
from M2Crypto.SSL import SSLError
from remacs import log
from remacs.pipebuff import PipeBuff


class PipeConnLost(Exception):
    pass


class Pipe(object):
    def __init__(self, ins, outs, cb, decoder, encoder):
        self.ifd = None
        self.ofd = None
        self.ins = ins
        self.outs = outs
        self.buff = PipeBuff(cb, decoder, encoder)
        self.reader = None

    def setPipes(self, ifd, ofd):
        self.ifd = ifd
        self.ofd = ofd

    def sendCmd(self, cmd, data):
        self.buff.encodeCmd(cmd, data)
        self.insList(self.ofd, self.outs)
        self.delList(self.ifd, self.ins)

    def run(self, do_read):
        while self._run(do_read):
            pass
        
    def _run(self, do_read):
        log.debug("Pipe.run(%s)" % str(do_read))
        if do_read:
            data = None
            try:
                log.debug("starting read(%s)" % str(self.ifd))
                data = self.ifd.read(1024)
                if data and len(data):
                    if self.buff.data:
                        self.buff.data = self.buff.data + data
                    else:
                        self.buff.data = data
                if data:
                    log.debug("read(%s): %s %s" % (str(self.ifd), len(data),
                                                   data))
                else:
                    log.debug("read(%s): %s" % (str(self.ifd), data))
            except OSError, e:
                log.debug("read(%s): EXC: %s" % (str(self.ifd), str(e)))
                if e.errno == errno.EIO:
                    log.debug("read(%s): EXC: %s" % (str(self.ifd), "EIO"))
                    delList(self.ifd, self.ins)
                    return False
                elif e.errno == errno.EAGAIN:
                    # log.debug("EAGAIN")
                    if not self.buff.output:
                        return False
                else:
                    log.debug("read(%s): EXC: %s:%d" %
                        (str(self.ifd), e.errno))
                    raise
            except SSLError, e:
                if e.args[0] == m2.ssl_error_want_read:
                    log.debug("ssl_read(%s): want_read" % str(self.ifd))
                    self.insList(self.ifd, self.ins)
                elif e.args[0] == m2.ssl_error_want_write:
                    log.debug("ssl_read(%s): want_write" % str(self.ifd))
                    self.insList(self.ofd, self.outs)
                elif e.args[0] != m2.ssl_error_none:
                    log.debug("ssl_read(%s): want_none" % str(self.ifd))
                    raise PipeConnLost("Connection lost")
                return False
            if (not data or not len(data)):
                raise PipeConnLost("Connection lost")
        if self.buff.data:
            self.buff.filterData()
        if self.buff.output:
            try:
                log.debug("write(%s) %s" % (str(self.ofd), self.buff.output))
                size = self.ofd.write(self.buff.output)
                log.debug("write(%s) size %s" % (str(self.ofd), str(size)))
            except OSError, e:
                log.debug("write(%s): EXC %s" % (str(self.ofd), str(e)))
                if e.errno == errno.EAGAIN:
                    log.debug("write(%s): EXC %s" % (str(self.ofd), "EAGAIN"))
                    self.insList(self.ofd, self.outs)
                    self.delList(self.ifd, self.ins)
                    return False
            except SSLError, e:
                if e.args[0] == m2.ssl_error_want_read:
                    log.debug("ssl_write(%s): want_read" % str(self.ofd))
                    self.insList(self.ifd, self.ins)
                elif e.args[0] == m2.ssl_error_want_write:
                    log.debug("ssl_write(%s): want_write" % str(self.ofd))
                    self.insList(self.ofd, self.outs)
                elif e.args[0] != m2.ssl_error_none:
                    raise PipeConnLost("Connection lost")
                else:
                    log.debug("ssl_write(%s): want_none" % str(self.ofd))
                    self.insList(self.ifd, self.ins)
                    self.delList(self.ofd, self.outs)                    
                return False
            if (size is None or size <= 0):
                raise PipeConnLost("Connection lost on write")
            elif size > 0:
                if size != len(self.buff.output):
                    self.insList(self.ofd, self.outs)
                    self.delList(self.ifd, self.ins)
                    self.buff.output = self.buff.output[size:]
                    log.debug("write(%s): %s partial %s" % (str(self.ofd),
                                                            str(size),
                                                            self.buff.output))
                    return False
                else:
                    log.debug("write(%s): reset" % str(self.ofd))
                    self.buff.output = None
                    self.insList(self.ifd, self.ins)
                    self.delList(self.ofd, self.outs)

        if do_read:
            return True
        else:
            return False

    # Non-exception throwing list managment functions. Sometimes python takes
    # exceptions too far.
    def insList(self, val, list):
        for index in range(len(list)):
            if list[index] == val:
                return
        else:
            list.append(val)

    def delList(self, val, list):
        for index in range(len(list)):
            if list[index] == val:
                del list[index]
                return
