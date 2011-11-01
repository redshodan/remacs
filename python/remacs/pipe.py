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

import os, errno

from M2Crypto import m2
from M2Crypto.SSL import SSLError
from remacs import log
from remacs.pipebuff import PipeBuff


class PipeConnLost(Exception):
    pass


class Pipe(object):
    def __init__(self, ins, outs, cb, decoder, encoder, inacker, outacker):
        self.ifd = None
        self.ofd = None
        self.ins = ins
        self.outs = outs
        self.buff = PipeBuff(cb, decoder, encoder, inacker, outacker)
        self.reader = None

    def setPipes(self, ifd, ofd):
        self.ifd = ifd
        self.ifd_read = hasattr(self.ifd, "read")
        if self.ifd:
            self.ifd_fileno = self.ifd.fileno()
        else:
            self.ifd_fileno = -1
        self.ofd = ofd
        self.ofd_write = hasattr(self.ofd, "write")
        if self.ofd:
            self.ofd_fileno = self.ofd.fileno()
        else:
            self.ofd_fileno = -1

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
                log.debug("starting read(%d)" % self.ifd_fileno)
                if self.ifd_read:
                    data = self.ifd.read(1024)
                else:
                    data = self.ifd.recv(1024)
                if data and len(data):
                    if self.buff.data:
                        self.buff.data = self.buff.data + data
                    else:
                        self.buff.data = data
                if data:
                    log.debug("read(%d): %s %s" % (self.ifd_fileno, len(data),
                                                   data))
                else:
                    log.debug("read(%d): %s" % (self.ifd_fileno, data))
            except IOError, e:
                log.debug("read(%d): EXC: %s : %d" %
                          (self.ifd_fileno, str(e), e.errno))
                if e.errno == 11:
                    log.debug("read(%d): EXC: %s" %
                              (self.ifd_fileno, "temp unavail"))
                    return False
                elif e.errno == 9:
                    raise PipeConnLost("Connection lost")
                else:
                    raise
            except OSError, e:
                log.debug("read(%d): EXC: %s" % (self.ifd_fileno, str(e)))
                if e.errno == errno.EIO:
                    log.debug("read(%d): EXC: %s" % (self.ifd_fileno, "EIO"))
                    self.delList(self.ifd, self.ins)
                    return False
                elif e.errno == errno.EAGAIN:
                    log.debug("EAGAIN")
                    if not self.buff.output:
                        return False
                else:
                    log.debug("read(%d): EXC: %s:%d" %
                        (self.ifd_fileno, e.errno))
                    raise
            except SSLError, e:
                if e.args[0] == m2.ssl_error_want_read:
                    log.debug("ssl_read(%d): want_read" % self.ifd_fileno)
                    self.insList(self.ifd, self.ins)
                elif e.args[0] == m2.ssl_error_want_write:
                    log.debug("ssl_read(%d): want_write" % self.ifd_fileno)
                    self.insList(self.ofd, self.outs)
                elif e.args[0] != m2.ssl_error_none:
                    log.debug("ssl_read(%d): want_none" % self.ifd_fileno)
                    raise PipeConnLost("Connection lost")
                return False
            if (not data or not len(data)):
                raise PipeConnLost("Connection lost")
        if self.buff.data:
            self.buff.filterData()
        if self.ofd and self.buff.output:
            try:
                log.debug("write(%d) %s" % (self.ofd_fileno, self.buff.output))
                if self.ofd_write:
                    size = self.ofd.write(self.buff.output)
                else:
                    size = self.ofd.send(self.buff.output)
                # For python File objects
                if size is None:
                    size = len(self.buff.output)
                log.debug("write(%d) size %s" % (self.ofd_fileno, str(size)))
            except IOError, e:
                log.debug("read(%d): EXC: %s : %d" %
                          (self.ifd_fileno, str(e), e.errno))
                if e.errno == 11:
                    log.debug("read(%d): EXC: %s" %
                              (self.ifd_fileno, "temp unavail"))
                    return False
                else:
                    raise
            except OSError, e:
                log.debug("write(%d): EXC %s" % (self.ofd_fileno, str(e)))
                if e.errno == errno.EAGAIN:
                    log.debug("write(%s): EXC %s" % (str(self.ofd), "EAGAIN"))
                    self.insList(self.ofd, self.outs)
                    self.delList(self.ifd, self.ins)
                    return False
            except SSLError, e:
                if e.args[0] == m2.ssl_error_want_read:
                    log.debug("ssl_write(%d): want_read" % self.ofd_fileno)
                    self.insList(self.ifd, self.ins)
                elif e.args[0] == m2.ssl_error_want_write:
                    log.debug("ssl_write(%d): want_write" % self.ofd_fileno)
                    self.insList(self.ofd, self.outs)
                elif e.args[0] != m2.ssl_error_none:
                    raise PipeConnLost("Connection lost")
                else:
                    log.debug("ssl_write(%d): want_none" % self.ofd_fileno)
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
        if val is None:
            return
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
