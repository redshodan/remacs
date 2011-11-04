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


import socket, struct

from remacs import log


class PipeBuff(object):
    # Command byte:
    #  - least significant 2 bits are command.
    #  - most significant 6 bits are size of data.
    #  - all most significant 6 bits set is size overflow, followed by 4 bytes
    #    of size
    CMD_NONE = -1
    CMD_ACK = 0
    CMD_TTY = 1
    CMD_CMD = 2
    CMD_BLOCK = 3
    CMD_MAX = 3
    CMD_BITS = 2
    CMD_SIZE_MAX = 62
    CMD_SIZE_MAXED = 252

    def __init__(self, cb, decoder, encoder, inacker, outacker):
        self.cb = cb
        self.all_cb = None
        self.decoder = decoder
        self.encoder = encoder
        self.data = None
        self.output = None
        self.length = -1
        self.cmd = self.CMD_NONE
        self.inacker = inacker
        self.outacker = outacker

    def toStr(self):
        if self.data:
            data = self.data
        else:
            data = ""
        return "cmd=%d len=%s data='%s' output='%s'" % \
            (self.cmd, str(len(data)), self.data, self.output)

    def filterData(self):
        if self.decoder:
            self.decodeCmd()
        else:
            if self.encoder:
                self.encodeCmd(self.CMD_TTY, self.data)
                self.data = None
                log.debug("encoded buff: %s" % self.output)
            else:
                log.debug("output buff: %s" % self.output)
                if self.output:
                    self.output = self.output + data
                else:
                    self.output = self.data
                self.data = None
                log.debug("output buff: %s" % self.output)

    def encodeCmd(self, cmd, data):
        isack = False
        log.debug("encodeCmd:%d %s" % (cmd, data))
        if data:
            if cmd == self.CMD_ACK:
                isack = True
                cmd = cmd + self.CMD_SIZE_MAXED
                log.debug("cmd: %d" % cmd)
                log.debug("e-nnl: %d" % data)
                log.debug("e-nl: %d" % socket.htonl(data))
                output = (struct.pack("B", cmd) +
                          struct.pack("I", socket.htonl(data)))
            else:
                length = len(data)
                log.debug("length=%d" % length)
                if length <= self.CMD_SIZE_MAX:
                    cmd = cmd + (length << self.CMD_BITS)
                    log.debug("cmd: %d" % cmd)
                    output = struct.pack("B", cmd) + data
                else:
                    cmd = cmd + self.CMD_SIZE_MAXED
                    log.debug("e-nnl: %d" % length)
                    log.debug("e-nl: %d" % socket.htonl(length))
                    output = (struct.pack("B", cmd) +
                              struct.pack("I", socket.htonl(length)) + str(data))
        else:
            output = struct.pack("B", cmd)
        if not isack:
            self.outacker.outPacket(output)
        if self.output:
            self.output = self.output + output
        else:
            self.output = output

    def decodeCmd(self):
        log.debug("Starting decodeCmd: data len: %d" % len(self.data))
        if self.cmd == self.CMD_NONE:
            self.cmd = struct.unpack("B", self.data[0])[0]
            self.data = self.data[1:]
            log.debug("unpacked cmd: %d" % self.cmd)
            log.debug("data len: %d" % len(self.data))
        if self.length == -1:
            if self.cmd & self.CMD_SIZE_MAXED == self.CMD_SIZE_MAXED:
                log.debug("MAXED")
                if len(self.data) < 4:
                    log.debug("dont have enough for length")
                    return False
                self.length = struct.unpack("I", self.data[:4])[0]
                self.length = socket.ntohl(self.length)
                self.data = self.data[4:]
            else:
                self.length = self.cmd >> self.CMD_BITS
            self.cmd = self.cmd & self.CMD_MAX
            log.debug("setting cmd to: %d" % self.cmd)
        cmd = self.cmd
        ret = False
        cmd_data = None
        if cmd == self.CMD_ACK:
            log.debug("ACK: got ack %d", self.length)
            acked = self.length
            self.length = -1
            self.cmd = self.CMD_NONE
            if len(self.data) > 0:
                ret = True
            self.outacker.handleAck(acked)
        elif len(self.data) == self.length:
            log.debug("data and length same size")
            cmd_data = self.data
            self.data = None
            self.cmd = self.CMD_NONE
            self.length = -1
        elif len(self.data) > self.length:
            log.debug("extra data: length=%d data len=%d" %
                (self.length, len(self.data)))
            cmd_data = self.data[:self.length]
            self.data = self.data[self.length:]
            self.cmd = self.CMD_NONE
            self.length = -1
            ret = True
        if cmd != self.CMD_ACK:
            self.inacker.inPacket()
        if self.all_cb:
            self.all_cb(cmd, cmd_data, self)
        if cmd_data:
            if cmd == self.CMD_TTY:
                if self.output:
                    self.output = self.output + cmd_data
                else:
                    self.output = cmd_data
            else:
                self.cb(cmd, cmd_data)
        if log.getLevel() == log.DEBUG:
            log.debug("decoded length: %d self.cmd: %d cmd: %d" %
                      (self.length, self.cmd, cmd))
            log.debug("decoded data: %s" % self.data)
            if self.data:
                log.debug("decoded data len: %d" % len(self.data))
            log.debug("decoded output: %s" % self.output)
            if self.output:
                log.debug("output len: %d" % len(self.output))
            log.debug("decoded cmd_data: %s" % cmd_data)
            if cmd_data:
                log.debug("cmd_data len: %d" % len(cmd_data))
                log.verb("decoded data: %s" % cmd_data)
        if ret:
            log.debug("Recursing decodeCmd")
            return self.decodeCmd()
        else:
            log.debug("Leaving decodeCmd")
            return ret

