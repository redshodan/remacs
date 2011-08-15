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


import socket, struct

from remacs import log


class PipeBuff(object):
    # Command byte:
    #  - least significant 3 bits are command.
    #  - middle 4 bits are size of data.
    #  - most significant bit indicates size is number of size bytes.
    CMD_NONE = -1
    CMD_TTY = 0
    CMD_CMD = 1
    CMD_BLOCK = 2
    CMD_MAX = 7
    CMD_CMDS = 7
    CMD_SIZE_MAX = 15
    CMD_SIZE_MAXED = 128

    def __init__(self, cb, decoder, encoder):
        self.cb = cb
        self.decoder = decoder
        self.encoder = encoder
        self.data = None
        self.output = None
        self.length = -1
        self.cmd = self.CMD_NONE

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
                log("encoded buff: %s" % self.output)
            else:
                log("output buff: %s" % self.output)
                if self.output:
                    self.output = self.output + data
                else:
                    self.output = self.data
                self.data = None
                log("output buff: %s" % self.output)

    def encodeCmd(self, cmd, data):
        log("encodeCmd:%d %s" % (cmd, data))
        if data:
            length = len(data)
            log("length=%d" % length)
            if length <= self.CMD_SIZE_MAX:
                cmd = cmd + (length << 3)
                log("cmd: %d" % cmd)
                output = struct.pack("B", cmd) + data
            else:
                cmd = cmd + self.CMD_SIZE_MAXED
                log("e-nnl: %d" % length)
                log("e-nl: %d" % socket.htonl(length))
                output = (struct.pack("B", cmd) +
                          struct.pack("I", socket.htonl(length)) + str(data))
        else:
            output = struct.pack("B", cmd)
        if self.output:
            self.output = self.output + output
        else:
            self.output = output

    def decodeCmd(self):
        log("Starting decodeCmd: data len: %d" % len(self.data))
        if self.cmd == self.CMD_NONE:
            self.cmd = struct.unpack("B", self.data[0])[0]
            self.data = self.data[1:]
            log("unpacked cmd: %d" % self.cmd)
            log("data len: %d" % len(self.data))
        if self.length == -1:
            if self.cmd & self.CMD_SIZE_MAXED:
                if len(self.data) < 4:
                    log("dont have enough for length")
                    return False
                self.length = struct.unpack("I", self.data[:4])[0]
                self.length = socket.ntohl(self.length)
                self.data = self.data[4:]
            else:
                self.length = self.cmd >> 3
            self.cmd = self.cmd & self.CMD_CMDS
            log("setting cmd to: %d" % self.cmd)
        cmd = self.cmd
        ret = False
        cmd_data = None
        if len(self.data) == self.length:
            log("data and length same size")
            cmd_data = self.data
            self.data = None
            self.cmd = self.CMD_NONE
            self.length = -1
        elif len(self.data) > self.length:
            log("extra data: length=%d data len=%d" %
                (self.length, len(self.data)))
            cmd_data = self.data[:self.length]
            self.data = self.data[self.length:]
            self.cmd = self.CMD_NONE
            self.length = -1
            ret = True
        if (cmd_data and (cmd != self.CMD_TTY)):
            cmd_data = self.cb(cmd, cmd_data)
        if cmd_data:
            if self.output:
                self.output = self.output + cmd_data
            else:
                self.output = cmd_data
        log("decoded length: %d self.cmd: %d cmd: %d" %
            (self.length, self.cmd, cmd))
        log("decoded data: %s" % self.data)
        if self.data:
            log("decoded data len: %d" % len(self.data))
        log("decoded output: %s" % self.output)
        if self.output:
            log("output len: %d" % len(self.output))
        log("decoded cmd_data: %s" % cmd_data)
        if cmd_data:
            log("cmd_data len: %d" % len(cmd_data))
        if ret:
            log("Recursing decodeCmd")
            return self.decodeCmd()
        else:
            log("Leaving decodeCmd")
            return ret

