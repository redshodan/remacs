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


from remacs import log
from remacs.pipebuff import PipeBuff


class InAcker(object):
    def __init__(self):
        self.outpipe = None
        self.ack_window = 5
        self.ack_cur = 0
        self.pkt_count = 0

    def inPacket(self):
        self.pkt_count = self.pkt_count + 1
        log.debug("ACK: inPacket: %d", self.pkt_count)
        if self.pkt_count - self.ack_cur >= self.ack_window:
            log.debug("ACK: Sending ack for %d", self.pkt_count)
            self.outpipe.sendCmd(PipeBuff.CMD_ACK, self.pkt_count)
            self.ack_cur = self.pkt_count


class OutAcker(object):
    def __init__(self,):
        self.ack_window = 5
        self.ack_cur = 0
        self.pkt_count = 0
        self.buffer = []

    def outPacket(self, pkt):
        self.pkt_count = self.pkt_count + 1
        if self.pkt_count - self.ack_cur >= self.ack_window:
            log.debug("ACK: %d unacked packets", self.pkt_count - self.ack_cur)
        self.buffer.append((self.pkt_count, pkt))
        log.debug("ACK: outPacket: %d", self.pkt_count)

    def handleAck(self, acked):
        log.debug("ACK: Handling %d", acked)
        if self.ack_cur >= acked:
            log.debug("ACK: mismatch: ack_cur=%d acked=%d", self.ack_cur, acked)
            return
        log.debug("ACK: ack_cur=%d acked=%d", self.ack_cur, acked)
        for pkt in self.buffer:
            log.debug("ACK: before pkt: %d", pkt[0])
        for index in range(len(self.buffer)):
            if self.buffer[index][0] > acked:
                log.debug("ACK: break on %d", index)
                break
        self.buffer = self.buffer[index + 1:]
        for pkt in self.buffer:
            log.debug("ACK: after pkt: %d", pkt[0])
        self.ack_cur = acked
        log.debug("ACK: buffer has %d packets", len(self.buffer))

    def resume(self, acked):
        pass
