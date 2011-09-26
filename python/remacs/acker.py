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


from remacs import log


class InAcker(object):
    def __init__(self):
        self.pbuff = None
        self.ack_window = 5
        self.ack_cur = 0
        self.pkt_count = 0

    def inPacket(self):
        self.pkt_count = self.pkt_count + 1
        if self.pkt_count - self.ack_cur >= self.ack_window:
            log.debug("ACK: Sending ack for %d", self.pkt_count)
            self.pbuff.encodeCmd(self.pbuff.CMD_ACK, self.pkt_count)
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

    def handleAck(self, acked):
        log.debug("ACK: Handling %d", acked)
        if self.ack_cur >= acked:
            log.debug("ACK: mismatch: ack_cur=%d acked=%d", self.ack_cur, acked)
            return
        delta = acked - self.ack_cur
        for index in range(delta):
            del self.buffer[index]
        self.ack_cur = acked
        log.debug("ACK: buffer has %d packets", delta)
