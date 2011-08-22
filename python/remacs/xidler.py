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


import time
import gobject

import remacs
from remacs import log, idle


class XIdler(object):
    def __init__(self, tray, client):
        self.tray = tray
        self.client = client
        self.idle_delay = 20

    def start(self):
        idle.init()
        self.last = time.time()
        self.idle = 0
        self.pending = True
        self.ignore = False
        gobject.timeout_add(1000, self.check)

    # Batch up unidle's to once every idle_delay seconds
    def check(self):
        cur = idle.getIdleSec()
        now = time.time()
        # log.verb("XIdler.check: idle=%d cur=%d now=%d", self.idle, cur, now)
        if self.ignore:
            self.ignore = False
            self.pending = False
            self.last = now
        else:
            fire = False
            if cur == 0 or cur < self.idle:
                if self.last + self.idle_delay < now:
                    fire = True
                else:
                    self.pending = True
            elif self.pending and self.last + self.idle_delay < now:
                fire = True
            if fire:
                self.client.sendUnidle()
                self.pending = False
                self.last = now
        self.idle = cur
        gobject.timeout_add(1000, self.check)

    def unidle(self):
        self.ignore = True
        idle.unIdle()
