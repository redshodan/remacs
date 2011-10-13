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


import os, time, fcntl, subprocess, commands
from subprocess import Popen
import gobject, gtk, glib

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
        self.ignore = 0
        self.saver_on = False
        gobject.timeout_add(1000, self.check)
        ret = commands.getstatusoutput("which xscreensaver-command")
        if ret[0] == 0 and len(ret[1]):
            log.verb("Starting xsaver: " + ret[1])
            self.xsaver = Popen([ret[1], "-watch"], stdout=subprocess.PIPE)
            fcntl.fcntl(self.xsaver.stdout, fcntl.F_SETFL, os.O_NONBLOCK)
            glib.io_add_watch(self.xsaver.stdout, glib.IO_IN, self.onXSaver)

    def onXSaver(self, fd, state):
        try:
            line = fd.readline().rstrip("\n")
            log.verb("xsaver: %s", line)
            if line.startswith("UNBLANK"):
                self.saver_on = False
            elif line.startswith("LOCK"):
                self.saver_on = True
            return True
        except Exception, e:
            log.exception("onXSaver: " + str(e))
            return False

    # Batch up unidle's to once every idle_delay seconds
    def check(self):
        cur = idle.getIdleSec()
        now = time.time()
        # log.verb(
        #     "XIdler.check: idle=%d cur=%d now=%d ignore=%s pending=%s on=%s",
        #     self.idle, cur, now, self.ignore, self.pending, self.saver_on)
        if (self.saver_on or (self.ignore and self.ignore < now)):
            self.ignore = 0
            self.pending = False
            self.last = now
        elif self.ignore and self.ignore >= now:
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
        if not self.saver_on:
            log.verb("Sending unidle to X server")
            self.ignore = time.time() + 2
            idle.unIdle()
