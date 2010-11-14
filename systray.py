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

import sys, os, threading
import gtk, gobject

class SysTray(threading.Thread):
    def __init__(self):
        threading.Thread.__init__(self)
        self.icon = gtk.StatusIcon()
        self.icon.set_from_file("/usr/share/icons/hicolor/scalable/apps/emacs23.svg")
        self.icon.connect('activate', self.onLeftClick)
        self.icon.connect('popup-menu', self.onRightClick)
        self.buildMenu()

    def buildMenu(self):
        self.menu = gtk.Menu()
        item = gtk.MenuItem("Quit")
        self.menu.append(item)
        item.connect("activate", self.onQuit)
        item.show()

    def onLeftClick(self, widget):
        print "left click"

    def onRightClick(self, widget, event_button, event_time):
        print "right click"
        self.menu.popup(None, None, None, event_button, event_time, None)

    def onQuit(self, widget):
        print "quit"
        sys.exit(0)

    def run(self):
        gtk.gdk.threads_init()
        gtk.main()


if __name__ == '__main__':
    tray = SysTray()
    gtk.main()
