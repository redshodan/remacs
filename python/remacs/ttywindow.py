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


import os
import pty, gtk, vte

import remacs


class TTYWindow(gtk.Window):
    def __init__(self, systray):
        gtk.Window.__init__(self)
        self.systray = systray
        self.set_title("remacs")
        self.connect('destroy', self.onQuit)
        self.connect('window-state-event', self.onWinStateEv)
        self.tty = vte.Terminal()
        self.tty.set_cursor_blinks(True)
        self.tty.set_emulation("xterm")
        self.tty.connect('eof', self.onQuit)
        self.add(self.tty)
        self.show_all()
        self.pair = pty.openpty()
        self.tty.set_pty(self.pair[0])
        self.icon_file = os.path.join(remacs.home, "share/emacs23.png")
        self.set_icon(gtk.gdk.pixbuf_new_from_file(self.icon_file))
    
    def onQuit(self, obj):
        self.systray.onQuit(None)

    def onWinStateEv(self, widget, event):
        self.winState = event.new_window_state

    def isIconified(self):
        return ((self.winState & gtk.gdk.WINDOW_STATE_ICONIFIED) and
                not (self.winState & gtk.gdk.WINDOW_STATE_WITHDRAWN))
