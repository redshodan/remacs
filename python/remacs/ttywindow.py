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


import pty
import gtk
import vte


class TTYWindow(gtk.Window):
    def __init__(self, systray):
        gtk.Window.__init__(self)
        self.systray = systray
        self.set_title("remacs")
        self.connect('destroy', self.onQuit)
        self.tty = vte.Terminal()
        self.tty.set_cursor_blinks(True)
        self.tty.set_emulation("xterm")
        self.tty.connect('eof', self.onQuit)
        self.add(self.tty)
        self.show_all()
        self.pair = pty.openpty()
        self.tty.set_pty(self.pair[0])
        self.icon_file = "/usr/share/icons/hicolor/48x48/apps/emacs23.png"
        self.set_icon(gtk.gdk.pixbuf_new_from_file(self.icon_file))
    
    def onQuit(self, obj):
        self.systray.onQuit(None)
