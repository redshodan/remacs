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


import sys, os, threading, pty
import gtk, gobject


log = None


class SysTray(threading.Thread):
    def __init__(self, client):
        threading.Thread.__init__(self)
        self.client = client
        self.icon = gtk.StatusIcon()
        self.icon.set_from_file(
            "/usr/share/icons/hicolor/scalable/apps/emacs23.svg")
        self.icon.connect('button_press_event', self.onBtnPress)
        self.icon.connect('activate', self.onLeftClick)
        self.icon.connect('popup-menu', self.onRightClick)
        self.buildMenu()

    def buildMenu(self):
        self.menu_sep = None
        self.menu = gtk.Menu()
        self.menu_sep = gtk.SeparatorMenuItem()
        self.menu.prepend(self.menu_sep)
        self.menu_sep.show()
        item = gtk.MenuItem("Quit")
        self.menu.append(item)
        item.connect("activate", self.onQuit)
        item.show()

    def onBtnPress(self, widget, event):
        self.menu_ev_btn = event.button
        self.menu_ev_time = event.time
        if event.button == 2:
            self.onMiddleClick(widget, event)

    def onLeftClick(self, widget):
        if self.msgPending():
            self.menu.popup(None, None, None, self.menu_ev_btn,
                            self.menu_ev_time, None)
        else:
            if self.ttywin.is_active():
                self.ttywin.iconify()
            else:
                self.raiseWindow()
        self.checkPending()

    def onMiddleClick(self, widget, event):
        pending = None
        for child in self.menu.get_children():
            if child == self.menu_sep:
                break
            elif child.get_sensitive() and hasattr(child, "remacs_id"):
                pending = child
                break
        if pending:
            self.client.invokeNotif(pending.remacs_id)
            pending.set_sensitive(False)
            if not self.ttywin.is_active():
                self.raiseWindow()
        self.checkPending()
    
    def onRightClick(self, widget, event_button, event_time):
        self.menu.popup(None, None, None, event_button, event_time, None)
        self.checkPending()

    def onQuit(self, widget):
        log("Menu quit")
        gtk.main_quit()
        self.client.quit()

    def onMenuMsg(self, widget):
        widget.set_sensitive(False)
        if hasattr(widget, "remacs_id"):
            self.client.invokeNotif(widget.remacs_id)
            if not self.ttywin.is_active():
                self.raiseWindow()
        self.checkPending()

    def raiseWindow(self):
        self.ttywin.unmap()
        self.ttywin.map()
        self.ttywin.present()
        
    def checkPending(self):
        if self.msgPending():
            self.icon.set_blinking(True)
            self.ttywin.set_urgency_hint(True)
        else:
            self.icon.set_blinking(False)
            self.ttywin.set_urgency_hint(False)
    
    def msgPending(self):
        for child in self.menu.get_children():
            if child == self.menu_sep:
                return False
            elif child.get_sensitive():
                return True
        return False

    def error(self, msg):
        self.message(None, "ERROR: " + msg)

    def notify(self, id, msg):
        self.message(id, msg)

    def message(self, id, msg):
        self.ttywin.set_urgency_hint(True)
        item = gtk.MenuItem(msg)
        item.remacs_id = id
        self.menu.prepend(item)
        item.connect("activate", self.onMenuMsg)
        item.show()
        self.icon.set_blinking(True)
    
    def startVTE(self):
        import ttywindow
        self.ttywin = ttywindow.TTYWindow(self)
        return self.ttywin.pair[1]

    def run(self):
        gtk.gdk.threads_init()
        gtk.main()
