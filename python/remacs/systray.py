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


import sys, os, threading, pty
import gtk, glib

import remacs
from remacs import log, xidler


class SysTray(threading.Thread):
    def __init__(self, client):
        threading.Thread.__init__(self)
        self.client = client

    def stop(self):
        def doquit():
            for a in range(gtk.main_level() + 1):
                gtk.main_quit()
        glib.idle_add(doquit)

    def run(self):
        self.icon_file = os.path.join(remacs.home, "share/emacs23.svg")
        self.icon = gtk.StatusIcon()
        self.icon.set_from_file(self.icon_file)
        self.icon.connect('button_press_event', self.onBtnPress)
        self.icon.connect('activate', self.onLeftClick)
        self.icon.connect('popup-menu', self.onRightClick)
        self.buildMenu()
        self.xidler = xidler.XIdler(self, self.client)
        self.xidler.start()
        gtk.gdk.threads_init()
        gtk.main()

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
        self.menu_ev_location = (event.x_root, event.y_root)
        if event.button == 2:
            self.onMiddleClick(widget, event)

    def onLeftClick(self, widget):
        if self.msgPending():
            self.menu.popup(None, None, None, self.menu_ev_btn,
                            self.menu_ev_time, None)
        else:
            if self.ttywin.isIconified():
                self.raiseWindow()
            else:
                self.ttywin.iconify()
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
            if self.ttywin.isIconified():
                self.raiseWindow()
            if child.remacs_id:
                if event.get_state() & gtk.gdk.SHIFT_MASK:
                    self.client.readNotif(pending.remacs_id)
                else:
                    self.client.invokeNotif(pending.remacs_id)
            else:
                self.showErrorDialog(pending.get_label())
            pending.set_sensitive(False)
        self.checkPending()
    
    def onRightClick(self, widget, event_button, event_time):
        self.menu.popup(None, None, None, event_button, event_time, None)
        self.checkPending()

    def onQuit(self, widget):
        log.info("Menu quit")
        gtk.main_quit()
        self.client.quit()

    def onMenuMsg(self, widget):
        widget.set_sensitive(False)
        if widget.remacs_id:
            self.client.invokeNotif(widget.remacs_id)
            if self.ttywin.isIconified():
                self.raiseWindow()
        self.checkPending()

    def raiseWindow(self):
        self.ttywin.unmap()
        self.ttywin.map()
        self.ttywin.present()
        self.client.resumeEmacs()

    def iconify(self):
        self.ttywin.iconify()
        
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

    def clearNotify(self, id):
        widget = None
        for child in self.menu.get_children():
            if hasattr(child, "remacs_id") and child.remacs_id == id:
                widget = child
                break
        if widget:
            widget.set_sensitive(False)
        self.checkPending()
        
    def showErrorDialog(self, msg):
        dialog = gtk.MessageDialog(buttons=gtk.BUTTONS_OK)
        dialog.set_markup(msg)
        dialog.run()
        dialog.destroy()

    def startVTE(self):
        import ttywindow
        self.ttywin = ttywindow.TTYWindow(self)
        return self.ttywin.pair[1]
