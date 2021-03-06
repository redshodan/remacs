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

import types

from remacs import log, dom, toxml
from remacs.pipebuff import PipeBuff
from remacs.protocolbase import ProtocolBase

class ClientListener(object):
    def start(self):
        pass

    def stop(self):
        pass

    def setupTTY(self):
        pass
    
    def error(self, error):
        pass

    def notify(self, id, msg):
        pass

    def clearNotify(self, id):
        pass

    def suspend(self):
        pass

    def unidle(self):
        pass
    

class ProtocolClient(ProtocolBase):
    def __init__(self, options):
        super(ProtocolClient, self).__init__(options)
        self.first_setup = True
        self.resuming = False
        self.emacs_suspended = False
        self.listener = None

    def sendSetup(self):
        tty = ("<tty term='%s' row='%d' col='%d'/>" %
               (self.options.term, self.options.row, self.options.col))
        if self.first_setup:
            action = "action='reset'"
            self.first_setup = False
        elif self.resuming:
            action = "action='resume'"
            self.resuming = False
        else:
            action = None
        if action:
            session = ("<session name='%s' acked='%d' %s/>" %
                       (self.options.id, self.mgr.getAcked(), action))
        else:
            session = ""
        self.sendCmd("<query from='%s' type='set'><setup>%s%s</setup></query>" %
                     (self.options.id, tty, session))

    def quit(self):
        log.info("ProtocolClient.quit")
        self.mgr.quit()

    def cmdCB(self, cmd, data):
        log.verb("CLIENT CMD: %d DATA: %s" % (cmd, data))
        if cmd == PipeBuff.CMD_CMD:
            d = dom.parseString(data)
            elem = d.firstChild
            if elem.nodeName == "query":
                self.handleQuery(cmd, data, elem)
            else:
                log.error("Unkown command: " + data)
            d.unlink()

    def handleQuery(self, cmd, data, elem):
        if elem.getAttribute("type") == "error":
            log.verb("Dropping errored query: %s" % data)
            return
        resp = "result"
        child = elem.firstChild
        if child.nodeName == "error":
            error = child.firstChild.data
            log.verbose("Got error: %s" % error)
            self.listener.error(error)
        elif child.nodeName == "notify":
            if elem.getAttribute("type") == "set":
                id = child.getAttribute("id")
                title = child.firstChild.firstChild.data
                body = child.firstChild.nextSibling.firstChild.data
                msg = title + " : " + body
                log.verb("Got notification: id=%s msg=%s" % (id, msg))
                self.listener.notify(id, msg)
            elif elem.getAttribute("type") == "result":
                id = child.getAttribute("id")
                log.verb("Clearing notification: %s" % id)
                self.listener.clearNotify(id)
        elif child.nodeName == "suspend":
            log.verb("Suspending")
            self.emacs_suspended = True
            self.listener.suspend()
        elif child.nodeName == "unidle":
            log.verb("Unidling")
            self.listener.unidle()
        else:
            log.error("Unkown command: " + data)
            resp = "error"
        
    def invokeNotif(self, id):
        log.info("Sending notification invoked: %s" % id)
        self.sendCmd(("<query><notify id='%s' type='result'><invoke/></notify>" +
                      "</query>") % id)

    def readNotif(self, id):
        log.info("Sending notification read: %s" % id)
        self.sendCmd("<query><notify id='%s' type='result'/></query>" % id)

    def resumeEmacs(self):
        if self.emacs_suspended:
            self.emacs_suspended = False
            self.sendCmd("<query><resume/></query>")

    def sendUnidle(self):
        self.sendCmd("<query type='single'><unidle/></query>")
