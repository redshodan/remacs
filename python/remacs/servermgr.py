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


class ServerMgr(object):
    def __init__(self):
        self.servers = {}

    def setServer(self, name, server):
        self.quitServer(name)
        self.servers[name] = server

    def getServer(self, name):
        if name in self.servers:
            return self.servers[name]
        else:
            return None

    def quitServer(self, name):
        if name in self.servers:
            self.servers[name].quit()

    def shutdown(self):
        for server in self.servers.values():
            server.quit()
    
    def reset(self, server):
        server.reset()
        self.setServer(server.name, server)

    def resume(self, server, acked):
        old = self.getServer(server.name)
        if not old:
            raise Exception("Resume on unknown session: " + server.name)
        server.resume(old, acked)
        self.setServer(server.name, server)

