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

import os, sys, unittest2, time
from subprocess import Popen

import remacs
from remacs import log


## Cert paths
CLIENT1 = "../../tests/certs/client1.pem"
EXPIRED_CLIENT1 = "../../tests/certs/expired1.pem"
REVOKED_CLIENT1 = "../../tests/certs/revoked1.pem"
SERVER1 = "../../tests/certs/server1.pem"
EXPIRED_SERVER1 = "../../tests/certs/expiredserver1.pem"
REVOKED_SERVER1 = "../../tests/certs/revokedserver1.pem"
CACERT1 = "../../tests/certs/cacert1.pem"
CRL1 = ["../../tests/certs/crl1.pem"]

CLIENT2 = "../../tests/certs/client2.pem"
EXPIRED_CLIENT2 = "../../tests/certs/expired2.pem"
REVOKED_CLIENT2 = "../../tests/certs/revoked2.pem"
SERVER2 = "../../tests/certs/server2.pem"
EXPIRED_SERVER2 = "../../tests/certs/expiredserver2.pem"
REVOKED_SERVER2 = "../../tests/certs/revokedserver2.pem"
CACERT2 = "../../tests/certs/cacert2.pem"
CRL2 = ["../../tests/certs/crl2.pem"]

CACERTBOTH = "../../tests/certs/cacert-both.pem"
CRLBOTH = ["../../tests/certs/crl1.pem", "../../tests/certs/crl2.pem"]


##
## unittest behavior adjustment
##
class RemacsTestCase(unittest2.TestCase):
    def __init__(self, name):
        unittest2.TestCase.__init__(self, name)
        self.orgtest = getattr(self, name)
        setattr(self, name, self._run)

    def setUp(self):
        unittest2.TestCase.setUp(self)
        log.info("=========Starting test: %s=========", str(self))

    def tearDown(self):
        unittest2.TestCase.tearDown(self)
        log.info("=========Ending test: %s=========", str(self))

    def _run(self):
        try:
            self.orgtest()
        except KeyboardInterrupt:
            log.exception("Keyboard Interrupt")
            raise
        except:
            log.exception("Exception during test")
            raise

class DefaultOpts(object):
    def __init__(self):
        self.server = False
        self.ssl = False
        self.host = "127.0.0.1"
        self.sslport = "4748"
        self.cert = None
        self.cacert = None
        self.crl = None
        self.transport = "ssh"
        self.no_x = True
        self.id = "test1"

class ModuleRef(object):
    def __init__(self, obj=None):
        self.__setobj__(obj)

    def __setobj__(self, obj):
        self.__dict__["__obj__"] = obj

    def __getattr__(self, name):
        if not self.__obj__:
            raise AttributeError("name")
        return getattr(self.__obj__, name)

    def __setattr__(self, name, val):
        self.__obj__.__setattr__(name, val)

def startEmacs():
    log.info("Starting emacs")
    emacs_log = open("emacs.log", "w+")
    cmd = ["emacs", "--daemon", "-nw", "-q", "--eval",
           '(progn (setq server-name "remacs-emacs")'
           '(setq remacs-server-name "remacs-test")'
           '(add-to-list \'load-path "../../elisp")'
           '(find-file-noselect "../../elisp/remacs.el" t)'
           '(eval-buffer (get-buffer "remacs.el"))(remacs-test))']
    Popen(cmd, stdout=emacs_log, stderr=emacs_log)
    time.sleep(2)

def stopEmacs():
    log.info("Stopping emacs")
    cmd = ("emacsclient -s /tmp/emacs%d/remacs-emacs --eval "
           "\"(progn (setq kill-emacs-hook 'nil) (kill-emacs))\"") % os.getuid()
    os.system(cmd)

def init():
    log.init("test")
    log.logger.setLevel(log.DEBUG)
