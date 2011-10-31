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
        self.transport = "ssh"
        self.no_x = True
        self.id = "test1"


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
