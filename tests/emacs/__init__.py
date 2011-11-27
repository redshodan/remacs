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


import os, socket, time
from subprocess import Popen

from remacs import log
from utils import RemacsTestCase

class EmacsBaseTestCase(RemacsTestCase):
    @classmethod
    def setUpClass(cls):
        RemacsTestCase.setUpClass()
        EmacsBaseTestCase.startEmacs()

    @classmethod
    def tearDownClass(cls):
        EmacsBaseTestCase.stopEmacs()
        RemacsTestCase.tearDownClass()

    @staticmethod
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

    @staticmethod
    def stopEmacs():
        log.info("Stopping emacs")
        cmd = ("emacsclient -s /tmp/emacs%d/remacs-emacs --eval "
               "\"(progn (condition-case err "
               "           (with-current-buffer (get-buffer \\\"*remacs*\\\") "
               "              (write-file \\\"remacs.log\\\"))"
               "           (setq kill-emacs-hook 'nil)"
               "         (error))"
               "         (kill-emacs))\"") % os.getuid()
        log.info("stopEmacs: " + cmd)
        os.system(cmd)

    def setUp(self):
        RemacsTestCase.setUp(self)
        self.sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self.sock.connect("/tmp/remacs%d/remacs-test" % os.geteuid())
        log.info("connected to emacs server")

    def tearDown(self):
        self.sock.close()
        RemacsTestCase.tearDown(self)


class EmacsClientBaseTestCase(EmacsBaseTestCase):
    def setUp(self):
        EmacsBaseTestCase.setUp(self)
    
    def tearDown(self):
        EmacsBaseTestCase.tearDown(self)


from .basic import *
from .client import *
