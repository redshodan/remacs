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

import os, select, termios, fcntl

from remacs import log
from remacs.pipe import Pipe, PipeConnLost


class TTYManager(object):
    def __init__(self, fdin, fdout, tty, cmd_cb, extra_fds=None,
                 extra_fds_cb=None):
        self.running = True
        self.fdin = fdin
        self.fdout = fdout
        self.tty = tty
        self.ins = []
        self.outs = []
        self.cmd_cb = cmd_cb
        if extra_fds:
            self.extra_fds = extra_fds
        else:
            self.extra_fds = []
        self.extra_fds_cb = extra_fds_cb
        self.inpipe = Pipe(self.ins, self.outs, self.cmd_cb, True, False)
        self.outpipe = Pipe(self.ins, self.outs, self.cmd_cb, False, True)
        self.setup()

    def close(self):
        try:
            self.fdin.close()
        except:
            pass
        try:
            self.fdout.close()
        except:
            pass
        try:
            termios.tcsetattr(self.tty, termios.TCSANOW, self.orig_tty)
            self.tty.close()
        except:
            pass
        for extra in self.extra_fds:
            try:
                extra.close()
            except:
                pass

    def setup(self):
        self.orig_tty = termios.tcgetattr(self.tty)
        new = termios.tcgetattr(self.tty)
        new[0] = new[0] | termios.IGNPAR
        new[0] = new[0] & ~(termios.ISTRIP|termios.INLCR|termios.IGNCR|
                            termios.ICRNL|termios.IXON|termios.IXANY|
                            termios.IXOFF)
        new[3] = new[3] & ~(termios.ICANON|termios.ISIG|termios.ECHO|
                            termios.ECHOE|termios.ECHOK|
                            termios.IEXTEN|termios.ECHONL)
        new[1] = new[1] & ~termios.OPOST
        termios.tcsetattr(self.tty, termios.TCSANOW, new)

        log.debug("fds: fdin=%s fdout=%s tty=%s" %
            (self.fdin, self.fdout, self.tty))
        
        fcntl.fcntl(self.tty, fcntl.F_SETFL, os.O_NONBLOCK)
        if hasattr(self.fdin, "setblocking"):
            self.fdin.setblocking(0)
        fcntl.fcntl(self.fdin, fcntl.F_SETFL, os.O_NONBLOCK)
        if hasattr(self.fdout, "setblocking"):
            self.fdout.setblocking(0)
        fcntl.fcntl(self.fdout, fcntl.F_SETFL, os.O_NONBLOCK)
        for fd in self.extra_fds:
            fcntl.fcntl(fd, fcntl.F_SETFL, os.O_NONBLOCK)

        self.ins.extend([self.fdin, self.tty])
        self.ins.extend(self.extra_fds)
        self.inpipe.setPipes(self.fdin, self.tty)
        self.outpipe.setPipes(self.tty, self.fdout)

    def run(self):
        log.debug("Starting TTYManager.run()")
        try:
            while self.running and (len(self.ins) or len(self.outs)):
                if ((len(self.ins) == 1) and (self.ins[0] == self.fdin) and
                    not len(self.outs)):
                    log("breaking out of TTYManager.run")
                    break
                try:
                    ret = select.select(self.ins, self.outs, [], 1.0)
                    # log.debug("select returned: %s" % (str(ret)))
                except select.error, e:
                    if e[0] == 4:
                        log.debug("select err 4")
                        continue
                try:
                    # self.logInOuts()
                    for fd in self.extra_fds:
                        if fd in ret[0]:
                            self.extra_fds_cb(fd)
                    if self.fdin in ret[0]:
                        self.inpipe.run(True)
                    if self.tty in ret[1]:
                        self.inpipe.run(False)
                    if self.tty in ret[0]:
                        self.outpipe.run(True)
                    if self.fdout in ret[1]:
                        self.outpipe.run(False)
                except PipeConnLost, e:
                    log.info("Connection lost")
                    self.close()
                    return
                except Exception, e:
                    log.exception("I/O exception: %s %s" % (type(e), str(e)))
                    import traceback
                    log.debug(traceback.format_exc())
                    self.close()
                    return
        except Exception, e:
            log.exception("Main loop exception: %s %s" % (type(e), str(e)))
            import traceback
            traceback.print_exc()
            raise

    def quit(self):
        self.running = False
        self.close()

    def logInOuts(self):
        msg = ""
        for fd in self.ins:
            if len(msg):
                msg = "%s, %s" % (msg, str(fd))
            else:
                msg = "%s" % str(fd)
        log.debug("ins: %s" % msg)
        msg = ""
        for fd in self.outs:
            if len(msg):
                msg = "%s, %s" % (msg, str(fd))
            else:
                msg = "%s" % str(fd)
        log.debug("outs: %s" % msg)
        log.debug("IN: %s" % self.inpipe.buff.toStr())
        log.debug("OUT: %s" % self.outpipe.buff.toStr())

    def sendCmd(self, cmd, data):
        self.outpipe.sendCmd(cmd, data)

