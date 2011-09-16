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

import os, logging
from logging.handlers import RotatingFileHandler

VERBOSE = 15
FORMAT = "%(asctime)s - %(name)s - %(levelname)s: %(message)s"
UI_FORMAT = "%(message)s"
logger = None
handler = None


def debug(*args, **kwargs):
    logger.debug(*args, **kwargs)

def verb(*args, **kwargs):
    logger.log(VERBOSE, *args, **kwargs)

def info(*args, **kwargs):
    logger.info(*args, **kwargs)

def warn(*args, **kwargs):
    logger.warning(*args, **kwargs)

def error(*args, **kwargs):
    logger.error(*args, **kwargs)

def crit(*args, **kwargs):
    logger.critical(*args, **kwargs)

def exception(*args, **kwargs):
    if isinstance(args[0], Exception):
        e = args[0]
        args = list(args[1:])
        args[0] = args[0] + "\n" + str(e)
    logger.exception(*args, **kwargs)

def init(name):
    global logger, handler, ui, uifile

    logging.addLevelName(VERBOSE, "VERB")
    logging.addLevelName(logging.WARNING, "WARN")
    logging.addLevelName(logging.CRITICAL, "CRIT")

    # Main logger
    logger = logging.getLogger(name)
    logger.setLevel(VERBOSE)
    handler = RotatingFileHandler(name + ".log")
    formatter = logging.Formatter(FORMAT)
    handler.setFormatter(formatter)
    logger.addHandler(handler)

    info("*******************************************************************")
    info("Started " + name)
