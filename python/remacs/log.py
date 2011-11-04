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

import os, logging
from logging import DEBUG, INFO, WARNING, CRITICAL
from logging.handlers import RotatingFileHandler

VERB = 15
FORMAT = "%(asctime)s - %(name)s - %(levelname)s: %(message)s"
UI_FORMAT = "%(message)s"
logger = None
handler = None


def debug(*args, **kwargs):
    logger.debug(*args, **kwargs)

def verb(*args, **kwargs):
    logger.log(VERB, *args, **kwargs)

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
        if len(args) > 1:
            args = list(args[1:])
            args[0] = args[0] + "\n" + str(e)
        else:
            args = [str(e)]
    logger.exception(*args, **kwargs)

def setLevel(level):
    logger.setLevel(level)

def getLevel():
    return logger.getEffectiveLevel()

def init(name):
    global logger, handler, ui, uifile

    logging.addLevelName(VERB, "VERB")
    logging.addLevelName(WARNING, "WARN")
    logging.addLevelName(CRITICAL, "CRIT")

    # Main logger
    logger = logging.getLogger(name)
    logger.setLevel(VERB)
    handler = RotatingFileHandler(name + ".log")
    formatter = logging.Formatter(FORMAT)
    handler.setFormatter(formatter)
    logger.addHandler(handler)

    info("*******************************************************************")
    info("Started " + name)
