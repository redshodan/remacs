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


from . import log


home = None


# Bleck
from xml.dom import minidom as dom
impl = dom.getDOMImplementation()
d = impl.createDocument(None, None, None)
XML_PREFIX = d.toxml()
XML_PREFIX_LEN = len(XML_PREFIX)
d.unlink()
del d
del impl


def toxml(elem):
    string = elem.toxml()
    if string.startswith(XML_PREFIX):
        return string[XML_PREFIX:]
    else:
        return string


def init(path):
    global home
    home = path
