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


import os, sys
if os.path.islink(__file__):
    path = os.path.dirname(os.readlink(__file__))
else:
    path = os.path.dirname(__file__)
sys.path = [os.path.abspath(os.path.join(path, "../python"))] + sys.path
del path

import unittest
from test import test_support
import utils

### Add test modules/packages here
import basic, protocol
test_modules = [basic, protocol]


utils.init()

print "Running tests..."
print "----------------------------------------------------------------------"
print
ts = unittest.TestSuite()
for module in test_modules:
    ts.addTests(unittest.defaultTestLoader.loadTestsFromModule(module))
test_support.run_unittest(ts)
