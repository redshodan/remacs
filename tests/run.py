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


###
### Add test modules/packages here
###
test_names = ["basic", "emacs", "protocol"]


import os, sys
if os.path.islink(__file__):
    path = os.path.dirname(os.readlink(__file__))
else:
    path = os.path.dirname(__file__)
sys.path = [os.path.abspath(os.path.join(path, "../python"))] + sys.path
del path

import unittest2, types, re
from test import test_support
import utils

utils.init()

for help in ["-h", "--help"]:
    if help in sys.argv:
        print "usage: run.py <pattern>"
        print
        print "pattern - pkg1.pkg2.partial_func"
        print "          pkg1.pkg2/regex"
        print
        print "eg: basic.ssl"
        print "    basic.ssl.Both"
        print "    basic.ssl/.*Both.*"
        print "    basic/.*Both.*"
        sys.exit(0)

if len(sys.argv) > 1:
    test_names = sys.argv[1:]

ts = unittest2.TestSuite()
for name in test_names:
    func = None
    try:
        __import__(name)
    except ImportError:
        words = name.split("/")
        if len(words) == 2:
            match = True
            func = words[1]
            name = words[0]
        else:
            match = False
            words = name.split(".")
            func = words[-1]
            name = ".".join(words[:-1])
        __import__(name)
    if func:
        mod = sys.modules[name]
        for key, val in mod.__dict__.iteritems():
            if type(val) == types.TypeType:
                for key2, val2 in val.__dict__.iteritems():
                    if key2[0] != "_":
                        if match:
                            ret = re.match(func, key2)
                        else:
                            ret = re.search(func, key2)
                        if type(val2) == types.FunctionType and ret:
                            tc = val(key2)
                            ts.addTests([tc])
    else:
        ts.addTests(
            unittest2.defaultTestLoader.loadTestsFromModule(sys.modules[name]))

print "Running tests..."
print "----------------------------------------------------------------------"
print
test_support.run_unittest(ts)
