#*******************************************************************************
#   Copyright (C) 2013 Kenneth L. Ho
#
#   This program is free software: you can redistribute it and/or modify it
#   under the terms of the GNU General Public License as published by the Free
#   Software Foundation, either version 3 of the License, or (at your option)
#   any later version.
#
#   This program is distributed in the hope that it will be useful, but WITHOUT
#   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
#   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
#   more details.
#
#   You should have received a copy of the GNU General Public License along with
#   this program.  If not, see <http://www.gnu.org/licenses/>.
#*******************************************************************************

import sys
sys.path.append('../python/')

import hypoct
import numpy as np
#import timeit

if __name__ == '__main__':
  n = 100
  theta = np.linspace(0, 2*np.pi, n+1)[:n]
  x = np.array([np.cos(theta), np.sin(theta)])
  tree = hypoct.build(x, 1)
  print tree.xi
  print
  print tree.lvlx
  print
  print tree.nodex
  #print timeit.timeit("tree = hypoct.build(x, 1)", "from __main__ import hypoct, x", number=1)