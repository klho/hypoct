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
import time

if __name__ == '__main__':
  n = 2**20
  theta = np.linspace(0, 2*np.pi, n+1)[:n]
  x = np.array([np.cos(theta), np.sin(theta)])

  print "Building tree...",
  t0 = time.clock()
  tree = hypoct.Tree(x, occ=20)
  t = time.clock() - t0
  print "%12.4e (s)" % t

  print "Generating geometry data..."
  t0 = time.clock()
  tree.generate_geometry_data()
  t = time.clock() - t0

  t0 = time.clock()
  tree.generate_child_data()
  t = time.clock() - t0

  t0 = time.clock()
  tree.find_neighbors()
  t = time.clock() - t0

  t0 = time.clock()
  tree.get_interaction_list()
  t = time.clock() - t0