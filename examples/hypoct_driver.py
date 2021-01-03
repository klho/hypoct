#*******************************************************************************
#   Copyright (C) 2013-2014 Kenneth L. Ho
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
  """
  Build quadtree on uniformly spaced points on the unit circle.
  """
  # initialize points
  n = 2**20
  theta = np.linspace(0, 2*np.pi, n+1)[:n]
  x = np.array([np.cos(theta), np.sin(theta)], order='F')

  # print input summary
  print("Number of points:                           {:8d}".format(n))
  print("----------------------------------------------------")

  # set print format
  fmt = "{:10.4e} (s) / {:6.2f} (MB)"

  # build tree
  print("Building tree...            ", end=" ")
  t0 = time.perf_counter()
  tree = hypoct.Tree(x, occ=16)
  t = time.perf_counter() - t0
  mb = 1e-6*(tree.lvlx.nbytes + tree.rootx.nbytes + tree.xi.nbytes
           + tree.xp.nbytes + tree.nodex.nbytes)
  print(fmt.format(t, mb))

  # generate child data
  print("Generating child data...    ", end=" ")
  t0 = time.perf_counter()
  tree.generate_child_data()
  t = time.perf_counter() - t0
  mb = 1e-6*(tree.chldp.nbytes)
  print(fmt.format(t, mb))

  # generate geometry data
  print("Generating geometry data... ", end=" ")
  t0 = time.perf_counter()
  tree.generate_geometry_data()
  t = time.perf_counter() - t0
  mb = 1e-6*(tree.l.nbytes + tree.ctr.nbytes)
  print(fmt.format(t, mb))

  # find neighbors
  print("Finding neighbors...        ", end=" ")
  t0 = time.perf_counter()
  tree.find_neighbors()
  t = time.perf_counter() - t0
  mb = 1e-6*(tree.nborp.nbytes + tree.nbori.nbytes)
  print(fmt.format(t, mb))

  # get interaction lists
  print("Getting interaction lists...", end=" ")
  t0 = time.perf_counter()
  tree.get_interaction_lists()
  t = time.perf_counter() - t0
  mb = 1e-6*(tree.ilstp.nbytes + tree.ilsti.nbytes)
  print(fmt.format(t, mb))

  # search tree
  print("Searching tree...           ", end=" ")
  m = 2**16;
  y = np.random.rand(2, m)
  y = 2*y - 1
  t0 = time.perf_counter()
  trav = tree.search(y)
  t = time.perf_counter() - t0
  mb = 1e-6*trav.nbytes
  print(fmt.format(t, mb))

  # print output summary
  print("----------------------------------------------------")
  print("Tree depth:                                 {:8d}"
          .format(tree.lvlx[1, 0]))
  print("Number of nodes:                            {:8d}"
          .format(tree.lvlx[0,-1]))
  print("Total number of neighbors:                  {:8d}"
          .format(tree.nborp[-1]))
  print("Total number of nodes in interaction lists: {:8d}"
          .format(tree.ilstp[-1]))
