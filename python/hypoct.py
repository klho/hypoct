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

from hypoct_python import hypoct_python as hypoct
import numpy as np

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

def build(x, occ, adap='a', intr='p', siz=0, lvlmax=-1, ext=0):
  x   = np.asfortranarray(x)
  siz = np.asfortranarray(siz)
  ext = np.asfortranarray(ext)
  d, n = x.shape
  if (siz.size == 1): siz = siz * np.ones(n, order='F')
  if (ext.size == 1): ext = ext * np.ones(d, order='F')

  rootx, xi = hypoct.hypoct_python_buildx(adap, intr, x, siz, occ, lvlmax, ext)
  lvlx  = np.array(hypoct.lvlx,  order='F')
  nodex = np.array(hypoct.nodex, order='F')
  hypoct.lvlx  = None
  hypoct.nodex = None

  tree = Tree(x, lvlx, rootx, xi, nodex, adap=adap, intr=intr, siz=siz,
              lvlmax=lvlmax, ext=ext)

  return tree

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

class Tree:

  def __init__(self, x, lvlx, rootx, xi, nodex, **kwargs):
    self.x     = x
    self.lvlx  = lvlx
    self.rootx = rootx
    self.xi    = xi
    self.nodex = nodex
    self.properties = kwargs

    self._flags = {'has_chld': False,
                  'has_geom': False,
                  'has_nbor': False}

  def generate_child_data(self):
    hypoct.hypoct_python_chld(self.lvlx, self.nodex)
    chldx = np.array(hypoct.chldx, order='F')
    hypoct.chldx = None

    self.properties['chldx'] = chldx
    self._flags['has_chld'] = True

  def generate_geometry_data(self):
    hypoct.hypoct_python_geom(self.lvlx, self.rootx, self.nodex)
    l   = np.array(hypoct.l,   order='F')
    ctr = np.array(hypoct.ctr, order='F')
    hypoct.l   = None
    hypoct.ctr = None

    self.properties['l'  ] = l
    self.properties['ctr'] = ctr
    self._flags['has_geom'] = True

  def generate_neighbor_data(self, per=False):
    if not self._flags['has_chld']: self.generate_child_data()
    per = np.asfortranarray(per)
    if (per.size == 1):
      per = per * np.ones(self.x.shape[0], dtype='int32', order='F')
    hypoct.hypoct_python_nborsx(self.lvlx, self.nodex, self.properties['chldx'],
                                per)
    nborp = np.array(hypoct.nborp, order='F')
    nbori = np.array(hypoct.nbori, order='F')
    hypoct.nborp = None
    hypoct.nbori = None

    self.properties['nborp'] = nborp
    self.properties['nbori'] = nbori
    self._flags['has_nbor'] = True