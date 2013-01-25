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

"""
Python module for interfacing with hypoct.
"""

from hypoct_python import hypoct_python
import numpy as np

def build(x, occ, adap='a', intr='p', siz=0, lvlmax=-1, ext=0):
  """
  Build hyperoctree.

  :param x: point coordinates
  :type x: :class:`list`
  :param occ: maximum leaf occupancy
  :param adap: adaptivity setting
  :param intr: interaction type
  :param siz: sizes associated with each point
  :param lvlmax: maximum tree depth
  :param ext: extent of root node
  :rtype: 
  """
  x   = np.asfortranarray(x)
  siz = np.asfortranarray(siz)
  ext = np.asfortranarray(ext)
  d, n = x.shape
  if (siz.size == 1): siz = siz * np.ones(n, order='F')
  if (ext.size == 1): ext = ext * np.ones(d, order='F')

  rootx, xi = hypoct_python.hypoct_python_buildx(adap, intr, x, siz, occ,
                                                 lvlmax, ext)
  lvlx  = np.array(hypoct_python.lvlx,  order='F')
  nodex = np.array(hypoct_python.nodex, order='F')
  hypoct_python.lvlx  = None
  hypoct_python.nodex = None

  tree = Tree(x, lvlx, rootx, xi, nodex, adap=adap, intr=intr, siz=siz,
              lvlmax=lvlmax, ext=ext)

  return tree

class Tree:
  """
  Tree
  """

  def __init__(self, x, lvlx, rootx, xi, nodex, **kwargs):
    self.x     = x
    self.lvlx  = lvlx
    self.rootx = rootx
    self.xi    = xi
    self.nodex = nodex
    self.properties = kwargs

    self._flags = {'has_chld': False,
                   'has_geom': False,
                   'has_ilst': False,
                   'has_nbor': False}

  def generate_child_data(self):
    hypoct_python.hypoct_python_chld(self.lvlx, self.nodex)
    chldp = np.array(hypoct_python.chldp, order='F')
    hypoct_python.chldp = None

    self.properties['chldp'] = chldp
    self._flags['has_chld'] = True

  def generate_geometry_data(self):
    hypoct_python.hypoct_python_geom(self.lvlx, self.rootx, self.nodex)
    l   = np.array(hypoct_python.l,   order='F')
    ctr = np.array(hypoct_python.ctr, order='F')
    hypoct_python.l   = None
    hypoct_python.ctr = None

    self.properties['l'  ] = l
    self.properties['ctr'] = ctr
    self._flags['has_geom'] = True

  def find_neighbors(self, per=False):
    if not self._flags['has_chld']: self.generate_child_data()
    per = np.asfortranarray(per)
    if (per.size == 1):
      per = per * np.ones(self.x.shape[0], dtype='int32', order='F')
    hypoct_python.hypoct_python_nborx(self.lvlx, self.nodex,
                                      self.properties['chldp'], per)
    nborp = np.array(hypoct_python.nborp, order='F')
    nbori = np.array(hypoct_python.nbori, order='F')
    hypoct_python.nborp = None
    hypoct_python.nbori = None

    self.properties['per']   = per
    self.properties['nborp'] = nborp
    self.properties['nbori'] = nbori
    self._flags['has_nbor'] = True
    self._flags['has_ilst'] = False

  def get_interaction_list(self):
    if not self._flags['has_nbor']: self.find_neighbors()
    hypoct_python.hypoct_python_ilst(
      self.lvlx, self.nodex, self.properties['chldp'], self.properties['nborp'],
      self.properties['nbori'])
    ilstp = np.array(hypoct_python.ilstp, order='F')
    ilsti = np.array(hypoct_python.ilsti, order='F')
    hypoct_python.ilstp = None
    hypoct_python.ilsti = None

    self.properties['ilstp'] = ilstp
    self.properties['ilsti'] = ilsti
    self._flags['has_ilst'] = True

  def update(self):
    if self._flags['has_nbor']:
      if not self._flags['has_ilst']: self.get_interaction_list()