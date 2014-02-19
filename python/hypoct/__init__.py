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

"""
Python module for interfacing with `hypoct`.
"""

from hypoct_python import hypoct_python as _hypoct
import numpy as np

class Tree:
  """
  Build hyperoctree.

  :param x:
    Point coordinates, where the coordinate of point `i` is `x[:,i]`.
  :type x: :class:`numpy.ndarray`

  :keyword adap:
    Adaptivity setting: adaptive (`'a'`), uniform (`'u'`). This specifies
    whether nodes are divided adaptively or to a uniformly fine level.
  :type adap: {`'a'`, `'u'`}

  :keyword elem:
    Element type: point (`'p'`), element (`'e'`), sparse element (`'s'`). This
    specifies whether the input points represent true points or general elements
    (with sizes) that can extend beyond node boundaries. The element type
    determines how points are assigned to nodes and also how node neighbors are
    defined (see :meth:`find_neighbors`). If `elem = 'p'`, then `siz` is
    ignored. Points and elements are intended to interact densely with each
    other, whereas sparse elements are intended to interact only by overlap.
  :type elem: {`'p'`, `'e'`, `'s'`}

  :keyword siz:
    Sizes associated with each point. If `siz` is a single float, then it is
    automatically expanded into an appropriately sized constant array. Not
    accessed if `elem = 'p'`.
  :type siz: :class:`numpy.ndarray`

  :keyword occ:
    Maximum leaf occupancy. Requires `occ > 0`.
  :type occ: int

  :keyword lvlmax:
    Maximum tree depth. The root is defined to have level zero. No maximum if
    `lvlmax < 0`.
  :type lvlmax: int

  :keyword ext:
    Extent of root node. If `ext[i] <= 0`, then the extent in dimension `i` is
    calculated from the data. Its primary use is to force nodes to conform to a
    a specified geometry (see :meth:`find_neighbors`). If `ext` is a single
    float, then it is automatically expanded into an appropriately sized
    constant array.
  :type ext: :class:`numpy.ndarray`
  """

  def __init__(self, x, adap='a', elem='p', siz=0, occ=1, lvlmax=-1, ext=0):
    """
    Initialize.
    """
    # process inputs
    self.x = np.asfortranarray(x)
    siz    = np.asfortranarray(siz)
    ext    = np.asfortranarray(ext)
    d, n = self.x.shape
    if (siz.size == 1): siz = siz * np.ones(n)
    if (ext.size == 1): ext = ext * np.ones(d)

    # call Fortran routine
    self.rootx, self.xi = _hypoct.hypoct_python_build(adap, elem, self.x, siz,
                                                      occ, lvlmax, ext)
    self.lvlx  = np.array(_hypoct.lvlx)
    self.xp    = np.array(_hypoct.xp)
    self.nodex = np.array(_hypoct.nodex)
    _hypoct.lvlx  = None
    _hypoct.xp    = None
    _hypoct.nodex = None

    # set properties
    self.properties = {'adap':   adap,
                       'elem':   elem,
                        'siz':    siz,
                        'occ':    occ,
                     'lvlmax': lvlmax,
                        'ext':    ext}

    # set flags
    self._flags = {'chld': False,
                   'geom': False,
                   'ilst': False,
                   'nbor': False}

  def generate_child_data(self):
    """
    Generate child data.
    """
    # call Fortran routine
    _hypoct.hypoct_python_chld(self.lvlx, self.nodex)
    self.chldp = np.array(_hypoct.chldp)
    _hypoct.chldp = None

    # set flags
    self._flags['chld'] = True

  def generate_geometry_data(self):
    """
    Generate geometry data.
    """
    # call Fortran routine
    _hypoct.hypoct_python_geom(self.lvlx, self.rootx, self.nodex)
    self.l   = np.array(_hypoct.l)
    self.ctr = np.array(_hypoct.ctr)
    _hypoct.l   = None
    _hypoct.ctr = None

    # set flags
    self._flags['geom'] = True

  def find_neighbors(self, per=False):
    """
    Find neighbors.

    For points (`elem = 'p'`), the neighbors of a given node consist of:

    - All nodes at the same level immediately adjoining it ("one over").

    - All non-empty nodes at a coarser level (parent or above) immediately
      adjoining it.

    For elements and sparse elements (`elem = 'e'` or `'s'`), first let the
    extension of a node be the spatial region corresponding to all possible
    point distributions belonging to that node.

    Then for elements (`elem = 'e'`), the neighbors of a given node consist of:

    - All nodes at the same level separated by at most the node's size ("two
      over")

    - All non-empty nodes at a coarser level (parent or above) whose extensions
      are separated from its own extension by less than its extension's size.

    Finally, for sparse elements (`elem = 's'`), the neighbors consist of:

    - All nodes at the same level immediately adjoining it ("one over").

    - All non-empty nodes at a coarser level (parent or above) whose extensions
      overlap with its own extension.

    In all cases, a node is not considered its own neighbor.

    This routine requires that the child and geometry data have already been
    generated. If this is not the case, then this is done automatically.

    See :meth:`generate_child_data` and :meth:`generate_geometry_data`.

    :param per:
      Periodicity of root note. The domain is periodic in dimension `i` if
      `per[i] = True`. If `per` is a single bool, then it is automatically
      expanded into an appropriately sized constant array. Use `ext` in
      :meth:`Tree` to control the extent of the root.
    :type per: :class:`numpy.ndarray`
    """
    # generate child and geometry data if nonexistent
    if not self._flags['chld']: self.generate_child_data()
    if not self._flags['geom']: self.generate_geometry_data()

    # process inputs
    per = np.asfortranarray(per, dtype='int32')
    if (per.size == 1): per = per * np.ones(self.x.shape[0], dtype='int32')

    # call Fortran routine
    _hypoct.hypoct_python_nbor(self.properties['elem'], self.lvlx, self.xp,
                               self.nodex, self.chldp, self.l, self.ctr, per)
    self.nbori = np.array(_hypoct.nbori)
    self.nborp = np.array(_hypoct.nborp)
    _hypoct.nborp = None
    if _hypoct.nbori.size > 0: _hypoct.nbori = None

    # set properties
    self.properties['per'] = per

    # set flags
    self._flags['nbor'] = True
    self._flags['ilst'] = False

  def get_interaction_lists(self):
    """
    Get interaction lists.

    The interaction list of a given node consists of:

    - All nodes at the same level that are children of the neighbors of the
      node's parent but not neighbors of the node itself.

    - All non-empty nodes at a coarser level (parent or above) that are
      neighbors of the node's parent but not neighbors of the node itself.

    This routine requires that the neighbor data have already been generated. If
    this is not the case, then this is done automatically (at default settings).

    See :meth:`find_neighbors`.
    """
    # find neighbors if nonexistent
    if not self._flags['nbor']: self.find_neighbors()

    # call Fortran routine
    _hypoct.hypoct_python_ilst(self.lvlx, self.xp, self.nodex, self.chldp,
                               self.nbori, self.nborp)
    self.ilsti = np.array(_hypoct.ilsti)
    self.ilstp = np.array(_hypoct.ilstp)
    _hypoct.ilstp = None
    if _hypoct.ilsti.size > 0: _hypoct.ilsti = None

    # set flags
    self._flags['ilst'] = True

  def search(self, x, siz=0, mlvl=-1):
    """
    Search hyperoctree.

    The element type of the points to search for are assumed to be the same as
    that used to construct the tree.

    This routine requires that the child and geometry data have already been
    generated. If this is not the case, then this is done automatically.

    See :meth:`generate_child_data` and :meth:`generate_geometry_data`.

    :param x:
      Point coordinates to search for, where the coordinate of point `i` is
      x[:,i]`.
    :type x: :class:`numpy.ndarray`

    :keyword siz:
      Sizes associated with each point. If `siz` is a single float, then it is
      automatically expanded into an appropriately sized constant array. Not
      accessed if `elem = 'p'`.
    :type siz: :class:`numpy.ndarray`

    :keyword mlvl:
      Maximum tree depth to search. Defaults to full tree depth if `mlvl < 0`.
    :type mlvl: int

    :return:
      Tree traversal array. The node containing point `i` at level `j` has index
      `trav[i,j]`; if no such node exists, then `trav[i,j] = 0`.
    :rtype: :class:`numpy.ndarray`
    """
    # generate child and geometry data if nonexistent
    if not self._flags['chld']: self.generate_child_data()
    if not self._flags['geom']: self.generate_geometry_data()

    # process inputs
    siz = np.asfortranarray(siz)
    n = x.shape[1]
    if (siz.size == 1): siz = siz * np.ones(n)
    if mlvl < 0: mlvl = self.lvlx[1,0]

    # call Fortran routine
    return _hypoct.hypoct_python_search(self.properties['elem'], x, siz, mlvl,
                                        self.lvlx, self.nodex, self.chldp,
                                        self.l, self.ctr)