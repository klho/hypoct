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
Python module for interfacing with ``hypoct``.
"""

from hypoct_python import hypoct_python
import numpy as np

class Tree:
  """
  Build hyperoctree.

  :param x:
    Point coordinates, where the coordinate of point ``i`` is ``x[:,i]``.
  :type x: :class:`numpy.ndarray`

  :keyword adap:
    Adaptivity setting: adaptive, uniform.
  :type adap: {``'a'``, ``'u'``}

  :keyword intr:
    Interaction type: point-point, point-element (collocation or qualocation),
    element-element (Galerkin).
  :type intr: {``'p'``, ``'c'``, ``'g'``}

  :keyword siz:
    Sizes associated with each point. If ``siz`` is a single float, then it is
    automatically expanded into an appropriately sized constant array. Ignored
    if ``intr = 'p'``.
  :type siz: :class:`numpy.ndarray`

  :keyword lvlmax:
    Maximum tree depth. No maximum if ``lvlmax < 0``.
  :type lvlmax: int

  :keyword ext:
    Extent of root node. If ``ext[i] <= 0``, then the extent in dimension ``i``
    is calculated from the data. If ``ext`` is a single float, then it is
    automatically expanded into an appropriately sized constant array.
  :type ext: :class:`numpy.ndarray`
  """

  def __init__(self, x, occ=1, adap='a', intr='p', siz=0, lvlmax=-1, ext=0):
    """
    Initialize.
    """
    # process inputs
    self.x = np.asfortranarray(  x)
    siz    = np.asfortranarray(siz)
    ext    = np.asfortranarray(ext)
    d, n = self.x.shape
    if (siz.size == 1): siz = siz * np.ones(n, order='F')
    if (ext.size == 1): ext = ext * np.ones(d, order='F')

    # call Fortran routine
    self.rootx, self.xi = hypoct_python.hypoct_python_buildx(
                            adap, intr, self.x, siz, occ, lvlmax, ext
                          )
    self.lvlx  = np.array(hypoct_python.lvlx,  order='F')
    self.nodex = np.array(hypoct_python.nodex, order='F')
    hypoct_python.lvlx  = None
    hypoct_python.nodex = None

    # set properties
    self.properties = {'adap':   adap,
                       'intr':   intr,
                        'siz':    siz,
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
    hypoct_python.hypoct_python_chld(self.lvlx, self.nodex)
    self.chldp = np.array(hypoct_python.chldp, order='F')
    hypoct_python.chldp = None

    # set flags
    self._flags['chld'] = True

  def generate_geometry_data(self):
    """
    Generate geometry data.
    """
    # call Fortran routine
    hypoct_python.hypoct_python_geom(self.lvlx, self.rootx, self.nodex)
    self.l   = np.array(hypoct_python.l,   order='F')
    self.ctr = np.array(hypoct_python.ctr, order='F')
    hypoct_python.l   = None
    hypoct_python.ctr = None

    # set flags
    self._flags['geom'] = True

  def find_neighbors(self, per=False):
    """
    Find neighbors.

    The neighbors of a given node are those nodes at the same level which adjoin
    it.

    :param per:
      Periodicity of root note. The domain is periodic in dimension ``i`` if
      ``per[i] = True``. Use ``ext`` in :meth:`Tree` to control the extent of
      the root.
    :type per: :class:`numpy.ndarray`

    .. tip::
       To avoid array copies, typecast ``per`` as ``dtype='int32'``.
    """
    # generate child data if non-existent
    if not self._flags['chld']: self.generate_child_data()

    # process `per` input
    per = np.asfortranarray(per)
    if (per.size == 1):
      per = per * np.ones(self.x.shape[0], dtype='int32', order='F')

    # call Fortran routine
    hypoct_python.hypoct_python_nborx(self.lvlx, self.nodex, self.chldp, per)
    self.nborp = np.array(hypoct_python.nborp, order='F')
    self.nbori = np.array(hypoct_python.nbori, order='F')
    hypoct_python.nborp = None
    hypoct_python.nbori = None

    # set properties
    self.properties['per'] = per

    # set flags
    self._flags['nbor'] = True
    self._flags['ilst'] = False

  def get_interaction_list(self):
    """
    Get interaction list.

    The interaction list of a given node consists of those nodes who are the
    children of its parent's neighbors but who are not themselves neighbors.

    See :meth:`find_neighbors`.
    """
    # find neighbors if non-existent
    if not self._flags['nbor']: self.find_neighbors()

    # call Fortran routine
    hypoct_python.hypoct_python_ilst(self.lvlx, self.nodex, self.chldp,
                                     self.nborp, self.nbori)
    self.ilstp = np.array(hypoct_python.ilstp, order='F')
    self.ilsti = np.array(hypoct_python.ilsti, order='F')
    hypoct_python.ilstp = None
    hypoct_python.ilsti = None

    # set flags
    self._flags['ilst'] = True