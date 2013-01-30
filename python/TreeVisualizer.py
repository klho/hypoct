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
Python module for visualizing 2D hyperoctrees.
"""

import matplotlib as mpl
from matplotlib import pyplot as plt
from matplotlib.patches import Rectangle
import numpy as np

class TreeVisualizer:
  """
  Visualize 2D hyperoctrees.

  :param tree:
    Hyperoctree.
  :type tree: :class:`hypoct.Tree`
  """

  def __init__(self, tree):
    """
    Initialize.
    """
    self.tree = tree
    if self.tree.x.shape[0] != 2:
      raise NotImplementedError("currently requires D = 2")
    if not self.tree._flags['geom']: tree.generate_geometry_data()

  def draw_base(self, c='k', **kwargs):
    """
    Draw wireframe outlines of all nodes in the tree.

    Accepts all :func:`matplotlib.pyplot.plot` keyword arguments.
    """
    for i in range(self.tree.lvlx[1,0]+1):
      l = self.tree.l[:,i]
      for j in range(self.tree.lvlx[0,i], self.tree.lvlx[0,i+1]):
        ctr = self.tree.ctr[:,j]
        x = np.array([[ctr[0] - 0.5*l[0], ctr[1] - 0.5*l[1]],
                      [ctr[0] + 0.5*l[0], ctr[1] - 0.5*l[1]],
                      [ctr[0] + 0.5*l[0], ctr[1] + 0.5*l[1]],
                      [ctr[0] - 0.5*l[0], ctr[1] + 0.5*l[1]],
                      [ctr[0] - 0.5*l[0], ctr[1] - 0.5*l[1]]])
        plt.plot(x[:,0], x[:,1], c=c, **kwargs)

  def draw_node(self, index, level=None, draw_points=True, update=True,
                node_alpha=0.5, node_color='b', point_c='k', **kwargs):
    """
    Draw node patch and, optionally, all points contained within it.

    The node is drawn as a :class:`matplotlib.patches.Rectangle` instance.
    Points are drawn using :func:`matplotlib.pyplot.scatter`.

    All keyword arguments prefaced with `'node_'` are passed to the node drawing
    routine without the prefix, and, similarly all arguments prefixed with
    `'point_'` are passed to the point drawing routine without the prefix. For
    example, setting `node_color='r'` and `point_c='b'` passes the keyword
    argument `color='r'` to the node drawer and `c='b'` to the point drawer.

    :param index:
      Node index.
    :type index: int

    :keyword level:
      Level of node. If `None`, the level is found automatically.
    :type level: int

    :keyword draw_points:
      Whether to draw points contained in the node.
    :type draw_points: bool

    :keyword update:
      Whether to update the plot after drawing the node.
    :type update: bool
    """
    # parse keyword arguments
    node_kwargs  = {'alpha': node_alpha,
                    'color': node_color}
    point_kwargs = {'c': point_c}
    for kw in kwargs:
      key = kw.split('_')
      if   key[0] ==  'node':  node_kwargs[key[1]] = kwargs[kw]
      elif key[0] == 'point': point_kwargs[key[1]] = kwargs[kw]
      else: raise Warning("unrecognized keyword '%s'" % kw)

    # find level if not provided
    if not level:
      for level in range(self.tree.lvlx[1,0]+1):
        if (self.tree.lvlx[0,level] < index <= self.tree.lvlx[0,level+1]): break

    # draw node patch
    l = self.tree.l[:,level]
    ctr = self.tree.ctr[:,index-1]
    plt.gca().add_patch(Rectangle(ctr - 0.5*l, l[0], l[1], **node_kwargs))

    # draw points in node
    if draw_points:
      for i in range(self.tree.nodex[0,index-1], self.tree.nodex[0,index]):
        plt.scatter(self.tree.x[0,self.tree.xi[i]-1],
                    self.tree.x[1,self.tree.xi[i]-1], **point_kwargs)

    # update plot
    if update: plt.draw()

  def draw_interactive(self, draw_neighbors=True, draw_interaction_list=True,
                       base_c='k', node_alpha=0.5, node_color='b',
                       nbor_alpha=0.5, nbor_color='r', ilst_alpha=0.5,
                       ilst_color='g', point_c='k', **kwargs):
    """
    Draw each node in the tree sequentially via an interactive session (press
    ``Enter`` to continue) along with all neighbor and interaction list data, if
    available.

    All keyword arguments prefaced with `'base_'` are passed to
    :meth:`draw_base`; those prefaced with `'node_'` are passed to
    :meth:`draw_node` when drawing each node; those prefaced with `'nbor_'` are
    passed to :meth:`draw_node` when drawing each node neighbor; and those
    prefaced with `'ilst_'` are passed to :meth:`draw_node` when drawing each
    node in the interaction list.

    See :meth:`draw_base` and :meth:`draw_node` for details.

    :keyword draw_neighbors:
      Whether to draw neighbors.
    :type draw_neighbors: bool

    :keyword draw_interaction_list:
      Whether to draw interaction lists.
    :type draw_interaction_list: bool
    """
    # parse keyword arguments
    base_kwargs  = {'c': base_c}
    node_kwargs  = {'node_alpha': node_alpha,
                    'node_color': node_color}
    nbor_kwargs  = {'node_alpha': nbor_alpha,
                    'node_color': nbor_color}
    ilst_kwargs  = {'node_alpha': ilst_alpha,
                    'node_color': ilst_color}
    point_kwargs = {'point_c': point_c}
    for kw in kwargs:
      key = kw.split('_')
      if   key[0] ==  'base':  base_kwargs[          key[1]] = kwargs[kw]
      elif key[0] ==  'node':  node_kwargs[          kw    ] = kwargs[kw]
      elif key[0] ==  'nbor':  nbor_kwargs['node_' + key[1]] = kwargs[kw]
      elif key[0] == ' ilst':  ilst_kwargs['node_' + key[1]] = kwargs[kw]
      elif key[0] == 'point': point_kwargs[          kw    ] = kwargs[kw]
      else: raise Warning("unrecognized keyword '%s'" % kw)
    node_kwargs.update(point_kwargs)

    # draw outlines of all nodes
    plt.ion()
    plt.show()
    self.draw_base(**base_kwargs)
    axis = plt.axis('equal')
    axis = (axis[0] - 0.05*(axis[1] - axis[0]),
            axis[1] + 0.05*(axis[1] - axis[0]),
            axis[2] - 0.05*(axis[3] - axis[2]),
            axis[3] + 0.05*(axis[3] - axis[2]))
    plt.axis(axis)

    # loop through all nodes
    for i in range(self.tree.lvlx[1,0]+1):
      for j in range(self.tree.lvlx[0,i], self.tree.lvlx[0,i+1]):

        # wait for keyboard input
        raw_input()

        # delete all node patches and points
        plt.gca().patches     = []
        plt.gca().collections = []

        # draw current node
        self.draw_node(j+1, level=i, update=False, **node_kwargs)

        # draw neighbors
        if draw_neighbors and self.tree._flags['nbor']:
          for k in range(self.tree.nborp[j], self.tree.nborp[j+1]):
            self.draw_node(self.tree.nbori[k], draw_points=False, update=False,
                           **nbor_kwargs)

        # draw interaction  list
        if draw_interaction_list and self.tree._flags['ilst']:
          for k in range(self.tree.ilstp[j], self.tree.ilstp[j+1]):
            self.draw_node(self.tree.ilsti[k], draw_points=False, update=False,
                           **ilst_kwargs)

        # update plot
        plt.axis(axis)
        plt.draw()

        # format descriptor string
        s = "Node %d/%d:\n" % (j+1, self.tree.lvlx[0,-1])
        s += "  center:\n    %s\n" % self.tree.ctr[:,j]
        s += "  extent:\n    %s\n" % self.tree.  l[:,i]

        # add contained points to description
        if self.tree.nodex[0,j] < self.tree.nodex[0,j+1]:
          s += "  points:\n"
          for k in range(self.tree.nodex[0,j], self.tree.nodex[0,j+1]):
            l = self.tree.xi[k] - 1
            s += "    %s" % self.tree.x[:,l]
            if 'siz' in self.tree.properties:
              s += " (size = %s)" % self.tree.properties['siz'][l]
            s += '\n'

        # print description
        print s.rstrip('\n')