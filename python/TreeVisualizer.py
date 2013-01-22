import matplotlib as mpl
from matplotlib import pyplot as plt
import numpy as np

class TreeVisualizer:

  def __init__(self, tree):
    self.tree = tree
    if self.tree.x.shape[0] != 2:
      raise NotImplementedError("currently requires D = 2")
    if not self.tree._flags['has_geom']: tree.generate_geometry_data()

  def draw_base(self, **kwargs):
    for i in range(self.tree.lvlx[1,0]+1):
      l = self.tree.properties['l'][:,i]
      for j in range(self.tree.lvlx[0,i], self.tree.lvlx[0,i+1]):
        ctr = self.tree.properties['ctr'][:,j]
        x = np.array([[ctr[0] - 0.5*l[0], ctr[1] - 0.5*l[1]],
                      [ctr[0] + 0.5*l[0], ctr[1] - 0.5*l[1]],
                      [ctr[0] + 0.5*l[0], ctr[1] + 0.5*l[1]],
                      [ctr[0] - 0.5*l[0], ctr[1] + 0.5*l[1]],
                      [ctr[0] - 0.5*l[0], ctr[1] - 0.5*l[1]]])
        plt.plot(x[:,0], x[:,1], **kwargs)

  def draw_node(self, index, level=None, draw_points=True, update=True,
                **kwargs):
    if not level:
      for level in range(self.tree.lvlx[1,0]+1):
        if (self.tree.lvlx[0,level] < index <= self.tree.lvlx[0,level+1]): break
    l = self.tree.properties['l'][:,level]
    ctr = self.tree.properties['ctr'][:,index-1]
    verts = [ctr + [-0.5, -0.5]*l, ctr + [+0.5, -0.5]*l,
             ctr + [+0.5, +0.5]*l, ctr + [-0.5, +0.5]*l]
    patch = mpl.collections.PolyCollection([verts], **kwargs)
    if draw_points:
      for i in range(self.tree.nodex[0,index-1], self.tree.nodex[0,index]):
        plt.scatter(self.tree.x[0,self.tree.xi[i]-1],
                    self.tree.x[1,self.tree.xi[i]-1], c='k')
    plt.gca().add_collection(patch)
    if update: plt.draw()

  def draw_interactive(self, draw_neighbors=True):
    plt.ion()
    plt.show()
    self.draw_base(c='k')
    axis = plt.axis('equal')
    axis = (axis[0] - 0.05*(axis[1] - axis[0]),
            axis[1] + 0.05*(axis[1] - axis[0]),
            axis[2] - 0.05*(axis[3] - axis[2]),
            axis[3] + 0.05*(axis[3] - axis[2]))
    plt.axis(axis)
    for i in range(self.tree.lvlx[1,0]+1):
      for j in range(self.tree.lvlx[0,i], self.tree.lvlx[0,i+1]):
        raw_input()
        plt.gca().collections = []
        self.draw_node(j+1, level=i, update=False, alpha=0.5, color='b')
        if draw_neighbors and self.tree._flags['has_nbor']:
          for k in range(self.tree.properties['nborp'][j],
                         self.tree.properties['nborp'][j+1]):
            self.draw_node(self.tree.properties['nbori'][k], draw_points=False,
                           update=False, alpha=0.5, color='r')
        plt.axis(axis)
        plt.draw()
        s = "Node %d/%d:\n" % (j + 1, self.tree.lvlx[0,-1])
        s += "  center:\n    %s\n" % self.tree.properties['ctr'][:,j]
        s += "  extent:\n    %s\n" % self.tree.properties['l'][:,i]
        if self.tree.nodex[0,j] < self.tree.nodex[0,j+1]:
          s += "  points:\n"
          for k in range(self.tree.nodex[0,j], self.tree.nodex[0,j+1]):
            l = self.tree.xi[k] - 1
            s += "    %s" % self.tree.x[:,l]
            if 'siz' in self.tree.properties:
              s += " (size = %s)" % self.tree.properties['siz'][l]
            s += '\n'
        if draw_neighbors and self.tree._flags['has_nbor']:
          if self.tree.properties['nborp'][j] < self.tree.properties['nborp'][j+1]:
            s += "  neighbors:\n"
            for k in range(self.tree.properties['nborp'][j], self.tree.properties['nborp'][j+1]):
              s += "    %s\n" % self.tree.properties['nbori'][k]
        print s.rstrip('\n')