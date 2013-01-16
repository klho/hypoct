from hypoct_python import hypoct_python as hypoct
import numpy as np

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

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

def build(x, occ, adap='a', intr='p', siz=0, lvlmax=-1, ext=0):
  x   = np.array(x)
  siz = np.array(siz)
  ext = np.array(ext)
  d, n = x.shape
  if (siz.size == 1): siz = siz * np.ones(n)
  if (ext.size == 1): ext = ext * np.ones(d)

  rootx, xi = hypoct.hypoct_python_buildx(adap, intr, x, siz, occ, lvlmax, ext)
  lvlx  = hypoct.lvlx.copy()
  nodex = hypoct.nodex.copy()
  hypoct.lvlx  = []
  hypoct.nodex = []

  tree = Tree(x, lvlx, rootx, xi, nodex, adap=adap, intr=intr, siz=siz,
              lvlmax=lvlmax, ext=ext)

  return tree

def geom(tree):
  hypoct.hypoct_python_geom(tree.lvlx, tree.rootx, tree.nodex)
  l   = hypoct.l  .copy()
  ctr = hypoct.ctr.copy()
  hypoct.l   = []
  hypoct.ctr = []

  tree.properties['l'  ] = l
  tree.properties['ctr'] = ctr