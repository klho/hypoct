/*******************************************************************************
 * Copyright (C) 2013-2014 Kenneth L. Ho
 *
 * This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

/*
 * Header file for C interface to HYPOCT.
 *
 * All functions share the same name as the corresponding wrapped Fortran
 * routine, with arguments of the same name playing the same role. Note that
 * some functions have additional arguments (namely, array descriptors).
 *
 * Array outputs are returned in Fortran order; see the Fortran source for more
 * details.
 */

/*
 * Build hyperoctree.
 *
 * Additional arguments:
 *   NLVL  - tree depth      (output)
 *   NNODE - number of nodes (output)
 */
extern void hypoct_build(
  char *adap, char *elem, int *d, int *n, double *x, double *siz, int *occ,
  int *lvlmax, double *ext, int *nlvl, int *nnode, int **lvlx, double *root,
  int *xi, int **xp, int **nodex
);

/*
 * Generate child data.
 *
 * Additional arguments:
 *   NLVL  - tree depth      (input)
 *   NNODE - number of nodes (input)
 */
extern void hypoct_chld(
  int *nlvl, int *nnode, int **lvlx, int **nodex, int **chldp
);

/*
 * Generate geometry data.
 *
 * Additional arguments:
 *   NLVL  - tree depth      (input)
 *   NNODE - number of nodes (input)
 */
extern void hypoct_geom(
  int *d, int *nlvl, int *nnode, int **lvlx, double *rootx, int **nodex,
  double **l, double **ctr
);

/*
 * Get interaction lists.
 *
 * Additional arguments:
 *   NLVL  - tree depth                                 ( input)
 *   NNODE - number of nodes                            ( input)
 *   NNBOR - total number of neighbors                  ( input)
 *   NILST - total number of nodes in interaction lists (output)
 */
extern void hypoct_ilst(
  int *nlvl, int *nnode, int **lvlx, int **xp, int **nodex, int **chldp,
  int *nnbor, int **nborp, int **nbori, int *nilst, int **ilsti, int **ilstp
);

/*
 * Find neighbors.
 *
 * Additional arguments:
 *   NLVL  - tree depth                ( input)
 *   NNODE - number of nodes           ( input)
 *   NNBOR - total number of neighbors (output)
 */
extern void hypoct_nbor(
  char *elem, int *d, int *nlvl, int *nnode, int **lvlx, int **xp, int **nodex,
  int **chldp, double **l, double **ctr, int *per,
  int *nnbor, int **nbori, int **nborp
);

/*
 * Search hyperoctree.
 *
 * Additional arguments:
 *   NLVL  - tree depth     (input)
 *   NNODE - number of nodes (input)
 */
extern void hypoct_search(
  char *elem, int *d, int *n, double *x, double *siz, int *mlvl, int *nlvl,
  int *nnode, int **lvlx, int **nodex, int **chldp, double **l, double **ctr,
  int *trav
);