/*******************************************************************************
 * Copyright (C) 2013-2015 Kenneth L. Ho
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
 * routine, with arguments of the same name playing the same role. Array outputs
 * are returned in Fortran order; see the Fortran source for more details.
 */

/*
 * Build hyperoctree.
 */
extern void hypoct_c_build(
  char *adap, char *elem, int *d, int *n, double *x, double *siz, int *occ,
  int *lvlmax, double *ext, int **lvlx, double *root, int *xi, int **xp,
  int **nodex
);

/*
 * Generate child data.
 */
extern void hypoct_c_chld(int *lvlx, int *nodex, int **chldp);

/*
 * Generate geometry data.
 */
extern void hypoct_c_geom(
  int *d, int *lvlx, double *rootx, int *nodex, double **l, double **ctr
);

/*
 * Get interaction lists.
 */
extern void hypoct_c_ilst(
  int *lvlx, int *xp, int *nodex, int *chldp, int *nborp, int *nbori,
  int **ilsti, int **ilstp
);

/*
 * Find neighbors.
 */
extern void hypoct_c_nbor(
  char *elem, int *d, int *lvlx, int *xp, int *nodex, int *chldp,
  double *l, double *ctr, int *per, int **nbori, int **nborp
);

/*
 * Search hyperoctree.
 */
extern void hypoct_c_search(
  char *elem, int *d, int *n, double *x, double *siz, int *mlvl, int *lvlx,
  int *nodex, int *chldp, double *l, double *ctr, int *trav
);