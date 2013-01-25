/******************************************************************************
 * Copyright (C) 2013 Kenneth L. Ho
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

// Header file for C interface to HYPOCT

extern void hypoct_build(
  int *d, int *n, double x[], int *occ, int *nlvl, int *nnode, int **lvlx,
  double rootx[], int xi[], int **nodex
);

extern void hypoct_buildx(
  char *adap, char *intr, int *d, int *n, double x[], double siz[], int *occ,
  int *lvlmax, double ext[], int *nlvl, int *nnode, int **lvlx, double rootx[],
  int xi[], int **nodex
);

extern void hypoct_chld(
  int *nlvl, int *nnode, int **lvlx, int **nodex, int **chldp
);

extern void hypoct_geom(
  int *d, int *nlvl, int *nnode, int **lvlx, double rootx[], int **nodex,
  double **l, double **ctr
);

extern void hypoct_ilst(
  int *nlvl, int *nnode, int **lvlx, int **nodex, int **chldp, int *nnbor,
  int **nborp, int **nbori, int *nilst, int **ilstp, int **ilsti
);

extern void hypoct_nbor(
  int *d, int *nlvl, int *nnode, int **lvlx, int **nodex, int *nnbor,
  int **nborp, int **nbori
);

extern void hypoct_nborx(
  int *d, int *nlvl, int *nnode, int **lvlx, int **nodex, int **chldp,
  int per[], int *nnbor, int **nborp, int **nbori
);