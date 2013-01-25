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

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <hypoct.h>

// extern void hypoct_build(int *d, int *n, double x[], int *occ,
//                          int **lvlx, double rootx[], int xi[], int **nodex);
// 
// extern void hypoct_buildx(char *adap, char *intr, int *d, int *n, double x[],
//                           double siz[], int *occ, int *lvlmax, double ext[],
//                           int **lvlx, double rootx[], int xi[], int **nodex);

int main() {
  char adap = 'a', intr = 'p';
  int d = 2, n = 100, occ = 1, xi[n], lvlmax = -1, nlvl, nnode, *lvlx, *nodex;
  double x[n][d], siz[n], ext[d], rootx[d][2];

  double *l, *ctr;

  int *chldp;

  int per[d], nnbor, *nborp, *nbori;

  int nilst, *ilstp, *ilsti;

  int i;
  double pi = 4*atan(1), theta;

  for (i = 0; i < n; i++) {
    theta = 2*pi*i / n;
    x[i][0] = cos(theta);
    x[i][1] = sin(theta);
    siz[i] = 0;
  }

  for (i = 0; i < d; i++) { ext[i] = 0; }

//   hypoct_build(&d, &n, *x, &occ, &nlvl, &nnode, &lvlx, *rootx, xi, &nodex);

  hypoct_buildx(&adap, &intr, &d, &n, *x, siz, &occ, &lvlmax, ext,
                &nlvl, &nnode, &lvlx, *rootx, xi, &nodex);

//   printf("%i\n", nlvl);
//   printf("%i\n", nnode);

//   for (i = 0; i < n; i++) { printf("%i\n", xi[i]); }

  hypoct_geom(&d, &nlvl, &nnode, &lvlx, *rootx, &nodex, &l, &ctr);

//   printf("%e\n", l[0]);
//   printf("%e\n", l[1]);
//   printf("%e\n", l[2]);
//   printf("%e\n", l[3]);

  hypoct_chld(&nlvl, &nnode, &lvlx, &nodex, &chldp);

//   for (i = 0; i < nnode+1; i++) { printf("%8i\n", chldp[i]); }

//   hypoct_nbor(&d, &nlvl, &nnode, &lvlx, &nodex, &nnbor, &nborp, &nbori);

  for (i = 0; i < d; i++) { per[i] = 0; }
  hypoct_nborx(&d, &nlvl, &nnode, &lvlx, &nodex, &chldp, per, &nnbor, &nborp,
               &nbori);

//   for (i = 0; i < nnode+1; i++) { printf("%8i\n", nborp[i]); }
//   for (i = 0; i < nnbor; i++) { printf("%8i\n", nbori[i]); }

  hypoct_ilst(&nlvl, &nnode, &lvlx, &nodex, &chldp, &nnbor, &nborp, &nbori,
              &nilst, &ilstp, &ilsti);

//   for (i = 0; i < nnode+1; i++) { printf("%8i\n", ilstp[i]); }
  for (i = 0; i < nilst; i++) { printf("%8i\n", ilsti[i]); }

  free(lvlx);
  free(nodex);

//   printf("freed\n");

  return 0;
}