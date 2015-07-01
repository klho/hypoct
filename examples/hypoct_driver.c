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

#include <hypoct.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

/* Build quadtree on uniformly spaced points on the unit circle. */
int main() {

  // local variables
  char *fmt = "%10.4e (s) / %6.2f (MB)\n";
  int i;
  double pi = 4*atan(1), theta, mb;
  clock_t t, t0;

  // set inputs (allocate on heap for large arrays)
  int d = 2, n = pow(2, 20);
  double  *x = (double *)malloc(d*n*sizeof(double)),
        *siz = (double *)malloc(  n*sizeof(double)), ext[d];
  for (i = 0; i < n; ++i) {
    theta = 2*pi*i / n;
    x[2*i  ] = cos(theta);
    x[2*i+1] = sin(theta);
    siz[i] = 0;
  }
  for (i = 0; i < d; ++i) { ext[i] = 0; }

  // print input summary
  printf("\
Number of points:                           %8i\n\
----------------------------------------------------\n",
n
);

  // build tree
  printf("Building tree...             ");
  char adap = 'a', elem = 'p';
  int occ = 16, lvlmax = -1, *lvlx,
      *xi = (int *)malloc(n*sizeof(int)), *xp, *nodex;
  double rootx[2*d];
  t0 = clock();
  hypoct_c_build(&adap, &elem, &d, &n, x, siz, &occ, &lvlmax, ext,
                 &lvlx, rootx, xi, &xp, &nodex);
  t = clock();
  int nlvl = lvlx[1], nnode = lvlx[2*(nlvl + 1)];
  mb = 1e-6*(sizeof(int)*(2*(nlvl + 2) + n + 3*nnode + 1) +
             sizeof(double)*(2*d));
  printf(fmt, (double)(t - t0) / CLOCKS_PER_SEC, mb);

  // generate child data
  printf("Generating child data...     ");
  int *chldp;
  t0 = clock();
  hypoct_c_chld(lvlx, nodex, &chldp);
  t = clock();
  mb = 1e-6*sizeof(int)*(nnode + 1);
  printf(fmt, (double)(t - t0) / CLOCKS_PER_SEC, mb);

  // generate geometry data
  printf("Generating geometry data...  ");
  double *l, *ctr;
  t0 = clock();
  hypoct_c_geom(&d, lvlx, rootx, nodex, &l, &ctr);
  t = clock();
  mb = 1e-6*sizeof(double)*(2*nlvl + 2*nnode);
  printf(fmt, (double)(t - t0) / CLOCKS_PER_SEC, mb);

  // find neighbors
  printf("Finding neighbors...         ");
  int per[d], *nborp, *nbori;
  for (i = 0; i < d; ++i) { per[i] = 0; }
  t0 = clock();
  hypoct_c_nbor(&elem, &d, lvlx, xp, nodex, chldp, l, ctr, per, &nbori, &nborp);
  t = clock();
  int nnbor = nborp[nnode];
  mb = 1e-6*sizeof(int)*(nnode + 1 + nnbor);
  printf(fmt, (double)(t - t0) / CLOCKS_PER_SEC, mb);

  // get interaction lists
  printf("Getting interaction lists... ");
  int *ilstp, *ilsti;
  t0 = clock();
  hypoct_c_ilst(lvlx, xp, nodex, chldp, nbori, nborp, &ilsti, &ilstp);
  t = clock();
  int nilst = ilstp[nnode];
  mb = 1e-6*sizeof(int)*(nnode + 1 + nilst);
  printf(fmt, (double)(t - t0) / CLOCKS_PER_SEC, mb);

  // search tree
  printf("Searching tree...            ");
  int m = pow(2, 16), *trav = (int *) malloc(m*(nlvl + 1)*sizeof(int));
  double *y = (double *) malloc(d*m*sizeof(double));
  for (i = 0; i < m; ++i) {
    y[2*i  ] = 2*((double)rand() / (double)RAND_MAX) - 1;
    y[2*i+1] = 2*((double)rand() / (double)RAND_MAX) - 1;
  }
  t0 = clock();
  hypoct_c_search(&elem, &d, &m, y, siz, &nlvl, lvlx, nodex, chldp, l, ctr, trav);
  t = clock();
  mb = 1e-6*sizeof(int)*(m*(nlvl + 1));
  printf(fmt, (double)(t - t0) / CLOCKS_PER_SEC, mb);

  // print output summary
  printf("\
----------------------------------------------------\n\
Tree depth:                                 %8i\n\
Number of nodes:                            %8i\n\
Total number of neighbors:                  %8i\n\
Total number of nodes in interaction lists: %8i\n",
nlvl, nnode, nnbor, nilst
);

  // free memory
  free(    x);
  free(  siz);
  free( lvlx);
  free(   xi);
  free(   xp);
  free(nodex);
  free(chldp);
  free(    l);
  free(  ctr);
  free(nborp);
  free(nbori);
  free(ilstp);
  free(ilsti);
  free( trav);
  free(    y);

  return 0;
}