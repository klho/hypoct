/*******************************************************************************
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
  double  *x = (double *) malloc(d*n*sizeof(double)),
        *siz = (double *) malloc(  n*sizeof(double)), ext[d];
  for (i = 0; i < n; i++) {
    theta = 2*pi*i / n;
    x[2*i  ] = cos(theta);
    x[2*i+1] = sin(theta);
    siz[i] = 0;
  }
  for (i = 0; i < d; i++) { ext[i] = 0; }

  // print input summary
  printf("\
Number of points:                           %8i\n\
----------------------------------------------------\n",
n
);

  // build tree
  printf("Building tree...             ");
  char adap = 'a', intr = 'p';
  int occ = 20, lvlmax = -1, nlvl, nnode, *lvlx,
      *xi = (int *) malloc(n*sizeof(int)), *xp, *nodex;
  double rootx[d][2];
  t0 = clock();
  hypoct_build(&adap, &intr, &d, &n, x, siz, &occ, &lvlmax, ext,
               &nlvl, &nnode, &lvlx, *rootx, xi, &xp, &nodex);
  t = clock();
  mb = 1e-6*(sizeof(int)*(2*(nlvl + 2) + n + 3*nnode + 1) +
             sizeof(double)*(2*d));
  printf(fmt, (double)(t - t0) / CLOCKS_PER_SEC, mb);

  // generate child data
  printf("Generating child data...     ");
  int *chldp;
  t0 = clock();
  hypoct_chld(&nlvl, &nnode, &lvlx, &nodex, &chldp);
  t = clock();
  mb = 1e-6*sizeof(int)*(nnode + 1);
  printf(fmt, (double)(t - t0) / CLOCKS_PER_SEC, mb);

  // generate geometry data
  printf("Generating geometry data...  ");
  double *l, *ctr;
  t0 = clock();
  hypoct_geom(&d, &nlvl, &nnode, &lvlx, *rootx, &nodex, &l, &ctr);
  t = clock();
  mb = 1e-6*sizeof(double)*(2*nlvl + 2*nnode);
  printf(fmt, (double)(t - t0) / CLOCKS_PER_SEC, mb);

  // find neighbors
  printf("Finding neighbors...         ");
  int per[d], nnbor, *nborp, *nbori;
  for (i = 0; i < d; i++) { per[i] = 0; }
  t0 = clock();
  hypoct_nbor(&d, &nlvl, &nnode, &lvlx, &xp, &nodex, &chldp, per,
              &nnbor, &nbori, &nborp);
  t = clock();
  mb = 1e-6*sizeof(int)*(nnode + 1 + nnbor);
  printf(fmt, (double)(t - t0) / CLOCKS_PER_SEC, mb);

  // get interaction lists
  printf("Getting interaction lists... ");
  int nilst, *ilstp, *ilsti;
  t0 = clock();
  hypoct_ilst(&nlvl, &nnode, &lvlx, &nodex, &chldp, &nnbor, &nbori, &nborp,
              &nilst, &ilsti, &ilstp);
  t = clock();
  mb = 1e-6*sizeof(int)*(nnode + 1 + nilst);
  printf(fmt, (double)(t - t0) / CLOCKS_PER_SEC, mb);

  // search tree
  printf("Searching tree...            ");
  int *trav = (int *) malloc(n*(nlvl + 1)*sizeof(int));
  t0 = clock();
  hypoct_search(&d, &n, x, &nlvl, &nlvl, &nnode, &lvlx, *rootx, &nodex, &chldp,
                &ctr, trav);
  t = clock();
  mb = 1e-6*sizeof(int)*(n*(nlvl + 1));
  printf(fmt, (double)(t - t0) / CLOCKS_PER_SEC, mb);

  // print output summary
  printf("\
----------------------------------------------------\n\
Tree depth:                                 %8i\n\
Number of nodes:                            %8i\n\
Total number of neighbors:                  %8i\n\
Total number of nodes in interaction lists: %8i\n",
nlvl, nnode, nborp[nnode], ilstp[nnode]
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

  return 0;
}