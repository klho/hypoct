/*
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
 */

// Build quadtree on uniformly spaced points on the unit circle.

#include <hypoct.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {

  // local variables
  double t;
  clock_t t0;

  /****************************************************************************/
  /* build tree                                                               */
  /****************************************************************************/
  // build variables
  char adap = 'a', intr = 'p';
  int d = 2, n = pow(2, 20), occ = 20, lvlmax = -1, nlvl, nnode,
      *lvlx, *xi, *nodex;
  double *x, *siz, ext[d], rootx[d][2];
  int i;
  double pi = 4*atan(1), theta;

  // set inputs (allocate on heap for large arrays)
    x = (double *) malloc(d*n*sizeof(double));
  siz = (double *) malloc(  n*sizeof(double));
   xi = (   int *) malloc(  n*sizeof(   int));
  for (i = 0; i < n; i++) {
    theta = 2*pi*i / n;
    x[2*i  ] = cos(theta);
    x[2*i+1] = sin(theta);
    siz[i] = 0;
  }
  for (i = 0; i < d; i++) { ext[i] = 0; }

  // build tree
  printf("Building tree...            ");
  t0 = clock();
  hypoct_buildx(&adap, &intr, &d, &n, x, siz, &occ, &lvlmax, ext,
                &nlvl, &nnode, &lvlx, *rootx, xi, &nodex);
  t = ((double)clock() - t0) / CLOCKS_PER_SEC;
  printf("%12.4e (s)\n", t);
  /****************************************************************************/

  // generate geometry data
  double *l, *ctr;
  printf("Generating geometry data... ");
  t0 = clock();
  hypoct_geom(&d, &nlvl, &nnode, &lvlx, *rootx, &nodex, &l, &ctr);
  t = ((double)clock() - t0) / CLOCKS_PER_SEC;
  printf("%12.4e (s)\n", t);

  // generate child data
  int *chldp;
  printf("Generating child data...    ");
  t0 = clock();
  hypoct_chld(&nlvl, &nnode, &lvlx, &nodex, &chldp);
  t = ((double)clock() - t0) / CLOCKS_PER_SEC;
  printf("%12.4e (s)\n", t);

  // find neighbors
  int per[d], nnbor, *nborp, *nbori;
  for (i = 0; i < d; i++) { per[i] = 0; }
  printf("Finding neighbors...        ");
  t0 = clock();
  hypoct_nborx(&d, &nlvl, &nnode, &lvlx, &nodex, &chldp, per, &nnbor, &nborp,
               &nbori);
  t = ((double)clock() - t0) / CLOCKS_PER_SEC;
  printf("%12.4e (s)\n", t);

  // get interaction list
  int nilst, *ilstp, *ilsti;
  printf("Getting interaction lists...");
  t0 = clock();
  hypoct_ilst(&nlvl, &nnode, &lvlx, &nodex, &chldp, &nnbor, &nborp, &nbori,
              &nilst, &ilstp, &ilsti);
  t = ((double)clock() - t0) / CLOCKS_PER_SEC;
  printf("%12.4e (s)\n", t);

  // print summary
  printf("\
----------------------------------------------------\n\
tree depth:                                 %8i\n\
number of nodes:                            %8i\n\
total number of neighbors:                  %8i\n\
total number of nodes in interaction lists: %8i\n",
nlvl, nnode, nborp[nnode], ilstp[nnode]
);

  // free memory
  free(    x);
  free(  siz);
  free( lvlx);
  free(nodex);
  free(    l);
  free(  ctr);
  free(chldp);
  free(nborp);
  free(nbori);
  free(ilstp);
  free(ilsti);

  return 0;
}