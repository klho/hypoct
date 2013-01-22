/*
   Copyright (C) 2013 Kenneth L. Ho

   This program is free software: you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the Free
   Software Foundation, either version 3 of the License, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   You should have received a copy of the GNU General Public License along with
   this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

extern void hypoct_build(int *d, int *n, double x[], int *occ,
                         int **lvlx, double rootx[], int xi[], int **nodex);

extern void hypoct_buildx(char *adap, char *intr, int *d, int *n, double x[],
                          double siz[], int *occ, int *lvlmax, double ext[],
                          int **lvlx, double rootx[], int xi[], int **nodex);

int main() {
  char adap = 'a', intr = 'p';
  int d = 2, n = 100, occ = 1, xi[n];
  double x[n][d], rootx[d][2];
  int *lvlx, *nodex;

  int lvlmax = -1;
  double siz[n], ext[d];

  int i;
  double pi = 4*atan(1), theta;

  for (i = 0; i < n; i++) {
    theta = 2*pi*i / n;
    x[i][0] = cos(theta);
    x[i][1] = sin(theta);
    siz[i] = 0;
  }

  for (i = 0; i < d; i++) {
    ext[i] = 0;
  }

//   hypoct_build(&d, &n, *x, &occ, &lvlx, *rootx, xi, &nodex);

  hypoct_buildx(&adap, &intr, &d, &n, *x, siz, &occ, &lvlmax, ext,
                &lvlx, *rootx, xi, &nodex);

  for (i = 0; i < n; i++) {
    printf("%8i\n", xi[i]);
  }

  free(lvlx);
  free(nodex);

  printf("freed\n");

  return 0;
}