/*
 * Distance measures for vegetation sciences.
 * The measures here were recommended by Peter Minchin, since
 * they have a good rank-order relation with gradient distance.
 * The standard distances are found in standard R library 
 * mva in function dist (distance.c).
 *
 * Author: Jari Oksanen
 */


/* Standard R headers */

#include <R.h>
#include <math.h>

/* Indices */

#define MANHATTAN 1
#define EUCLIDEAN 2
#define CANBERRA 3
#define BRAY 4
#define KULCZYNSKI 5
#define GOWER 6

/* Distance functions */

double veg_manhattan(double *x, int nr, int nc, int i1, int i2)
{
     double dist;
     int count, j;
  
     dist = 0.0;
     count = 0;
     for (j=0; j<nc; j++) {
	  if (R_FINITE(x[i1]) && R_FINITE(x[i2])) {
	       dist += fabs( x[i1] - x[i2] );
	       count++;
	  }
	  i1 += nr;
	  i2 += nr;
     }
     if (count == 0) dist = NA_REAL;
     return dist;
}

/* Gower is like Manhattan, but data were standardized to 
   range 0..1 for rows before call and dist is divided
   by the number of non-zero pairs 
*/

double veg_gower(double *x, int nr, int nc, int i1, int i2)
{
     double dist;
     int count, j;
  
     dist = 0.0;
     count = 0;
     for (j=0; j<nc; j++) {
	  if (R_FINITE(x[i1]) && R_FINITE(x[i2])) {
	       dist += fabs( x[i1] - x[i2] );
	       count++;
	  }
	  i1 += nr;
	  i2 += nr;
     }
     if (count == 0) dist = NA_REAL;
     dist /= (double) count;
     return dist;
}

double veg_euclidean(double *x, int nr, int nc, int i1, int i2)
{
     double dist, dev;
     int count, j;

     count = 0;
     dist = 0.0;
     for (j=0; j<nc; j++) {
	  if (R_FINITE(x[i1]) && R_FINITE(x[i2])) {
	       dev = x[i1] - x[i2];
	       dist += dev*dev;
	       count++;
	  }
	  i1 += nr;
	  i2 += nr;
     }
     if (count == 0) return NA_REAL;
     return sqrt(dist);
}

double veg_canberra(double *x, int nr, int nc, int i1, int i2)
{
     double numer, denom, dist;
     int count, j;

     count = 0;
     dist = 0.0;
     for (j=0; j<nc; j++) {
	  if (R_FINITE(x[i1]) && R_FINITE(x[i2])) {
	       if (x[i1] != 0 || x[i2] != 0) {
		    count++;
		    denom = x[i1] + x[i2];
		    if (denom > 0.0) {
			 numer = fabs(x[i1] - x[i2]);
			 dist += numer/denom;
		    }
		    else {
			 dist += R_PosInf;
		    }
	       }
	  }
	  i1 += nr;
	  i2 += nr;
     }
     if (count == 0) return NA_REAL;
     dist /= (double)count;
     return dist;
}

double veg_bray(double *x, int nr, int nc, int i1, int i2)
{
     double dist, total;
     int count, j;
  
     total = 0.0;
     count = 0;
     dist = 0;
     for (j=0; j<nc; j++) {
	  if (R_FINITE(x[i1]) && R_FINITE(x[i2])) {
	       dist += fabs(x[i1] - x[i2]);
	       total += x[i1] + x[i2];
	       count++;
	  }
	  i1 += nr;
	  i2 += nr;
     }
     if (count==0) return NA_REAL;
     dist /= total;
     return dist;
}

double veg_kulczynski(double *x, int nr, int nc, int i1, int i2)
{
     double sim, dist, t1, t2;
     int count, j;

     t1 = 0.0;
     t2 = 0.0;
     count = 0;
     sim = 0.0;
     for (j=0; j<nc; j++) {
	  if (R_FINITE(x[i1]) && R_FINITE(x[i2])) {
	       sim += (x[i1] < x[i2]) ? x[i1] : x[i2] ;
	       t1 += x[i1];
	       t2 += x[i2];
	       count++;
	  }
	  i1 += nr;
	  i2 += nr;
     }
     if (count==0) return NA_REAL;
     dist = 1 - sim/t1/2 - sim/t2/2;
     return dist;
}

/* Driver */

static double (*distfun)(double*, int, int, int, int);

void veg_distance(double *x, int *nr, int *nc, double *d, int *diag, int *method)
{
     int dc, i, j, ij;
     switch(*method) {
     case MANHATTAN:
	  distfun = veg_manhattan;
	  break;
     case EUCLIDEAN:
	  distfun = veg_euclidean;
	  break;
     case CANBERRA:
	  distfun = veg_canberra;
	  break;
     case BRAY:
	  distfun = veg_bray;
	  break;
     case KULCZYNSKI:
	  distfun = veg_kulczynski;
	  break;
     case GOWER:
	  distfun = veg_gower;
	  break;
     default:
	  error("veg_distance(): unknown distance");
     }

     dc = (*diag) ? 0 : 1;
     ij = 0;
     for (j=0; j <= *nr; j++)
	  for (i=j+dc; i < *nr; i++) {
	       d[ij++] = distfun(x, *nr, *nc, i, j);
	  }
}



