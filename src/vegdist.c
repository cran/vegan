/*
 * Distance measures for community ecologists.  The measures here were
 * recommended by Peter Minchin, since they have a good rank-order
 * relation with gradient distance.  The standard distances are found
 * in standard R library mva in function dist (distance.c).
 *
 * The calling program defines the index by a number (internally and
 * factually). Extra care is needed to get the numbers right there
 * above when calling the code.
 *
 * Number "99" is an extra case: It is not for vegdist.R, but for
 * something simpler.
 *
 * (C) 2001, Jari Oksanen, Your are free to use this code if you accept GPL2.
 *
 * Oct 2003: Added Morisita, Horn-Morisita, "Jaccard", and Mountford.
 */


/* Standard R headers */

#include <R.h>
#include <Rmath.h>
#include <math.h>


/* Indices */

#define MANHATTAN 1
#define EUCLIDEAN 2
#define CANBERRA 3
#define BRAY 4
#define KULCZYNSKI 5
#define GOWER 6
#define MORISITA 7
#define HORN 8
#define MOUNTFORD 9
#define JACCARD 10
#define MILLAR 11
#define NOSHARED 99

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

/* Gower is like Manhattan, but data were standardized to range 0..1
 * for rows before call and dist is divided by the number of non-zero
 * pairs.
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

/* Jaccard = (2 * Bray)/(1 + Bray). If Jaccard is requested, Bray is
 * calculated in this function and it is left as the task of the
 * caller to translate this into Jaccard. Actually, Jaccard is
 * redundant, but since people ask for Jaccard, they get it.
 */

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
     if (dist < 0)
	  dist = 0;
     return dist;
}

double veg_morisita(double *x, int nr, int nc, int i1, int i2)
{
     double sim, dist, t1, t2, tlam1, tlam2;
     int count, j;

     t1 = 0.0;
     t2 = 0.0;
     count = 0;
     sim = 0.0;
     tlam1 = 0.0;
     tlam2 = 0.0;
     for (j=0; j<nc; j++) {
	  if (R_FINITE(x[i1]) && R_FINITE(x[i2])) {
	       sim += x[i1]*x[i2];
	       t1 += x[i1];
	       t2 += x[i2];
	       tlam1 += x[i1]*(x[i1] - 1);
	       tlam2 += x[i2]*(x[i2] - 1);
	       count++;
	  }
	  i1 += nr;
	  i2 += nr;
     }
     if (count==0) return NA_REAL;
     dist = 1 - 2*sim/(tlam1/t1/(t1-1) + tlam2/t2/(t2-1))/t1/t2;
     if (dist < 0)
	  dist = 0;
     return dist;
}

double veg_horn(double *x, int nr, int nc, int i1, int i2)
{
     double sim, dist,  t1, t2, sq1, sq2;
     int count, j;

     t1 = 0.0;
     t2 = 0.0;
     count = 0;
     sim = 0.0;
     sq1 = 0.0;
     sq2 = 0.0;
     for (j=0; j<nc; j++) {
	  if (R_FINITE(x[i1]) && R_FINITE(x[i2])) {
	       sim += x[i1]*x[i2];
	       t1 += x[i1];
	       t2 += x[i2];
	       sq1 += x[i1]*x[i1];
	       sq2 += x[i2]*x[i2];
	       count++;
	  }
	  i1 += nr;
	  i2 += nr;
     }
     if (count==0) return NA_REAL;
     dist = 1 - 2*sim/(sq1/t1/t1 + sq2/t2/t2)/t1/t2;
     if (dist < 0)
	  dist = 0;
     return dist;
}


/* Mountford index theta is defined as the root of an exponential
 * equation (function mount_fun below). The value of theta is found
 * using Newton method (derivatives in mount_der) in veg_mountford.
 * The result is divided by log(2) to put dissimilarities into
 * conventional range 0...1.
 */

#define MAXIT 20
#define EPS 1e-12
#define TOL 1e-5

double mount_fun(double theta, double j, double a, double b) 
{
     return(exp(theta*a) + exp(theta*b) - exp(theta*(a+b-j)) - 1);
}

double mount_der(double theta, double j, double a, double b) 
{
     return(a*exp(theta*a) + b*exp(theta*b) - (a+b-j)*exp(theta*(a+b-j)));
}

double veg_mountford(double *x, int nr, int nc, int i1, int i2)
{
     double dist, oldist, A,  B, J;
     int sim, t1, t2, j, count;
     
     sim = 0;
     t1 = 0;
     t2 = 0;
     count = 0;
     for (j = 0; j < nc; j++) {
	  if (R_FINITE(x[i1]) && R_FINITE(x[i2])) {
	       if (x[i1] > 0.0 && x[i2] > 0.0)
		    sim++;
	       if (x[i1] > 0)
		    t1++;
	       if (x[i2] > 0)
		    t2++;
	       count++;
	  }
	  i1 += nr;
	  i2 += nr;
     }
     if (count == 0) return NA_REAL;
     if (sim == 0)
	  dist = 0;
     else if (sim == t1 || sim == t2)
	  dist = M_LN2;
     else {
	  J = (double)(sim);
	  A = (double)(t1);
	  B = (double)(t2);
	  dist = 2*J/(2*A*B - (A+B)*J);
	  for (j = 0; j < MAXIT; j++) {
	       oldist = dist;
	       dist -= mount_fun(dist, J, A, B)/mount_der(dist, J, A, B);
	       if(fabs(oldist - dist)/oldist < TOL || fabs(oldist - dist) < EPS) 
		    break;
	  }
     }
     return 1 - dist/M_LN2;
}

#undef MAXIT
#undef EPS
#undef TOL

/* "Millar dissimilarity" is unpublished.  I found this in the lecture
 * notes of Marti Anderson over the internet, and she attributes this
 * idea to her colleague Russell Millar.  The index is basically
 * binomial deviance under H0 that species are equally common in the
 * two compared communities.  This could be easily generalized over
 * to, say, Poisson case. 
 */

double veg_millar(double *x, int nr, int nc, int i1, int i2)
{
     double dist, t1, t2, nk, lognk;
     int count, j;
  
     count = 0;
     dist = 0;
     for (j=0; j<nc; j++, i1 += nr, i2 += nr) {
	  if (R_FINITE(x[i1]) && R_FINITE(x[i2])) {
	       nk = x[i1] + x[i2];
	       if (nk == 0) continue;
	       lognk = log(nk);
	       t1 = (x[i1] > 0) ? x[i1] * (log(x[i1]) - lognk) : 0;
	       t2 = (x[i2] > 0) ? x[i2] * (log(x[i2]) - lognk) : 0;
	       dist += (t1 + t2 + nk * M_LN2)/nk;
	       count++;
	  }
     }
     if (count==0) return NA_REAL;
     return dist;
}


/* veg_noshared is not a proper dissimilarity index, but a pretty
 * useless helper function. It returns 1 when there are no shared
 * species, and 0 if two sites have at least one shared species.
 */

double veg_noshared(double *x, int nr, int nc, int i1, int i2)
{
     double dist;
     int j, count;
     dist = 1;
     count = 0;
     for (j = 0; j<nc; j++) {
	  if (R_FINITE(x[i1]) && R_FINITE(x[i2])) {
	       count++;
	       if (x[i1] > 0 && x[i2] > 0) {
		    dist = 0;
		    break;
	       }
	  }
	  i1 += nr;
	  i2 += nr;
     }
     if (count == 0) return NA_REAL;
     return(dist);
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
     case JACCARD:
	  distfun = veg_bray;
	  break;
     case KULCZYNSKI:
	  distfun = veg_kulczynski;
	  break;
     case GOWER:
	  distfun = veg_gower;
	  break;
     case MORISITA:
	  distfun = veg_morisita;
	  break;
     case HORN:
	  distfun = veg_horn;
	  break;
     case MOUNTFORD:
	  distfun = veg_mountford;
	  break;
     case MILLAR:
	  distfun = veg_millar;
	  break;
     case NOSHARED:
	  distfun = veg_noshared;
	  break;	 
     default:
	  error("Unknown distance in the internal C function");
     }

     dc = (*diag) ? 0 : 1;
     ij = 0;
     for (j=0; j <= *nr; j++)
	  for (i=j+dc; i < *nr; i++) {
	       d[ij++] = distfun(x, *nr, *nc, i, j);
	  }
}



