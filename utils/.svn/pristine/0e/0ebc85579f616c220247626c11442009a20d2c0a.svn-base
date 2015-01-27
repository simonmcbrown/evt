/* test prog to interface with R - testing malloc */

#include <R.h>

void test_malloc(int *n, double **data1)
{
  int 		i;
  double	*d1;
  
	(*data1) = (double *) malloc(*n * sizeof(double));
	
	d1 = (*data1);
	
	for (i=0; i<*n; i++)
        {
        *(d1+i) = 1.0*i;
		Rprintf("%i %lf \n",i, *(d1+i));
        }

  
  
}
