#include <stdio.h>
#include <math.h>
#include <strings.h>
#include <malloc.h>
#include "pp.h"

#define	ABS(a)	(((a)>(-a))? (a):(-a))

/* read n_fields from n_files and write out sequetially to outname.  n_fields
has to be the same for each file
 22/12/99	Simon Brown */

main(argc, argv)
int	argc;
char	*argv[];

{
	FILE 		*fopen(),*fin, *fout, *fclim;
	char   		fname[256],outname[256];
	pp_struct	*pp1;
	int			i,j,k;
	int			n_fields, n_files;
	int			count,nrow,npt;
	
	void		c_inquire_pp_file(char* fname, int* count, int* nrow, int* npt);

	if (argc < 2)
		{
		fprintf(stderr,"Usage: test_read in_file\n");
		}

   	strcpy(fname,argv[1]);

	c_inquire_pp_file(fname,&count,&nrow,&npt);

	fprintf(stderr,"%d fields read %d %d\n",count,nrow,npt);

	fclose(fin);
	

}
