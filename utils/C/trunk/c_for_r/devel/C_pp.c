#include <stdio.h>
#include <math.h>
#include <strings.h>
#include <malloc.h>
  
#include <netinet/in.h>

#include "pp.h"

#include <R.h>

#define	ABS(a)	(((a)>(-a))? (a):(-a))

#ifndef WRITEPP
#define WRITEPP    (int)1
#endif

#ifndef READPP
#define READPP   (int)0
#endif

void swap_4byte(int *buffer, int number_of_swaps)
{

   int *temp;
   int swap_loop;

   for (swap_loop = 0, temp = buffer; swap_loop < number_of_swaps; swap_loop++, temp++)
   {
      *temp = ((*temp & 0x000000ff) << 24) |
              ((*temp & 0x0000ff00) << 8) |
              ((*temp & 0x00ff0000) >> 8) |
              ((*temp & 0xff000000) >> 24);
   }

}

/***********************************************************************/
void
byte_swap_pp(pp_ptr,readwrite)
	pp_struct 	*pp_ptr;
	int			readwrite;
{
	int	i,lblrec;
	pp64_header *dumm;

	/*if readwrite is WRITEPP then need to store data record length before byte swap  */
	if (readwrite == WRITEPP) lblrec= pp_ptr->head.LBLREC; else lblrec= ntohl(pp_ptr->head.LBLREC);
	
	pp_ptr->fortran_buffer1 = ntohl(pp_ptr->fortran_buffer1);
	
	dumm = &(pp_ptr->head);
			
	for (i=0; i<45; i++) /* swap header */
			{
			*( (int *)dumm +i)= ntohl(*( (int *)dumm +i));	
 			}
	
	swap_4byte((int *)&(pp_ptr->head.BRSVD[0]),19);
	
	pp_ptr->fortran_buffer2 = ntohl(pp_ptr->fortran_buffer2);
	pp_ptr->fortran_buffer3 = ntohl(pp_ptr->fortran_buffer3);			
			
	/* swap data */
	if ( (pp_ptr)->data != NULL)
		{
		swap_4byte((int *)(pp_ptr)->data,lblrec);
		/*		
		for (i=0; i<pp_ptr->head.LBLREC; i++) 
			{
 			*(((pp_ptr)->data) +i) = ntohl(*(((pp_ptr)->data) +i));
 			}
		*/
		}
			
	pp_ptr->fortran_buffer4 = ntohl(pp_ptr->fortran_buffer4);
			
}	


/***********************************************************************/
void
c_inquire_pp_file(fname,count,nrow,npt)
char**		fname;
int*		count;
int*		nrow;
int*		npt;

/*      reads all pp fields:  header, data and buffer bytes
        returns number of successfully read fields, number of rows and points 
		through the pointers in the argument list
*/
	
{
	
	int			i,j, n;
	FILE 		*fopen(),*fin;
	pp_struct 	*pp_ptr;
	
	/* Rprintf("Trying to Opening %s\n",(*fname));
	*/
	fin = fopen((*fname),"r");
	if (fin == NULL)
      {
      Rprintf("cant open file %s\n",(*fname));
      Rprintf("make sure there are no environment variables in strings\n");
	  error("help2");		
      }
	
	pp_ptr = (pp_struct *) Calloc(sizeof(pp_struct)*1,pp_struct);
	if(pp_ptr == NULL){
          Rprintf("No memory C\n");
          error("No memory C\n");
      	}
	
	fread(&(pp_ptr->fortran_buffer1), 1*sizeof(int),           1 ,fin);

	n      = 0;
	while (!(feof(fin)))
		{
		fread(&(pp_ptr->head),           64*sizeof(int),           1, fin);
		fread(&(pp_ptr->fortran_buffer2), 1*sizeof(int),           1 ,fin);
		fread(&(pp_ptr->fortran_buffer3), 1*sizeof(int),           1 ,fin);

		fseek(fin, ntohl(pp_ptr->head.LBLREC) * sizeof(float), SEEK_CUR);

		fread(&(pp_ptr->fortran_buffer4), 1*sizeof(int),           1 ,fin);

/*		byte_swap_pp(pp_ptr,READPP);
*/
		n      += 1;

		fread(&(pp_ptr->fortran_buffer1), 1*sizeof(int),           1 ,fin);
	
		}
	
	*count = n;
/*	*nrow  = pp_ptr->head.LBROW;
	*npt   = pp_ptr->head.LBNPT;
*/
	*nrow  = ntohl(pp_ptr->head.LBROW);
	*npt   = ntohl(pp_ptr->head.LBNPT);

	fclose(fin);
	Free(pp_ptr);
	
	/*Rprintf("%d fields read, %d rows %d points\n",*count,*nrow,*npt);
	*/

}

													
/***********************************************************************/
void
c_read_pp_file(char **fname,
					int *count,
					int *idxfields,
					int *LBYR,
					int *LBMON,
					int *LBDAT,
					int *LBHR,
					int *LBMIN,
					int *LBDAY,
					int *LBYRD,
					int *LBMOND,
					int *LBDATD,
					int *LBHRD,
					int *LBMIND,
					int *LBDAYD,
					int *LBTIM,
					int *LBFT,
					int *LBLREC,
					int *LBCODE,
					int *LBHEM,
					int *LBROW,
					int *LBNPT,
					int *LBEXT,
					int *LBPACK,
					int *LBREL,
					int *LBFC,
					int *LBCFC,
					int *LBPROC,
					int *LBVC,
					int *LBRVC,
					int *LBEXP,
					int *LBEGIN,
					int *LBNREC,
					int *LBPROJ,
					int *LBTYP,
					int *LBLEV,
					int *LBRSVD,					
					int *LBSRCE,
					int *LBUSER,
					float *BRSVD,					
					float *BDATUM,
					float *BACC,
					float *BLEV,
					float *BRLEV,
					float *BHLEV,
					float *BHRLEV,
					float *BPLAT,
					float *BPLON,
					float *BGOR,
					float *BZY,
					float *BDY,
					float *BZX,
					float *BDX,
					float *BMDI,
					float *BMKS,
					float *data,
					int *BUF1,
					int *BUF2,
					int *BUF3,
					int *BUF4					
				)

{
/*      reads all pp fields:  header, data and buffer bytes
        writes all corresponding data from pp file to  
		the pointers in the argument list
*/
	
	int			i, j, n, n2;
	FILE 		*fopen(),*fin;
	pp_struct 	*pp_ptr;
	int			float_size, pp_size;
	
	pp_size    = sizeof(pp_struct);
	float_size = sizeof(float);
	
	fin = fopen((*fname),"r");
	if (fin == NULL)
      {
      Rprintf("cant open file %s\n",(*fname));
      Rprintf("make sure there are no environment variables in strings\n");
	  error("help2");		
      }
	
	  
	pp_ptr = (pp_struct *) Calloc(sizeof(pp_struct)*1,pp_struct);
	if(pp_ptr == NULL){
          Rprintf("No memory D\n");
          error("No memory D\n");
      	}
	
	fread(&(pp_ptr->fortran_buffer1), 1*sizeof(int),           1 ,fin);

	n      = 0;
	n2     = 0;
	while (!(feof(fin)))
	  {
	  if (n == (idxfields[n2]-1) )
	    {
		fread(&(pp_ptr->head),           64*sizeof(int),           1, fin);
		fread(&(pp_ptr->fortran_buffer2), 1*sizeof(int),           1 ,fin);
		fread(&(pp_ptr->fortran_buffer3), 1*sizeof(int),           1 ,fin);

		pp_ptr->data = (float *) Calloc(ntohl(pp_ptr->head.LBLREC)*sizeof(float),float);
		if(pp_ptr->data == NULL){
              Rprintf("No memory E\n");
              error("No memory E\n");
      		}
			
		fread(pp_ptr->data,             ntohl(pp_ptr->head.LBLREC)*sizeof(float), 1, fin);

		fread(&(pp_ptr->fortran_buffer4), 1*sizeof(int),           1 ,fin);		
		
		byte_swap_pp(pp_ptr,READPP);

					LBYR[n2] = pp_ptr->head.LBYR;
					LBMON[n2] = pp_ptr->head.LBMON;
					LBDAT[n2] = pp_ptr->head.LBDAT;
					LBHR[n2] = pp_ptr->head.LBHR;
					LBMIN[n2] = pp_ptr->head.LBMIN;
					LBDAY[n2] = pp_ptr->head.LBDAY;
					LBYRD[n2] = pp_ptr->head.LBYRD;
					LBMOND[n2] = pp_ptr->head.LBMOND;
					LBDATD[n2] = pp_ptr->head.LBDATD;
					LBHRD[n2] = pp_ptr->head.LBHRD;
					LBMIND[n2] = pp_ptr->head.LBMIND;
					LBDAYD[n2] = pp_ptr->head.LBDAYD;
					LBTIM[n2] = pp_ptr->head.LBTIM;
					LBFT[n2] = pp_ptr->head.LBFT;
					LBLREC[n2] = pp_ptr->head.LBLREC;
					LBCODE[n2] = pp_ptr->head.LBCODE;
					LBHEM[n2] = pp_ptr->head.LBHEM;
					LBROW[n2] = pp_ptr->head.LBROW;
					LBNPT[n2] = pp_ptr->head.LBNPT;
					LBEXT[n2] = pp_ptr->head.LBEXT;
					LBPACK[n2] = pp_ptr->head.LBPACK;
					LBREL[n2] = pp_ptr->head.LBREL;
					LBFC[n2] = pp_ptr->head.LBFC;
					LBCFC[n2] = pp_ptr->head.LBCFC;
					LBPROC[n2] = pp_ptr->head.LBPROC;
					LBVC[n2] = pp_ptr->head.LBVC;
					LBRVC[n2] = pp_ptr->head.LBRVC;
					LBEXP[n2] = pp_ptr->head.LBEXP;
					LBEGIN[n2] = pp_ptr->head.LBEGIN;
					LBNREC[n2] = pp_ptr->head.LBNREC;
					LBPROJ[n2] = pp_ptr->head.LBPROJ;
					LBTYP[n2] = pp_ptr->head.LBTYP;
					LBLEV[n2] = pp_ptr->head.LBLEV;
					LBSRCE[n2] = pp_ptr->head.LBSRCE;

					BDATUM[n2] = pp_ptr->head.BDATUM;
					BACC[n2] = pp_ptr->head.BACC;
					BLEV[n2] = pp_ptr->head.BLEV;
					BRLEV[n2] = pp_ptr->head.BRLEV;
					BHLEV[n2] = pp_ptr->head.BHLEV;
					BHRLEV[n2] = pp_ptr->head.BHRLEV;
					BPLAT[n2] = pp_ptr->head.BPLAT;
					BPLON[n2] = pp_ptr->head.BPLON;
					BGOR[n2] = pp_ptr->head.BGOR;
					BZY[n2] = pp_ptr->head.BZY;
					BDY[n2] = pp_ptr->head.BDY;
					BZX[n2] = pp_ptr->head.BZX;
					BDX[n2] = pp_ptr->head.BDX;
					BMDI[n2] = pp_ptr->head.BMDI;
					BMKS[n2] = pp_ptr->head.BMKS;
					
					BUF1[n2] = pp_ptr->fortran_buffer1;
					BUF2[n2] = pp_ptr->fortran_buffer2;
					BUF3[n2] = pp_ptr->fortran_buffer3;
					BUF4[n2] = pp_ptr->fortran_buffer4;

					for(i=0; i<=3; i++) LBRSVD[n2*4+i] = pp_ptr->head.LBRSVD[i];
					for(i=0; i<=6; i++) LBUSER[n2*7+i] = pp_ptr->head.LBUSER[i];
					for(i=0; i<=3; i++)  BRSVD[n2*4+i] = pp_ptr->head.BRSVD[i];

					for(i=0; i<=pp_ptr->head.LBLREC -1; i++)
						 data[n2*pp_ptr->head.LBLREC +i] = pp_ptr->data[i];
					Free(pp_ptr->data);
					
		n  += 1;	
		n2 += 1;
		fread(&(pp_ptr->fortran_buffer1), 1*sizeof(int),           1 ,fin);

		} else {

		fread(&(pp_ptr->head),           64*sizeof(int),           1, fin);
		fread(&(pp_ptr->fortran_buffer2), 1*sizeof(int),           1 ,fin);
		fread(&(pp_ptr->fortran_buffer3), 1*sizeof(int),           1 ,fin);

		fseek(fin, ntohl(pp_ptr->head.LBLREC) * sizeof(float), SEEK_CUR);

		fread(&(pp_ptr->fortran_buffer4), 1*sizeof(int),           1 ,fin);

/*		byte_swap_pp(pp_ptr,READPP);
*/
		n  += 1;

		fread(&(pp_ptr->fortran_buffer1), 1*sizeof(int),           1 ,fin);
			
		}
	  }
	
	*count = n2;
	fclose(fin);
	Free(pp_ptr);

}													
/***********************************************************************/
void
c_write_pp_file(char **fname,
					int *count,
					int *LBYR,
					int *LBMON,
					int *LBDAT,
					int *LBHR,
					int *LBMIN,
					int *LBDAY,
					int *LBYRD,
					int *LBMOND,
					int *LBDATD,
					int *LBHRD,
					int *LBMIND,
					int *LBDAYD,
					int *LBTIM,
					int *LBFT,
					int *LBLREC,
					int *LBCODE,
					int *LBHEM,
					int *LBROW,
					int *LBNPT,
					int *LBEXT,
					int *LBPACK,
					int *LBREL,
					int *LBFC,
					int *LBCFC,
					int *LBPROC,
					int *LBVC,
					int *LBRVC,
					int *LBEXP,
					int *LBEGIN,
					int *LBNREC,
					int *LBPROJ,
					int *LBTYP,
					int *LBLEV,
					int *LBRSVD,					
					int *LBSRCE,
					int *LBUSER,
					float *BRSVD,					
					float *BDATUM,
					float *BACC,
					float *BLEV,
					float *BRLEV,
					float *BHLEV,
					float *BHRLEV,
					float *BPLAT,
					float *BPLON,
					float *BGOR,
					float *BZY,
					float *BDY,
					float *BZX,
					float *BDX,
					float *BMDI,
					float *BMKS,
					float *data,
					int *BUF1,
					int *BUF2,
					int *BUF3,
					int *BUF4					
				)

{
/*      writes all pp fields:  header, data and buffer bytes
        writes all corresponding data from   
		the pointers in the argument list
*/
	
	int			i,j, n, nfields, temp1;
	int			fort_buf_size_header, fort_buf_size_data;
	float		*swapped_data, fa,fb,fc;
	FILE 		*fopen(),*fout;
	pp_struct 	*pp_ptr;

	fout = fopen((*fname),"w");
	if (fout == NULL)
      {
      Rprintf("cant open file %s\n",(*fname));
      Rprintf("make sure there are no environment variables in strings\n");
	  error("help3");		
      }
	 
	pp_ptr = (pp_struct *) Calloc(sizeof(pp_struct)*1,pp_struct);
	if(pp_ptr == NULL){
          Rprintf("No memory F\n");
          error("No memory F\n");
      	}
  
	nfields = *count;	

	fort_buf_size_header = ntohl(64*sizeof(int));
	fort_buf_size_data   = LBLREC[0]*sizeof(float);
	swapped_data         = (float *) Calloc(fort_buf_size_data,float);
	if(swapped_data == NULL){
          Rprintf("No memory G\n");
          error("No memory G\n");
      	}
		
	fort_buf_size_data   = ntohl(LBLREC[0]*sizeof(float));

for (n=0; n<=nfields-1; n++)
	{

	/* write fortran buffer  */
	fwrite(&(fort_buf_size_header), 1*sizeof(int),           1 ,fout);

/* put header data into header structure, swab bytes and write headder out */

pp_ptr->head.LBYR = ntohl( *(LBYR +n));
pp_ptr->head.LBMON = ntohl(*(LBMON +n));
pp_ptr->head.LBDAT = ntohl(*(LBDAT +n));
pp_ptr->head.LBHR = ntohl(*(LBHR +n));
pp_ptr->head.LBMIN = ntohl(*(LBMIN +n));
pp_ptr->head.LBDAY = ntohl(*(LBDAY +n));
pp_ptr->head.LBYRD = ntohl(*(LBYRD +n));
pp_ptr->head.LBMOND = ntohl(*(LBMOND +n));
pp_ptr->head.LBDATD = ntohl(*(LBDATD +n));
pp_ptr->head.LBHRD = ntohl(*(LBHRD +n));
pp_ptr->head.LBMIND = ntohl(*(LBMIND +n));
pp_ptr->head.LBDAYD = ntohl(*(LBDAYD +n));
pp_ptr->head.LBTIM = ntohl(*(LBTIM +n));
pp_ptr->head.LBFT = ntohl(*(LBFT +n));
pp_ptr->head.LBLREC = ntohl(*(LBLREC +n));
pp_ptr->head.LBCODE = ntohl(*(LBCODE +n));
pp_ptr->head.LBHEM = ntohl(*(LBHEM +n));
pp_ptr->head.LBROW = ntohl(*(LBROW +n));
pp_ptr->head.LBNPT = ntohl(*(LBNPT +n));
pp_ptr->head.LBEXT = ntohl(*(LBEXT +n));
pp_ptr->head.LBPACK = ntohl(*(LBPACK +n));
pp_ptr->head.LBREL = ntohl(*(LBREL +n));
pp_ptr->head.LBFC = ntohl(*(LBFC +n));
pp_ptr->head.LBCFC = ntohl(*(LBCFC +n));
pp_ptr->head.LBPROC = ntohl(*(LBPROC +n));
pp_ptr->head.LBVC = ntohl(*(LBVC +n));
pp_ptr->head.LBRVC = ntohl(*(LBRVC +n));
pp_ptr->head.LBEXP = ntohl(*(LBEXP +n));
pp_ptr->head.LBEGIN = ntohl(*(LBEGIN +n));
pp_ptr->head.LBNREC = ntohl(*(LBNREC +n));
pp_ptr->head.LBPROJ = ntohl(*(LBPROJ +n));
pp_ptr->head.LBTYP = ntohl(*(LBTYP +n));
pp_ptr->head.LBLEV = ntohl(*(LBLEV +n));
pp_ptr->head.LBRSVD[0] = ntohl(*(LBRSVD + n*4 +0));		
pp_ptr->head.LBRSVD[1] = ntohl(*(LBRSVD + n*4 +1));		
pp_ptr->head.LBRSVD[2] = ntohl(*(LBRSVD + n*4 +2));		
pp_ptr->head.LBRSVD[3] = ntohl(*(LBRSVD + n*4 +3));		
pp_ptr->head.LBSRCE = ntohl(*(LBSRCE +n));
pp_ptr->head.LBUSER[0] = ntohl(*(LBUSER + n*7 +0));
pp_ptr->head.LBUSER[1] = ntohl(*(LBUSER + n*7 +1));
pp_ptr->head.LBUSER[2] = ntohl(*(LBUSER + n*7 +2));
pp_ptr->head.LBUSER[3] = ntohl(*(LBUSER + n*7 +3));
pp_ptr->head.LBUSER[4] = ntohl(*(LBUSER + n*7 +4));
pp_ptr->head.LBUSER[5] = ntohl(*(LBUSER + n*7 +5));
pp_ptr->head.LBUSER[6] = ntohl(*(LBUSER + n*7 +6));
	
pp_ptr->head.BRSVD[0] = BRSVD[n*4 +0];					
pp_ptr->head.BRSVD[1] = BRSVD[n*4 +1];					
pp_ptr->head.BRSVD[2] = BRSVD[n*4 +2];					
pp_ptr->head.BRSVD[3] = BRSVD[n*4 +3];					
pp_ptr->head.BDATUM = BDATUM[n];
pp_ptr->head.BACC = BACC[n];
pp_ptr->head.BLEV = BLEV[n];
pp_ptr->head.BRLEV = BRLEV[n];
pp_ptr->head.BHLEV = BHLEV[n];
pp_ptr->head.BHRLEV = BHRLEV[n];
pp_ptr->head.BPLAT = BPLAT[n];
pp_ptr->head.BPLON = BPLON[n];
pp_ptr->head.BGOR = BGOR[n];
pp_ptr->head.BZY = BZY[n];
pp_ptr->head.BDY = BDY[n];
pp_ptr->head.BZX = BZX[n];
pp_ptr->head.BDX = BDX[n];
pp_ptr->head.BMDI = BMDI[n];
pp_ptr->head.BMKS = BMKS[n];

	swap_4byte((int *)&(pp_ptr->head.BRSVD[0]),19);

	fwrite(  &(pp_ptr->head),	sizeof(pp64_header),           1, fout);


/* write fortran buffer  */
	fwrite(&(fort_buf_size_header), 1*sizeof(int),           1 ,fout);

	
/* write fortran buffer  */
	fwrite(&(fort_buf_size_data), 1*sizeof(int),           1 ,fout);

/* write data  */

/*for (i=0;i<LBLREC[n];i++) *(swapped_data +i) = (float)(ntohl((int)(data + LBLREC[n]*n +i)));*/
	swap_4byte((int *)(data+LBLREC[n]*n),LBLREC[n]);

	fwrite((data+LBLREC[n]*n), 		sizeof(float),	LBLREC[n], fout);

/* write fortran buffer  */
	fwrite(&(fort_buf_size_data), 1*sizeof(int),           1 ,fout);
				
	}
	
	fclose(fout);

	if (n!=nfields)
		Rprintf("Problem writing to %s\nCheck output\n",(*fname));
	else
		Rprintf("%d fields written to %s\n",nfields,(*fname));


}													

/***********************************************************************/

void
c_read_point_pp_file(int *ipt,
					int *irow,
					char **fname,
					int *count,
					int *idxfields,
					int *LBYR,
					int *LBMON,
					int *LBDAT,
					int *LBHR,
					int *LBMIN,
					int *LBDAY,
					int *LBYRD,
					int *LBMOND,
					int *LBDATD,
					int *LBHRD,
					int *LBMIND,
					int *LBDAYD,
					int *LBTIM,
					int *LBFT,
					int *LBLREC,
					int *LBCODE,
					int *LBHEM,
					int *LBROW,
					int *LBNPT,
					int *LBEXT,
					int *LBPACK,
					int *LBREL,
					int *LBFC,
					int *LBCFC,
					int *LBPROC,
					int *LBVC,
					int *LBRVC,
					int *LBEXP,
					int *LBEGIN,
					int *LBNREC,
					int *LBPROJ,
					int *LBTYP,
					int *LBLEV,
					int *LBRSVD,					
					int *LBSRCE,
					int *LBUSER,
					float *BRSVD,					
					float *BDATUM,
					float *BACC,
					float *BLEV,
					float *BRLEV,
					float *BHLEV,
					float *BHRLEV,
					float *BPLAT,
					float *BPLON,
					float *BGOR,
					float *BZY,
					float *BDY,
					float *BZX,
					float *BDX,
					float *BMDI,
					float *BMKS,
					float *data,
					int *BUF1,
					int *BUF2,
					int *BUF3,
					int *BUF4					
				)

{
/*      reads specified point from all pp fields:  header, 
		data and buffer bytes
        writes all corresponding data from pp file to  
		the pointers in the argument list
*/
	
	int			i,j, n, n2;
	FILE 		*fopen(),*fin;
	pp_struct 	*pp_ptr;
	int			float_size, pp_size;
	float		*data_buff, data_point;
	
	pp_size    = sizeof(pp_struct);
	float_size = sizeof(float);
	
	fin = fopen((*fname),"r");
	if (fin == NULL)
      {
      Rprintf("cant open file %s\n",(*fname));
      Rprintf("make sure there are no environment variables in strings\n");
	  error("help2");		
      }
	
	pp_ptr = (pp_struct *) Calloc(sizeof(pp_struct)*1,pp_struct);
	if(pp_ptr == NULL){
          Rprintf("No memory H\n");
          error("No memory H\n");
      	}
	
	fread(&(pp_ptr->fortran_buffer1), 1*sizeof(int),           1 ,fin);

	n      = 0;
	n2     = 0;
	while (!(feof(fin)))
	  {
				
	  if (n == (idxfields[n2]-1) )
	    {
		fread(&(pp_ptr->head),           64*sizeof(int),           1, fin);
		fread(&(pp_ptr->fortran_buffer2), 1*sizeof(int),           1 ,fin);
		fread(&(pp_ptr->fortran_buffer3), 1*sizeof(int),           1 ,fin);

		pp_ptr->data = (float *) Calloc(1*sizeof(float),float);
		if(pp_ptr->data == NULL){
              Rprintf("No memory I\n");
              error("No memory I\n");
      		}
		
		data_buff    = (float *) Calloc(ntohl(pp_ptr->head.LBLREC)*sizeof(float),float);
		if(data_buff == NULL){
              Rprintf("No memory J\n");
              error("No memory J\n");
      		}
			
		fread(data_buff,                ntohl(pp_ptr->head.LBLREC)*sizeof(float), 1, fin);
	   	
		data_point = *(data_buff + ((*irow)-1)*(ntohl(pp_ptr->head.LBNPT)) + (*ipt) -1);
	   	
		pp_ptr->data[0] = data_point;
		
		fread(&(pp_ptr->fortran_buffer4), 1*sizeof(int),           1 ,fin);		

		pp_ptr->head.LBLREC = htonl(1);
		
		byte_swap_pp(pp_ptr,READPP);

					LBYR[n] = pp_ptr->head.LBYR;
					LBMON[n] = pp_ptr->head.LBMON;
					LBDAT[n] = pp_ptr->head.LBDAT;
					LBHR[n] = pp_ptr->head.LBHR;
					LBMIN[n] = pp_ptr->head.LBMIN;
					LBDAY[n] = pp_ptr->head.LBDAY;
					LBYRD[n] = pp_ptr->head.LBYRD;
					LBMOND[n] = pp_ptr->head.LBMOND;
					LBDATD[n] = pp_ptr->head.LBDATD;
					LBHRD[n] = pp_ptr->head.LBHRD;
					LBMIND[n] = pp_ptr->head.LBMIND;
					LBDAYD[n] = pp_ptr->head.LBDAYD;
					LBTIM[n] = pp_ptr->head.LBTIM;
					LBFT[n] = pp_ptr->head.LBFT;
					LBLREC[n] = pp_ptr->head.LBLREC;
					LBCODE[n] = pp_ptr->head.LBCODE;
					LBHEM[n] = pp_ptr->head.LBHEM;
					LBROW[n] = pp_ptr->head.LBROW;
					LBNPT[n] = pp_ptr->head.LBNPT;
					LBEXT[n] = pp_ptr->head.LBEXT;
					LBPACK[n] = pp_ptr->head.LBPACK;
					LBREL[n] = pp_ptr->head.LBREL;
					LBFC[n] = pp_ptr->head.LBFC;
					LBCFC[n] = pp_ptr->head.LBCFC;
					LBPROC[n] = pp_ptr->head.LBPROC;
					LBVC[n] = pp_ptr->head.LBVC;
					LBRVC[n] = pp_ptr->head.LBRVC;
					LBEXP[n] = pp_ptr->head.LBEXP;
					LBEGIN[n] = pp_ptr->head.LBEGIN;
					LBNREC[n] = pp_ptr->head.LBNREC;
					LBPROJ[n] = pp_ptr->head.LBPROJ;
					LBTYP[n] = pp_ptr->head.LBTYP;
					LBLEV[n] = pp_ptr->head.LBLEV;
					LBSRCE[n] = pp_ptr->head.LBSRCE;

					BDATUM[n] = pp_ptr->head.BDATUM;
					BACC[n] = pp_ptr->head.BACC;
					BLEV[n] = pp_ptr->head.BLEV;
					BRLEV[n] = pp_ptr->head.BRLEV;
					BHLEV[n] = pp_ptr->head.BHLEV;
					BHRLEV[n] = pp_ptr->head.BHRLEV;
					BPLAT[n] = pp_ptr->head.BPLAT;
					BPLON[n] = pp_ptr->head.BPLON;
					BGOR[n] = pp_ptr->head.BGOR;
					BZY[n] = pp_ptr->head.BZY;
					BDY[n] = pp_ptr->head.BDY;
					BZX[n] = pp_ptr->head.BZX;
					BDX[n] = pp_ptr->head.BDX;
					BMDI[n] = pp_ptr->head.BMDI;
					BMKS[n] = pp_ptr->head.BMKS;
					
					BUF1[n] = pp_ptr->fortran_buffer1;
					BUF2[n] = pp_ptr->fortran_buffer2;
					BUF3[n] = pp_ptr->fortran_buffer3;
					BUF4[n] = pp_ptr->fortran_buffer4;

					for(i=0; i<=3; i++) LBRSVD[n*4+i] = pp_ptr->head.LBRSVD[i];
					for(i=0; i<=6; i++) LBUSER[n*7+i] = pp_ptr->head.LBUSER[i];
					for(i=0; i<=3; i++)  BRSVD[n*4+i] = pp_ptr->head.BRSVD[i];

					data[n] = pp_ptr->data[0];
					
					Free(pp_ptr->data);

					Free(data_buff);
					
		n  += 1;	
		n2 += 1;
		fread(&(pp_ptr->fortran_buffer1), 1*sizeof(int),           1 ,fin);
		} else {

		fread(&(pp_ptr->head),           64*sizeof(int),           1, fin);
		fread(&(pp_ptr->fortran_buffer2), 1*sizeof(int),           1 ,fin);
		fread(&(pp_ptr->fortran_buffer3), 1*sizeof(int),           1 ,fin);

		fseek(fin, ntohl(pp_ptr->head.LBLREC) * sizeof(float), SEEK_CUR);

		fread(&(pp_ptr->fortran_buffer4), 1*sizeof(int),           1 ,fin);

/*		byte_swap_pp(pp_ptr,READPP);
*/
		n  += 1;

		fread(&(pp_ptr->fortran_buffer1), 1*sizeof(int),           1 ,fin);
			
		}
	  }
	
	*count = n2;
	fclose(fin);
	Free(pp_ptr);

}													
/***********************************************************************/


/***********************************************************************/
void
c_read_row_pp_file(int *irow,
					char **fname,
					int *count,
					int *idxfields,
					int *LBYR,
					int *LBMON,
					int *LBDAT,
					int *LBHR,
					int *LBMIN,
					int *LBDAY,
					int *LBYRD,
					int *LBMOND,
					int *LBDATD,
					int *LBHRD,
					int *LBMIND,
					int *LBDAYD,
					int *LBTIM,
					int *LBFT,
					int *LBLREC,
					int *LBCODE,
					int *LBHEM,
					int *LBROW,
					int *LBNPT,
					int *LBEXT,
					int *LBPACK,
					int *LBREL,
					int *LBFC,
					int *LBCFC,
					int *LBPROC,
					int *LBVC,
					int *LBRVC,
					int *LBEXP,
					int *LBEGIN,
					int *LBNREC,
					int *LBPROJ,
					int *LBTYP,
					int *LBLEV,
					int *LBRSVD,					
					int *LBSRCE,
					int *LBUSER,
					float *BRSVD,					
					float *BDATUM,
					float *BACC,
					float *BLEV,
					float *BRLEV,
					float *BHLEV,
					float *BHRLEV,
					float *BPLAT,
					float *BPLON,
					float *BGOR,
					float *BZY,
					float *BDY,
					float *BZX,
					float *BDX,
					float *BMDI,
					float *BMKS,
					float *data,
					int *BUF1,
					int *BUF2,
					int *BUF3,
					int *BUF4					
				)

{
/*      reads all pp fields:  header, data and buffer bytes
        writes all corresponding data from pp file to  
		the pointers in the argument list
*/
	
	int			i, j, n, n2;
	FILE 		*fopen(),*fin;
	pp_struct 	*pp_ptr;
	int			float_size, pp_size;
	float		*data_buff, *data_row, f1;
	
	pp_size    = sizeof(pp_struct);
	float_size = sizeof(float);
	
	Rprintf("Trying to open %s\n",(*fname));
	
	fin = fopen((*fname),"r");
	if (fin == NULL)
      {
      Rprintf("cant open file %s\n",(*fname));
      Rprintf("make sure there are no environment variables in strings\n");
	  error("help2");		
      }
	
	  
	pp_ptr = (pp_struct *) Calloc(sizeof(pp_struct)*1,pp_struct);
	if(pp_ptr == NULL){
              Rprintf("No memory A\n");
              error("No memory A\n");
      }
	
	fread(&(pp_ptr->fortran_buffer1), 1*sizeof(int),           1 ,fin);

	n      = 0;
	n2     = 0;
	while (!(feof(fin)))
	  {
	  if (n == (idxfields[n2]-1) )  
	    {
		fread(&(pp_ptr->head),           64*sizeof(int),           1, fin);
		fread(&(pp_ptr->fortran_buffer2), 1*sizeof(int),           1 ,fin);
		fread(&(pp_ptr->fortran_buffer3), 1*sizeof(int),           1 ,fin);

		data_buff    = (float *) Calloc(ntohl(pp_ptr->head.LBLREC)*sizeof(float),float);
		if(data_buff == NULL){
              Rprintf("No memory B\n");
              error("No memory B\n");
      		}
			
		fread(data_buff,                ntohl(pp_ptr->head.LBLREC)*sizeof(float), 1, fin);

		pp_ptr->data = (data_buff + ((*irow)-1)*(ntohl(pp_ptr->head.LBNPT)) );

		fread(&(pp_ptr->fortran_buffer4), 1*sizeof(int),           1 ,fin);		
		
		byte_swap_pp(pp_ptr,READPP);

					LBYR[n2] = pp_ptr->head.LBYR;
					LBMON[n2] = pp_ptr->head.LBMON;
					LBDAT[n2] = pp_ptr->head.LBDAT;
					LBHR[n2] = pp_ptr->head.LBHR;
					LBMIN[n2] = pp_ptr->head.LBMIN;
					LBDAY[n2] = pp_ptr->head.LBDAY;
					LBYRD[n2] = pp_ptr->head.LBYRD;
					LBMOND[n2] = pp_ptr->head.LBMOND;
					LBDATD[n2] = pp_ptr->head.LBDATD;
					LBHRD[n2] = pp_ptr->head.LBHRD;
					LBMIND[n2] = pp_ptr->head.LBMIND;
					LBDAYD[n2] = pp_ptr->head.LBDAYD;
					LBTIM[n2] = pp_ptr->head.LBTIM;
					LBFT[n2] = pp_ptr->head.LBFT;
					LBLREC[n2] = pp_ptr->head.LBLREC;
					LBCODE[n2] = pp_ptr->head.LBCODE;
					LBHEM[n2] = pp_ptr->head.LBHEM;
					LBROW[n2] = pp_ptr->head.LBROW;
					LBNPT[n2] = pp_ptr->head.LBNPT;
					LBEXT[n2] = pp_ptr->head.LBEXT;
					LBPACK[n2] = pp_ptr->head.LBPACK;
					LBREL[n2] = pp_ptr->head.LBREL;
					LBFC[n2] = pp_ptr->head.LBFC;
					LBCFC[n2] = pp_ptr->head.LBCFC;
					LBPROC[n2] = pp_ptr->head.LBPROC;
					LBVC[n2] = pp_ptr->head.LBVC;
					LBRVC[n2] = pp_ptr->head.LBRVC;
					LBEXP[n2] = pp_ptr->head.LBEXP;
					LBEGIN[n2] = pp_ptr->head.LBEGIN;
					LBNREC[n2] = pp_ptr->head.LBNREC;
					LBPROJ[n2] = pp_ptr->head.LBPROJ;
					LBTYP[n2] = pp_ptr->head.LBTYP;
					LBLEV[n2] = pp_ptr->head.LBLEV;
					LBSRCE[n2] = pp_ptr->head.LBSRCE;

					BDATUM[n2] = pp_ptr->head.BDATUM;
					BACC[n2] = pp_ptr->head.BACC;
					BLEV[n2] = pp_ptr->head.BLEV;
					BRLEV[n2] = pp_ptr->head.BRLEV;
					BHLEV[n2] = pp_ptr->head.BHLEV;
					BHRLEV[n2] = pp_ptr->head.BHRLEV;
					BPLAT[n2] = pp_ptr->head.BPLAT;
					BPLON[n2] = pp_ptr->head.BPLON;
					BGOR[n2] = pp_ptr->head.BGOR;
					BZY[n2] = pp_ptr->head.BZY;
					BDY[n2] = pp_ptr->head.BDY;
					BZX[n2] = pp_ptr->head.BZX;
					BDX[n2] = pp_ptr->head.BDX;
					BMDI[n2] = pp_ptr->head.BMDI;
					BMKS[n2] = pp_ptr->head.BMKS;
					
					BUF1[n2] = pp_ptr->fortran_buffer1;
					BUF2[n2] = pp_ptr->fortran_buffer2;
					BUF3[n2] = pp_ptr->fortran_buffer3;
					BUF4[n2] = pp_ptr->fortran_buffer4;

					for(i=0; i<=3; i++) LBRSVD[n2*4+i] = pp_ptr->head.LBRSVD[i];
					for(i=0; i<=6; i++) LBUSER[n2*7+i] = pp_ptr->head.LBUSER[i];
					for(i=0; i<=3; i++)  BRSVD[n2*4+i] = pp_ptr->head.BRSVD[i];

					for(i=0; i<=pp_ptr->head.LBNPT -1; i++)
						data[n2*pp_ptr->head.LBNPT +i] = *(pp_ptr->data +i);
					
					Free(data_buff);
		n  += 1;	
		n2 += 1;
		fread(&(pp_ptr->fortran_buffer1), 1*sizeof(int),           1 ,fin);

		} else {

		fread(&(pp_ptr->head),           64*sizeof(int),           1, fin);
		fread(&(pp_ptr->fortran_buffer2), 1*sizeof(int),           1 ,fin);
		fread(&(pp_ptr->fortran_buffer3), 1*sizeof(int),           1 ,fin);

		fseek(fin, ntohl(pp_ptr->head.LBLREC) * sizeof(float), SEEK_CUR);

		fread(&(pp_ptr->fortran_buffer4), 1*sizeof(int),           1 ,fin);

		n  += 1;

		fread(&(pp_ptr->fortran_buffer1), 1*sizeof(int),           1 ,fin);
			
		}
	  }
	
	*count = n2;
	fclose(fin);
	Free(pp_ptr);

}													
/***********************************************************************/


/***********************************************************************/
/***********************************************************************/
/***********************************************************************/
