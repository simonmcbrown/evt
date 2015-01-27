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
