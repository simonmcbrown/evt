 			&( (int *)(pp_ptr->head) +i) = ntohl(   (int)((int *)(pp_ptr->head) +i))   );



















	fin = fopen((*fname),"r");
	if (fin == NULL)
      {
      printf("cant open file %s\n",(*fname));
      printf("make sure there are no environment variables in strings\n");
	  return;		
      }
	
	(*pp1) = (pp_struct *) malloc(sizeof(pp_struct)*1);
	pp_ptr = (*pp1);
	
	fread(&(pp_ptr->fortran_buffer1), 1*sizeof(int),           1 ,fin);

	n      = 0;
	while (!(feof(fin)))
		{
		fread(&(pp_ptr->head),           64*sizeof(int),           1, fin);
		fread(&(pp_ptr->fortran_buffer2), 1*sizeof(int),           1 ,fin);
		fread(&(pp_ptr->fortran_buffer3), 1*sizeof(int),           1 ,fin);

		pp_ptr->data = (float *) malloc(pp_ptr->head.LBLREC * sizeof(float));
		fread(pp_ptr->data,   pp_ptr->head.LBLREC*sizeof(float), 1, fin);

		fread(&(pp_ptr->fortran_buffer4), 1*sizeof(int),           1 ,fin);
		
		
/*		LBYR[n] = pp_ptr->head.LBYR;
*/		
		

		n       += 1;	
		(*pp1)   = (pp_struct *) realloc((*pp1), sizeof(pp_struct)*n);
		pp_ptr   = (*pp1) +(n-1);  /* set the pointer to the begining of the
										new last header  */

		fread(&(pp_ptr->fortran_buffer1), 1*sizeof(int),           1 ,fin);
		}
	
	*count = n;
	*nrow  = pp_ptr->head.LBROW;
	*npt   = pp_ptr->head.LBNPT;

	fclose(fin);
					

					/*LBRSVD=as.integer(array(int.dummy,dim=c(4,count))),*/
					/*LBUSER=as.integer(array(int.dummy,dim=c(7,count))),*/
					/*BRSVD=as.single(array(dbl.dummy,dim=c(4,count))),*/
					/*data=as.single(array(dbl.dummy,dim=c(npt,nrow,count))),*/
