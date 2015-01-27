	typedef struct	{
		int			LBYR;
		int			LBMON;
		int			LBDAT;
		int			LBHR;
		int			LBMIN;
		int			LBDAY;
		int			LBYRD;
		int			LBMOND;
		int			LBDATD;
		int			LBHRD;
		int			LBMIND;
		int			LBDAYD;
		int			LBTIM;
		int			LBFT;
		int			LBLREC;
		int			LBCODE;
		int			LBHEM;
		int			LBROW;
		int			LBNPT;
		int			LBEXT;
		int			LBPACK;
		int			LBREL;
		int			LBFC;
		int			LBCFC;
		int			LBPROC;
		int			LBVC;
		int			LBRVC;
		int			LBEXP;
		int			LBEGIN;
		int			LBNREC;
		int			LBPROJ;
		int			LBTYP;
		int			LBLEV;
		int			LBRSVD[4];
		int			LBSRCE;
		int			LBUSER[7];
		float		BRSVD[4];
		float		BDATUM;
		float		BACC;
		float		BLEV;
		float		BRLEV;
		float		BHLEV;
		float		BHRLEV;
		float		BPLAT;
		float		BPLON;
		float		BGOR;
		float		BZY;
		float		BDY;
		float		BZX;
		float		BDX;
		float		BMDI;
		float		BMKS;
		}
		pp64_header;

	typedef struct	{
		int			fortran_buffer1;
		pp64_header	head;
		int			fortran_buffer2;
		int			fortran_buffer3;
		float*		data;
		int			fortran_buffer4;
		}
		pp_struct;
		
