typedef /* Subroutine */ int (*S_fp)();

/* Subroutine */
void fdcom(int *n, int *m, int *nar, int *nma,
	   double *hood, double *flmin, double *flmax,
	   double *epmin, double *epmax);

/* Subroutine */ int
fdfilt_(double *x, double *d__, double *y,
	double *slogvk, double *amk, double *ak, double *vk,
	double *phi, double *pi);

void fdsim(int *n, int *ip, int *iq, double *ar, double *ma,
	   double *d__, double *rmu, double *y, double *s,
	   double *flmin, double *flmax, double *epmin, double *epmax);

int lmder1_(S_fp fcn, int *m, int *n, double *x,
	    double *fvec, double *fjac, int *ldfjac, double *ftol,
	    double *xtol, double *gtol, int *maxfev, double *diag,
	    int *mode, double *factor, int *info,
	    int *nfev, int *njev, int *ipvt, double *qtf,
	    double *wa1, double *wa2, double *wa3, double *wa4,
	    double *y);

int ajqp_(double *qp, double *a, double *ajac,
	  int *lajac, int *iflag, double *y);