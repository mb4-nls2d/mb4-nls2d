#include <stdio.h>

int csrchk_(int* pm, double* v, int* r, int* c)
{
	int i, j, m = *pm;
	int dummy=0;
	double d = 0.;
	for(i=0; i<=m; ++i) {
		if(r[i] <= 0) return 1;
		if(i && r[i-1] > r[i]) return 2;
	}
	int sz = r[m] - 1;
	for(j=0; j<sz; ++j) {
		dummy += c[j];
		d += v[j];
	}
	if(dummy < 0) {
		printf("%f\n", d);
		return 3;
	}

	for(i=0; i<m; ++i)
		for(j=r[i]; j<r[i+1]-1; ++j){
			if(c[j-1] >= c[j]) return 4;	
		}
	return 0;
}
