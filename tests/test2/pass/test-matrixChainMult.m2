int main(){
	matrix int [1][7] p;
	matrix int[6][6] DP;
	int n;
	int i;
	int l;
	int j;
	int k;
	int q;
	int tmp1;
	int tmp2;
	int tmp3;

	n = DP:cols;

	p = [[30,35,15,5,10,20,25]];

	for (i = 0; i < DP:cols; ++i){
		DP[i][i] = 0;
	}

	for (l = 2; l < n+1; ++l){
		for(i = 0; i < n-l+1; ++i){
			j = i + l - 1;
			DP[i][j] = 100000;
			for(k = i; k < j; ++k){
				tmp1 = k+1;
				tmp2 = i-1;
				tmp3 = j+1;

				q = DP[i][k] + DP[tmp1][j] + p[0][i] * p[0][tmp1] * p[0][tmp3];

				if (q < DP[i][j]){
					DP[i][j] = q;
				}
			}
		}
	}
	myprint(DP);

	return 0;
}


void myprint(matrix int [6][6] M){
  int i;
  int j;
  for(i = 0; i < M:rows; ++i){
    for(j = 0; j < M:cols; ++j){
      printInt(M[i][j]);
    }
    printStr("");
  }
}