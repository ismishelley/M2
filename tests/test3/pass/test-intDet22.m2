int det22(matrix int [2][2] input){
	int det;
	det = input[0][0] * input[1][1] - input[1][0] * input[0][1];
	return det;
}

int main(){
	matrix int [2][2] m;
	m = [[1,2];
		 [3,4]];
	printStr("The determinant of matrix");
	myprint(m);
	printStr("is");
	printInt(det22(m));
	
	return 0;
}

void myprint(matrix int [2][2] M){
  int i;
  int j;
  for(i = 0; i < M:rows; ++i){
    for(j = 0; j < M:cols; ++j){
      printInt(M[i][j]);
    }
    printStr("");
  }
}