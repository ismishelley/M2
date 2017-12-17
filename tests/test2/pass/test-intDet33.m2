int det33(matrix int [3][3] input){
	int det;
	det = input[0][0] * (input[1][1] * input[2][2] - input[1][2] * input[2][1])
		- input[0][1] * (input[1][0] * input[2][2] - input[1][2] * input[2][0])
		+ input[0][2] * (input[1][0] * input[2][1] - input[1][1] * input[2][0]);

	return det;
}

int main(){
	matrix int [3][3] m;
	m = [[1,2,3];
		 [4,5,6];
		 [7,8,9]];

	printStr("The determinant of matrix");
	myprint(m);
	printStr("is");

	printInt(det33(m));
	
	return 0;
}

void myprint(matrix int [3][3] M){
  int i;
  int j;
  for(i = 0; i < M:rows; ++i){
    for(j = 0; j < M:cols; ++j){
      printInt(M[i][j]);
    }
    printStr("");
  }
}