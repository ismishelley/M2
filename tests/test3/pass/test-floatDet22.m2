int main(){
	matrix float [2][2] m;
	float det;
	m = [[1.0,2.0];
		 [3.0,4.0]];
	det = m[0][0] * m[1][1] - m[1][0] * m[0][1];

	printStr("The determinant of matrix");
	myprint(m);
	printStr("is");
	printFloat(det);
	
	return 0;
}

void myprint(matrix float [2][2] M){
  int i;
  int j;
  for(i = 0; i < M:rows; ++i){
    for(j = 0; j < M:cols; ++j){
      printFloat(M[i][j]);
    }
    printStr("");
  }
}