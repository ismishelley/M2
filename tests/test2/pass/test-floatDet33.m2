int main(){
	matrix float [3][3] m;
	float det;

	m = [[1.0,2.0,3.0];
		 [4.0,5.0,6.0];
		 [7.0,8.0,9.0]];

	det = det = m[0][0] * (m[1][1] * m[2][2] - m[1][2] * m[2][1])
		- m[0][1] * (m[1][0] * m[2][2] - m[1][2] * m[2][0])
		+ m[0][2] * (m[1][0] * m[2][1] - m[1][1] * m[2][0]);

	printStr("The determinant of matrix");
	myprint(m);
	printStr("is");
	printFloat(det);
	
	return 0;
}

void myprint(matrix float [3][3] M){
  int i;
  int j;
  for(i = 0; i < M:rows; ++i){
    for(j = 0; j < M:cols; ++j){
      printFloat(M[i][j]);
    }
    printStr("");
  }
}