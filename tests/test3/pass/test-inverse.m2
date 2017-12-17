int main(){
	matrix float [2][2] m;
	matrix float [2][2] inverse;
	float det;

	m = [[1.0,2.0];
		 [3.0,4.0]];

	det = m[0][0] * m[1][1] - m[1][0] * m[0][1];

	if(det != 0.0){
		inverse[0][0] = m[1][1] / det;
	    inverse[0][1] = - m[0][1] /det;
	    inverse[1][0] = - m[1][0] /det;
	    inverse[1][1] = m[0][0] / det ;
	    printStr("The inverse of matrix");
		myprint(m);
		printStr("is");
		myprint(inverse);
	}
	else{
		printStr("Cannot find inverse");
	}
	
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