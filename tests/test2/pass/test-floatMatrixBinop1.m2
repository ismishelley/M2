int main() {
  matrix float [2][2] m;
  m = [[2.0,1.0];
        [0.0,3.0]];

  printStr("Matrix m is");
  myprint(m);

  printStr("m + m is");
  myprint(m + m);

  printStr("m - m is");
  myprint(m - m);

  printStr("m * m is");
  myprint(m * m);

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

