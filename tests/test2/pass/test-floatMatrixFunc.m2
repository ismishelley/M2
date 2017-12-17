int main() {
  matrix float [2][2] m;
  m = [[1.0,1.0];
      [0.0,2.0]];

  printStr("Matrix m is");
  myprint(m);

  printStr("Rows of m");
  printInt(m:rows);
  printStr("");

  printStr("Columns of m");
  printInt(m:cols);
  printStr("");

  printStr("Transpose of m");
  myprint(m:transpose);

  printStr("Trace of m");
  printFloat(m:trace);
  printStr("");

  printStr("Submatrix (0,1,0,1) of m");
  printFloat(m:subMatrix[0,0,0,0]);
  printStr("");

  printStr("Submatrix (0,1,0,1) of m");
  myprint(m:subMatrix[0,1,0,1]);

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

