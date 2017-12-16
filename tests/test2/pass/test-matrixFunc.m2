int main() {
  matrix int [2][2] m;
  m = [[1,1];
      [0,2]];

  printStr("Rows of m");
  printInt(m:rows);
  printStr("");

  printStr("Columns of m");
  printInt(m:cols);
  printStr("");

  printStr("Transpose of m");
  myprint(m:transpose);

  printStr("Trace of m");
  printInt(m:trace);
  printStr("");

  printStr("Submatrix (0,1,0,1) of m");
  printInt(m:subMatrix[0,0,0,0]);
  printStr("");

  printStr("Submatrix (0,1,0,1) of m");
  myprint(m:subMatrix[0,1,0,1]);

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

