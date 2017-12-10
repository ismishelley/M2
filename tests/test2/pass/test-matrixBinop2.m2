int main() {
  int a;
  matrix int [2][3] M;
  a = 9;
  M = [[0,0,0];
      [0,0,0]];

  printStr("Matrix M is");
  myprint(M);
  printStr("Int a is");
  printInt(a);
  printStr("");

  printStr("a * M is");
  myprint(a * M);    


  return 0;
}

void myprint(matrix int [2][3] M){
  int i;
  int j;
  for(i = 0; i < M:rows; ++i){
    for(j = 0; j < M:cols; ++j){
      printInt(M[i][j]);
    }
    printStr("");
  }
}

