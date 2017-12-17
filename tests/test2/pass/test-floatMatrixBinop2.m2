int main() {
  float a;
  matrix float [2][3] M;
  a = 9.0;
  M = [[0.0,0.0,0.0];
      [0.0,0.0,0.0]];

  printStr("Matrix M is");
  myprint(M);
  printStr("Float a is");
  printFloat(a);
  printStr("");

  printStr("a * M is");
  myprint(a * M);    


  return 0;
}

void myprint(matrix float [2][3] M){
  int i;
  int j;
  for(i = 0; i < M:rows; ++i){
    for(j = 0; j < M:cols; ++j){
      printFloat(M[i][j]);
    }
    printStr("");
  }
}

