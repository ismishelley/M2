int main() {
  matrix float [3][3] m;
  matrix float [3][1] m2;
  matrix float [3][3] tr;
  matrix float [3][3] adj;
  matrix float [3][3] inverse;
  matrix float [3][1] result;
  float det;
  int i;
  int j;

  m = [[1.0, 1.0, 1.0];
        [2.0, 3.0, 1.0];
        [1.0, 1.0, 5.0]];

  m2 = [[3.0];
        [6.0];
        [7.0]];

  tr = m:transpose;

  det = m[0][0] * (m[1][1] * m[2][2] - m[1][2] * m[2][1])
    - m[0][1] * (m[1][0] * m[2][2] - m[1][2] * m[2][0])
    + m[0][2] * (m[1][0] * m[2][1] - m[1][1] * m[2][0]);

  if (det != 0.0){
    adj[0][0] = tr[1][1]*tr[2][2] - tr[1][2]*tr[2][1];
    adj[0][1] = tr[1][0]*tr[2][2] - tr[1][2]*tr[2][0];
    adj[0][2] = tr[1][0]*tr[2][1] - tr[1][1]*tr[2][0];
    adj[1][0] = tr[0][1]*tr[2][2] - tr[0][2]*tr[2][1];
    adj[1][1] = tr[0][0]*tr[2][2] - tr[0][2]*tr[2][0];
    adj[1][2] = tr[0][0]*tr[2][1] - tr[0][1]*tr[2][0];
    adj[2][0] = tr[0][1]*tr[1][2] - tr[0][2]*tr[1][1];
    adj[2][1] = tr[0][0]*tr[1][2] - tr[0][2]*tr[1][0];
    adj[2][2] = tr[0][0]*tr[1][1] - tr[0][1]*tr[1][0];


    adj[0][1] = adj[0][1] * (0.0-1.0);
    adj[1][0] = adj[1][0] * (0.0-1.0);
    adj[1][2] = adj[1][2] * (0.0-1.0);
    adj[2][1] = adj[2][1] * (0.0-1.0);

    for(i = 0; i < inverse:rows; ++i){
      for (j = 0; j < inverse:cols; ++j){
        inverse[i][j] = adj[i][j] / det;
      }
    }

    result = inverse * m2;

    printStr("Input equations are:");

    for (i = 0; i < m:rows; ++i){
      printFloat(m[i][0]);
      printStr("x + ");
      printFloat(m[i][1]);
      printStr("y ");
      printFloat(m[i][2]);
      printStr("z = ");
      printFloat(m2[i][0]);
      printStr("");
      printStr("");
    }

    printStr("Solutions are:");
    printStr("x = ");
    printFloat(result[0][0]);
    printStr("");
    printStr("y = ");
    printFloat(result[1][0]);
    printStr("");
    printStr("z = ");
    printFloat(result[2][0]);

  }
  else{
    printStr("Cannot find a solution");
  }

  return 0;
}


void myprint(matrix float [3][1] M){
  int i;
  int j;
  for(i = 0; i < M:rows; ++i){
    for(j = 0; j < M:cols; ++j){
      printFloat(M[i][j]);
    }
    printStr("");
  }
}