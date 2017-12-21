int main() {
  matrix float [2][2] m1;
  matrix float [2][1] m2;

  m1 = [[2.0,1.0];
        [3.0,3.0]];

  m2 = [[4.0];
        [9.0]];

  solveEqn(m1,m2);   

  return 0;
}

void solveEqn(matrix float [2][2] m1, matrix float [2][1] m2){
  float det;
  matrix float [2][2] inverse;
  matrix float [2][1] result;
  int i;
  int j;

  det = m1[0][0] * m1[1][1] - m1[1][0] * m1[0][1];

  if (det != 0.0){
    inverse[0][0] = m1[1][1] / det;
    inverse[0][1] = - m1[0][1] /det;
    inverse[1][0] = - m1[1][0] /det;
    inverse[1][1] = m1[0][0] / det ;

    result = inverse * m2;

    printStr("Input equation is:");
    printFloat(m1[0][0]);
    printStr("x + ");
    printFloat(m1[0][1]);
    printStr("y = ");
    printFloat(m2[0][0]);
    printStr("");
    printStr("");

    printFloat(m1[1][0]);
    printStr("x + ");
    printFloat(m1[1][1]);
    printStr("y = ");
    printFloat(m2[1][1]);
    printStr("");
    printStr("");

    printStr("Solutions are:");
    printStr("x = ");
    printFloat(result[0][0]);
    printStr("");
    printStr("y = ");
    printFloat(result[1][0]);
  }
  else{
    printStr("Cannot find a solution");
  }


}