int norm1(matrix int [4][4] input)
{
	int row;
	int col;
	int maxSum;
	int Sum;
	int finalIndex;
	int index;
	int index2;
	maxSum = -10000000;
	Sum = 0;
	row = input:rows;
	col = input:cols;


	for(index = 0; index < col; index = index + 1) {
		Sum = 0;
		for(index2 = 0; index2 < row; index2 = index2 + 1) {
			Sum = Sum + input[index2][index];
		}
		if(Sum > maxSum) {
			maxSum = Sum;
			finalIndex = index;
		}
	}

	return maxSum;
}


int main() {
  matrix int [4][4] m;
  m = [[2,9,1,1];
        [0,2,1,1];
        [1,1,1,1];
        [3,1,1,1]];

  printInt(norm1(m));

  return 0;
}