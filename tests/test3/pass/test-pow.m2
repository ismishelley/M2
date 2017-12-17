void myprint(matrix int [4][4] M) { 
    int i;
    int j;
    for(i = 0; i < M:rows; ++i) {
        for(j = 0; j < M:cols; ++j) {
            printInt(M[i][j]);
        }
        printStr("");
    }
}

void pow(matrix int[4][4] input, int N) {
    matrix int[4][4] res;
    matrix int[4][4] ret;
    int flag;
    flag = 1;
    res = input;
    while(N > 1) {
        if(N - N / 2 * 2 == 0) {
            res = res * res;
            N = N / 2;
        }
        else {
            if(flag == 1) {
                ret = res;
                flag = -1;
            }
            else {
                ret = ret * res;
            }
            res = res * res;
            N = N / 2;
        }
    }
    if(flag == 1) {
        myprint(res);
    }
    else {
        ret = ret * res;
        myprint(ret);
    }
    
}

int main() {
    matrix int [4][4] m;
    int time;
    time = 2;
    m = [[2,9,1,1];
        [0,2,1,1];
        [1,1,1,1];
        [3,1,1,1]
    ];
    pow(m, 6);
    return 0;
}