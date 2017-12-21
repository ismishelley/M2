int main() {
    matrix int [8][8] m;
    int i;
    int j;
    int k;
    int sum;
    int sumM;
    int new;
    m = [[21,9,1,13,12,13,30,11];
        [0,2,5,14,1,13,1,13];
        [30,9,12,31,54,16,18,8];
        [3,44,13,15,19,7,8,18];
        [3,45,45,20,31,7,8,18];
        [3,45,45,20,19,7,2,1];
        [23,13,13,13,23,7,4,1];
        [12,44,13,31,11,7,8,8]
        ];
    myprint(m);
    for(i = 0; i < 6; ++i) {
        sum = 0;
        sumM = 0;
        for(j = 0; j < 8; ++j) {
            for(k = 0; k < 8; ++k) {
                if(m[j][k] > 0) {
                    m[j][k] = m[j][k] - 1;
                    ++sum;
                }
                sumM = sumM + m[j][k];
                
            }
        }
        for(j = 0; j < 8; ++j) {
            for(k = 0; k < 8; ++k) {
                new = sum * m[j][k] / sumM;
                m[j][k] = m[j][k] + new;
            }
        }
        myprint(m);
        printStr("");

    }
    return 0;
}

void myprint(matrix int [8][8] M) { 
    int i;
    int j;
    for(i = 0; i < M:rows; ++i) {
        for(j = 0; j < M:cols; ++j) {
            printInt(M[i][j]);
            printInt(999);
        }
        printStr("");
    }
}