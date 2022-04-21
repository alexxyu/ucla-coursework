#include <cstring>
#include <iostream>
#include <stdlib.h>
#include <omp.h>
using namespace std;

#define M 100
#define N 1000000

void histogram_sequential(int *a, int *h) {
    int i;
    for (i=0; i<N; i++) {
        h[a[i]]++;
    }
}

void histogram_atomic(int *a, int *h) {
    int i;

    #pragma omp parallel for
    for (i=0; i<N; i++) {
        #pragma omp atomic
        h[a[i]]++;
    }
}

void histogram_reduce(int *a, int *h) {
    int i;

    #pragma omp parallel
    {
        int hTmp[M];
        std::memset(hTmp, 0, sizeof(int) * M);
        
        #pragma omp for
        for (i=0; i<N; i++) {
            hTmp[a[i]]++;
        }

        #pragma omp critical
        for (i=0; i<M; i++) {
            h[i] += hTmp[i];
        }
    }
    
}

int main() {
    int a[N];

    for (int i=0; i<N; i++) {
        a[i] = rand() % M;
    }

    int hSeq[M], hAtomic[M], hReduce[M];
    histogram_sequential(a, hSeq);
    histogram_atomic(a, hAtomic);
    histogram_reduce(a, hReduce);

    for (int i=0; i<M; i++) {
        if (hSeq[i] != hAtomic[i]) {
            cout << "Atomic histogram failed" << endl;
            break;
        }
    }

    for (int i=0; i<M; i++) {
        if (hSeq[i] != hReduce[i]) {
            cout << "Reduction histogram failed: " << hSeq[i] << " vs. " << hReduce[i] << endl;
            break;
        }
    }

    return 0;
}
