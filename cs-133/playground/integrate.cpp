#include <cmath>
#include <omp.h>
#include <iostream>
using namespace std;

#define NSTEPS 10000000

float f(float x) {
    return sqrt(x) / (1 + pow(x, 3));
}

float integrate(int nsteps) {
    float area;
    float dx = 1 / ((float) nsteps);

    #pragma omp parallel for reduction(+:area)
    for (int i=0; i<nsteps; i++) {
        area += f(dx*i) * dx;
    }

    return area;
}

int main() {
    cout << integrate(NSTEPS) << endl;
    return 0;
}
