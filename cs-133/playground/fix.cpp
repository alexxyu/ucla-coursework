#include <chrono>
#include <thread>
#include <omp.h>

int main() {
    int i, j;

    #pragma omp parallel for private(j) schedule(guided, 1)
    for (i=0; i<12; i++) {
        for (j=i; j<12; j++) {
            std::this_thread::sleep_for(std::chrono::milliseconds(100));
        }
    }

    return 0;
}
