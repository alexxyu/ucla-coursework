#include "lib/macros.cuh"
#include "kernel.h"

__global__ void cnn_gpu(float* input,
    float* weight,
    float* bias, float* output)
{
  // your implementation goes here
  // refer to the seq. implementation until you achieve correctness

  // Allocate memory on heap to avoid stack overflow.
  float C[kImSize][kImSize];

  // Input size: 256 x 228 x 228
  // GPU specs: 16 SMs, 32 blocks/SM, 2048 threads/SM (32K threads total)
  // 256 channels -> 256 x 1 x 1 grid (note: we're using 50% of max # of blocks)

  // Bias
  int i = blockIdx.x;
  for (int h = 0; h < kImSize; ++h) {
    for (int w = 0; w < kImSize; ++w)
      C[h][w] = bias[i];
  }

  // Convolution
  for (int j = 0; j < kNum; ++j) {
    for (int h = 0; h < kImSize; ++h) {
      for (int w = 0; w < kImSize; ++w) {
        for (int p = 0; p < kKernel; ++p) {
          for (int q = 0; q < kKernel; ++q)
            C[h][w] += weight(i, j, p, q) * input(j, h + p, w + q);
        }
      }
    }
  }

  // ReLU
  for (int h = 0; h < kImSize; ++h) {
    for (int w = 0; w < kImSize; ++w) {
      C[h][w] = max(0.f, C[h][w]);
    }
  }

  // Max pooling
  for (int h = 0; h < kOutImSize; ++h) {
    for (int w = 0; w < kOutImSize; ++w) {
      output(i, h, w) = max(
          max(C[h * 2][w * 2    ], C[h * 2 + 1][w * 2    ]),
          max(C[h * 2][w * 2 + 1], C[h * 2 + 1][w * 2 + 1]));
    }
  }
}
