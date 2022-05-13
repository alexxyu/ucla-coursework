#include "lib/macros.cuh"
#include "kernel.h"

#define TILE_SIZE 32

__global__ void cnn_gpu(float* input,
    float* weight,
    float* bias, float* output)
{
  // Input size: 256 x 224 x 224
  // OUtput size: 256 x 112 x 112
  // GPU specs: 16 SMs, 32 blocks/SM, 2048 threads/SM (32K threads total)

  const int br = blockIdx.y * blockDim.y * 2;
  const int bc = blockIdx.x * blockDim.x * 2;

  const int tr = threadIdx.y;
  const int tc = threadIdx.x;
  const int h = 2*tr;
  const int w = 2*tc;

  __shared__ float inputShared [TILE_SIZE+kKernel-1][TILE_SIZE+kKernel-1] __attribute__((aligned(16 * sizeof(float))));
  __shared__ float weightShared[kKernel            ][kKernel            ] __attribute__((aligned(16 * sizeof(float))));

  for (int i = 0; i < kNum; i++) {
    // Bias
    float C0 = bias[i];
    float C1 = bias[i];
    float C2 = bias[i];
    float C3 = bias[i];

    // Convolution
    for (int j = 0; j < kNum; j++) {
      // Load input and weight submatrices into shared memory
      inputShared[h  ][w  ] = input(j, br+h  , bc+w  );
      inputShared[h  ][w+1] = input(j, br+h  , bc+w+1);
      inputShared[h+1][w  ] = input(j, br+h+1, bc+w  );
      inputShared[h+1][w+1] = input(j, br+h+1, bc+w+1);

      if (tr < kKernel-1) {
        inputShared[TILE_SIZE+tr][w  ] = input(j, br+TILE_SIZE+tr, bc+w  );
        inputShared[TILE_SIZE+tr][w+1] = input(j, br+TILE_SIZE+tr, bc+w+1);
      }
      if (tc < kKernel-1) {
        inputShared[h  ][TILE_SIZE+tc] = input(j, br+h  , bc+TILE_SIZE+tc);
        inputShared[h+1][TILE_SIZE+tc] = input(j, br+h+1, bc+TILE_SIZE+tc);
      }
      if (tr < kKernel-1 && tc < kKernel-1) {
        inputShared[TILE_SIZE+tr][TILE_SIZE+tc] = input(j, br+TILE_SIZE+tr, bc+TILE_SIZE+tc);
      }
      if (tr < kKernel && tc < kKernel) {
        weightShared[tr][tc] = weight(i, j, tr, tc);
      }
      __syncthreads();

      // Matrix multiplication between weight and input submatrices
      for (int p = 0; p < kKernel; p++) {
        for (int q = 0; q < kKernel; q++) {
          C0 += weightShared[p][q] * inputShared[h+p  ][w+q  ];
          C1 += weightShared[p][q] * inputShared[h+p+1][w+q  ];
          C2 += weightShared[p][q] * inputShared[h+p  ][w+q+1];
          C3 += weightShared[p][q] * inputShared[h+p+1][w+q+1];
        }
      }
      __syncthreads();
    }

    // Max pooling + ReLU
    output(i, (br+h)/2, (bc+w)/2) = max(0.f, max(
        max(C0, C1),
        max(C2, C3)));
  }
}
