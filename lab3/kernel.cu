#include "lib/macros.cuh"
#include "kernel.h"

#define BLOCKDIM_X 16
#define BLOCKDIM_Y 16

#define TILE_WIDTH_C BLOCKDIM_X*2
#define TILE_WIDTH_R BLOCKDIM_Y*2

__global__ void cnn_gpu(float* input,
    float* weight,
    float* bias, float* output)
{
  // Input size: 256 x 224 x 224
  // OUtput size: 256 x 112 x 112
  // GPU specs: 16 SMs, 32 blocks/SM, 2048 threads/SM (32K threads total)

  const int nc = kNum / gridDim.z;
  const int channel = blockIdx.z * nc;

  const int br = blockIdx.y * TILE_WIDTH_R;
  const int bc = blockIdx.x * TILE_WIDTH_C;

  const int tr = threadIdx.y;
  const int tc = threadIdx.x;
  const int h = 2 * tr;
  const int w = 2 * tc;

  __shared__ float inputShared [TILE_WIDTH_R+kKernel-1][TILE_WIDTH_C+kKernel-1] __attribute__((aligned(16 * sizeof(float))));
  __shared__ float weightShared[kKernel               ][kKernel               ] __attribute__((aligned(16 * sizeof(float))));

  for (int i = channel; i < channel+nc && i < kNum; i++) {
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
        inputShared[TILE_WIDTH_R+tr][w  ] = input(j, br+TILE_WIDTH_R+tr, bc+w  );
        inputShared[TILE_WIDTH_R+tr][w+1] = input(j, br+TILE_WIDTH_R+tr, bc+w+1);
      }
      if (tc < kKernel-1) {
        inputShared[h  ][TILE_WIDTH_C+tc] = input(j, br+h  , bc+TILE_WIDTH_C+tc);
        inputShared[h+1][TILE_WIDTH_C+tc] = input(j, br+h+1, bc+TILE_WIDTH_C+tc);
      }
      if (tr < kKernel-1 && tc < kKernel-1) {
        inputShared[TILE_WIDTH_R+tr][TILE_WIDTH_C+tc] = input(j, br+TILE_WIDTH_R+tr, bc+TILE_WIDTH_C+tc);
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
