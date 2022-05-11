#include "lib/macros.cuh"
#include "kernel.h"

#define TILE_WIDTH 32
#define BLOCK_J 8

__global__ void cnn_gpu(float* input,
    float* weight,
    float* bias, float* output)
{
  // Input size: 256 x 224 x 224
  // OUtput size: 256 x 112 x 112
  // GPU specs: 16 SMs, 32 blocks/SM, 2048 threads/SM (32K threads total)

  const int N_CHANNELS = kNum / gridDim.x;

  int bi = blockIdx.x * N_CHANNELS;
  const int tr = threadIdx.y;
  const int tc = threadIdx.x;

  __shared__ float C[TILE_WIDTH][TILE_WIDTH];
  __shared__ float weightShared[BLOCK_J][kKernel][kKernel];
  __shared__ float inputShared[BLOCK_J][TILE_WIDTH+kKernel-1][TILE_WIDTH+kKernel-1];
  
  for (int i = bi; i < bi+N_CHANNELS; i++) {
    for (int h = tr; h < kImSize; h += TILE_WIDTH) {
      for (int w = tc; w < kImSize; w += TILE_WIDTH) {
        // Bias
        float reg = bias[i];

        // Convolution
        for (int bj = 0; bj < kNum; bj += BLOCK_J) {
          for (int j = 0; j < BLOCK_J; j++) {
            inputShared[j][tr][tc] = input(bj+j, h, w);
            if (tc < kKernel-1) {
              inputShared[j][tr][tc+TILE_WIDTH] = input(bj+j, h, w+TILE_WIDTH);
            }
            if (tr < kKernel-1) {
              inputShared[j][tr+TILE_WIDTH][tc] = input(bj+j, h+TILE_WIDTH, w);
            }
            if (tr < kKernel-1 && tc < kKernel-1) {
              inputShared[j][tr+TILE_WIDTH][tc+TILE_WIDTH] = input(bj+j, h+TILE_WIDTH, w+TILE_WIDTH);
            }
            if (tr < kKernel && tc < kKernel) {
              weightShared[j][tr][tc] = weight(i, bj+j, tr, tc);
            }
          }
          __syncthreads();

          for (int j = 0; j < BLOCK_J; j++) {
            for (int p = 0; p < kKernel; ++p) {
              for (int q = 0; q < kKernel; ++q) {
                reg += weightShared[j][p][q] * inputShared[j][tr + p][tc + q];
              }
            }
          }
          __syncthreads();
        }

        // ReLU
        C[tr][tc] = max(0.f, reg);

        // Max pooling
        __syncthreads();
        if (tr % 2 == 0 && tc % 2 == 0) {
          output(i, h/2, w/2) = max(
              max(C[tr][tc    ], C[tr + 1][tc    ]),
              max(C[tr][tc + 1], C[tr + 1][tc + 1]));
        }
      }
    }
  }
}
