// If you want to modify the tiling size, uncomment:
#define kTileH   (28)
#define kTileW   (56)

// Tiling specification must be before the #include
// and 224 must be a multiple of the tiling size
#include "lib/cnn-krnl.h"

void ConvolveWQ(weight_t* weight, input_t* input, compute_t* C) {
#pragma HLS inline off
#pragma HLS pipeline
  conv_w: for (int w = 0; w < kTileW; ++w) {
#pragma HLS unroll
    compute_t c = 0;
    conv_q: for (int q = 0; q < kKernel; ++q) {
#pragma HLS unroll
      c += weight[q] * input[w+q];
    }
    C[w] += c;
  }
}

void ConvolveH(weight_t* weight, input_t* input, compute_t* C) {
#pragma HLS inline off
  conv_h: for (int h = 0; h < kTileH; ++h) {
#pragma HLS pipeline
    ConvolveWQ(weight, &input[h*(kTileW+kKernel-1)], &C[h*kTileW]);
  }
}

void CnnKernel_YourCode(
    const input_g_t *input_g, const weight_g_t *weight_g,
    const bias_g_t  *bias_g,        output_g_t *output_g) {

  static input_t   input [kNum][kTileH+kKernel-1][kTileW+kKernel-1];
  static weight_t  weight[kNum][kNum][kKernel][kKernel];
  static bias_t    bias  [kNum];
  static output_t  output[kNum][kTileH/2][kTileW/2];

  static compute_t C[kTileH][kTileW];

  compute_t zero = 0;

  // TODO:  You may want to add array partitioning here, e.g.:
  // #pragma HLS array_partition variable=input dim=3 factor=5 cyclic

  #pragma HLS array_partition variable=weight dim=4 complete
  #pragma HLS array_partition variable=input  dim=3 complete
  #pragma HLS array_partition variable=C      dim=0 complete

  // Read the whole arrays from memory to device
  read_weight_from_memory(weight_g, weight);
  read_bias_from_memory  (bias_g,   bias);

  main_loop_tile_h:
  for (int hh = 0; hh < kImSize; hh += kTileH) {

    main_loop_tile_w:
    for (int ww = 0; ww < kImSize; ww += kTileW) {

      // Read input[j][h][w] = Input(j, hh + h, ww + w);
      read_input_from_memory(hh, ww, input_g, input);

      main_loop_i:
      for (int i = 0; i < kNum; ++i) {
        // TODO:  Please modify the code inside this loop :-)

        // You can use printf in software simulation for debugging
        fprintf(stderr, "Finished %d%% channel(s) #%d/#%d\r",
                100*i/kNum, i, kNum);

        // Set bias
        bias_t b = bias[i];
        bias_h: for (int h = 0; h < kTileH; ++h) {
          bias_w: for (int w = 0; w < kTileW; ++w) {
            C[h][w] = b;
          }
        }

        // Convolution
        conv:
        conv_j: for (int j = 0; j < kNum; ++j) {
          conv_p: for (int p = 0; p < kKernel; ++p) {
#pragma HLS pipeline
            ConvolveH((weight_t*) weight[i][j][p], (input_t*) input[j][p], (compute_t*) C);
          }
        }

        // ReLU + Max pooling
        relu_maxpool:
        relu_maxpool_h: for (int h = 0; h < kTileH/2; ++h) {
          relu_maxpool_w: for (int w = 0; w < kTileW/2; ++w) {
            output[i][h][w] = max(zero, max(
                max(C[h * 2][w * 2    ], C[h * 2 + 1][w * 2    ]),
                max(C[h * 2][w * 2 + 1], C[h * 2 + 1][w * 2 + 1])));
          }
        }
      }

      // Write Output(i, hh/2 + h, ww/2 + w) = output[i][h][w];
      write_output_to_memory(hh, ww, output_g, output);

      fprintf(stderr, "Computation for tile (%d, %d) is completed.\n",
              hh, ww) ;
    }
  }

}
