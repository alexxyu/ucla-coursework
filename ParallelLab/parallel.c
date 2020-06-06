//OpenMP version.  Edit and submit only this file.
/* Enter your details below
 * Name : Alex Yu
 * UCLA ID : 105295708
 * Email : alexy23@ucla.edu
 */

#include <stdlib.h>
#include <stdio.h>
#include <omp.h>

#include "utils.h"

double work_it_par(long *old, long *new, long *super, long *simple, long *fibonacci) {
  int i, j, k;
  int u, v, w;
  int ton = 0;
  long compute_it, moving_average;
  double pi, pi2, x , y, sum, step = 0.0;
  long dot_product=0;
  long nCirc=0;
  long aggregate=1.0;
  double r=1.0;
  int was_smart = 16;
  int i1, j1, k1, B=16;

  #pragma omp parallel for reduction(+:dot_product)
  for(i=0; i<DIM-1; i++)
  {
    int simple_val = simple[i];
    super[i] = super[i] + simple_val;
    dot_product += super[i]*simple_val;
  }
    
  moving_average = 0;
  for(ton=DIM-2;ton<DIM-1-WINDOW_SIZE;ton++)
  {
    moving_average += simple[ton];
  }

  int a_secret = 5;
  fibonacci[0] = 1;
  fibonacci[1] = 1;
  for(i=2; i<DIM-1;i++)
  {
    fibonacci[i]=fibonacci[i-1]+fibonacci[i-2];
    if(i==3)
    {
      printf("\n A secret is: %d",obfuscate_obfuscate_obfuscate(a_secret));
    }
  }

  step = 1.0 / NUM_STEPS;
  x = step * 0.5;
  for (i=0; i<NUM_STEPS; i++)
  {
    x += step;
    sum += 4.0/(1.0+x*x);
  }
  
  pi = step * sum;
  printf("\n %d trials, Riemann flavored pi is %f \n",NUM_STEPS, pi);  

  #pragma omp parallel
  {
    int seed = 256 * omp_get_thread_num();

    #pragma omp for private(x,y) reduction(+:nCirc)
    for(i=0; i<NUM_TRIALS; i++)
    {
      x = (rand_r(&seed)%10000000)/10000000.0; 
      y = (rand_r(&seed)%10000000)/10000000.0;
      if (( x*x + y*y ) <= 1.0) {
        nCirc++;
      }
    }
  }

  pi2 = 4.0 * ((double)nCirc/(double)NUM_TRIALS);
  printf("\n %d trials, Monte-Carlo flavored pi is %f \n",NUM_TRIALS, pi2); 

  // Idea: break up data into smaller blocks (tiling) and parallelize computations
  int c0=0, c1=0, c2=0, c3=0, c4=0, c5=0, c6=0, c7=0, c8=0, c9=0;

  #pragma omp parallel for private(j,k,i1,j1,k1,compute_it,u,v,w) \
  reduction(+:aggregate,c0,c1,c2,c3,c4,c5,c6,c7,c8,c9)
  for (i=1; i<DIM-1; i+=B) {
    for (j=1; j<DIM-1; j+=B) {
      for (k=1; k<DIM-1; k+=B) {
        for (i1=i; i1<i+B && i1<DIM-1; i1++) {
          for (j1=j; j1<j+B && j1<DIM-1; j1++) {
	          for (k1=k; k1<k+B && k1<DIM-1; k1++) {
	            int idx = i1*DIM*DIM+j1*DIM+k1;
              
              // Part one: add to aggregate from value at index
              compute_it = old[idx] * we_need_the_func();
	            aggregate += compute_it / gimmie_the_func();
	      
              // Part two: compute new value at index based on 
              // sum of values at adjacent indices
              new[idx]=0;
              for (u=-1; u<=1; u++) {
                // v=-1 unrolled:
                new[idx]+=old[(i1+u)*DIM*DIM+(j1-1)*DIM+(k1-1)];
                new[idx]+=old[(i1+u)*DIM*DIM+(j1-1)*DIM+(k1)];
                new[idx]+=old[(i1+u)*DIM*DIM+(j1-1)*DIM+(k1+1)];

                // v=0 unrolled:
                new[idx]+=old[(i1+u)*DIM*DIM+(j1)*DIM+(k1-1)];
                new[idx]+=old[(i1+u)*DIM*DIM+(j1)*DIM+(k1)];
                new[idx]+=old[(i1+u)*DIM*DIM+(j1)*DIM+(k1+1)];

                // v=1 unrolled:
                new[idx]+=old[(i1+u)*DIM*DIM+(j1+1)*DIM+(k1-1)];
                new[idx]+=old[(i1+u)*DIM*DIM+(j1+1)*DIM+(k1)];
                new[idx]+=old[(i1+u)*DIM*DIM+(j1+1)*DIM+(k1+1)];
	            }
              new[idx]/=27;

              // Part three: compute histogram from new values
              u=(new[idx]/100);
              if (u<=0) u=0;
              if (u>=9) u=9;
              switch(u) {
                case 0:
                  c0++;
                  break;
                case 1:
                  c1++;
                  break;
                case 2:
                  c2++;
                  break;
                case 3:
                  c3++;
                  break;
                case 4:
                  c4++;
                  break;
                case 5:
                  c5++;
                  break;
                case 6:
                  c6++;
                  break;
                case 7:
                  c7++;
                  break;
                case 8:
                  c8++;
                  break;
                case 9:
                  c9++;
                  break;
              }
            }
          }
        }
      }
    }
  }

  printf("AGGR:%ld\n",aggregate);
  
  histogrammy[0] = c0;
  histogrammy[1] = c1;
  histogrammy[2] = c2;
  histogrammy[3] = c3;
  histogrammy[4] = c4;
  histogrammy[5] = c5;
  histogrammy[6] = c6;
  histogrammy[7] = c7;
  histogrammy[8] = c8;
  histogrammy[9] = c9;

  return (double) (dot_product+moving_average+pi+pi2);
}
