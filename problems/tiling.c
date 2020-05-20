/*
* src is the input matrix
* dest is where the transposed matrix should be stored
* n is the number of rows and columns in the matrix of blocks
* m is the number of rows and columns in each individual block
* B is the tiling factor for the first level of transposing
* L is the tiling factor for the second level of transposing
*/
void transpose(int** src, int** dest, int n, int m, int B, int L) {
  for(int i1=0; i1<n; i1+=B) {
    for(int j1=0; j1<n; j1+=B) {
      for(int i2=i1; i2<n && i2<i1+B; i2++) {
        for(int j2=j1; j2<n && j2<j1+B; j2++) {
          // Transpose matrix of blocks
          dest[n*j1 + i1] = src[n*i1 + j1];

          // Allocate temporary memory for transposing within matrix
          int* tmp_src = (int*) malloc(m * m * sizeof(int));
          int* tmp_dest = dest[n*j1 + i1];
          memcpy(tmp_src, tmp_dest, m * m * sizeof(int));

          // Transpose each block by its elements
          for(int a=0; a<m; a+=L)
            for(int b=0; b<m; b+=L)
              for(int a1=a; a1<m && a<b+L; a+=L)
                for(int b1=b; b1<m && b1<b+L; b+=L)
                  tmp_dest[m*a1 + b1] = tmp_src[m*b1 + a1];
        }
      }
    }
  }
}
