#include <termios.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>

const int BUFF_SIZE = 32;
const int CR_CODE = 0x0D;
const int LF_CODE = 0x0A;
const int EOF_CODE = 0x04;

struct termios currmode;

void restore_terminal_mode() {
    tcsetattr(0, TCSANOW, &currmode);
}

void set_terminal_mode() {

    int res;
    struct termios newmode;

    res = tcgetattr(0, &newmode);

    memcpy(&currmode, &newmode, sizeof(newmode));
    newmode.c_iflag = ISTRIP;	/* only lower 7 bits	*/
    newmode.c_oflag = 0;		/* no processing	*/
    newmode.c_lflag = 0;		/* no processing	*/
    
    tcsetattr(0, TCSANOW, &newmode);

}

int main() {

    set_terminal_mode();

    char buffer[BUFF_SIZE];
    int read_size;

    int exit_flag = 0;
    while((read_size = read(0, buffer, BUFF_SIZE)) >= 0 && !exit_flag) {

        for(int i=0; i<read_size && !exit_flag; i++) {
            char c = buffer[i];

            if(c == EOF_CODE) {
                exit_flag = 1;
            } else if(c == LF_CODE || c == CR_CODE) {
                if(write(1, &(CR_CODE), 1) < 0) {
                    fprintf(stderr, "Write failed: %s\n", strerror(errno));
                    exit(1);
                }
                
                if(write(1, &(LF_CODE), 1) < 0) {
                    fprintf(stderr, "Write failed: %s\n", strerror(errno));
                    exit(1);
                }
            } else {
                if(write(1, &c, 1) < 0) {
                    fprintf(stderr, "Write failed: %s\n", strerror(errno));
                    exit(1);
                }
            }
        }

    }

    if(read_size < 0) {
        fprintf(stderr, "Read failed: %s\n", strerror(errno));
        exit(1);
    }

    restore_terminal_mode();
    exit(0);

}
