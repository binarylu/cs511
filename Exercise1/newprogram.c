#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
    int len = 0;
    char ch;
    char buf[3];
    int ret;

    ch = getchar();
    while (len < 3 && ch != '\n') {
        buf[len++] = ch;
        ch = getchar();
    }
    ret = atoi(buf);
    if (ret > 255) {
        perror("exit code can not be bigger than 255\n");
        exit(0);
    }
    return ret;
}
