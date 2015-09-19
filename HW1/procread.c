#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>

#define FIFO_NAME "myfifo"
#define BUF_SIZE 1024

int main(int argc, char *argv[])
{
    int fd;
    char buf[BUF_SIZE];

    umask(0);
    fd = open(FIFO_NAME, O_RDONLY);
    read(fd, buf, BUF_SIZE);
    printf("Read content: %s\n", buf);
    close(fd);
    return 0;
}