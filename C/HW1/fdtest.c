#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

int main(int argc, char *argv[])
{
    int fd = open("abc", O_RDONLY);
    int fd2 = dup(fd);
    int fd3 = open("abc", O_RDONLY);
    int n;
    char buf[1024];

    int pid = fork();
    if (pid < 0) {
        perror("fork");
    } else if (pid == 0) {
        sleep(2);
        printf("child fd: %d\n", fd2);
        while ((n = read(fd, buf, 1024)) > 0) {
            write(STDOUT_FILENO, buf, n);
        }
        if (n < 0) {
            perror("read failed");
        }
    } else {
        lseek(fd2, 9, SEEK_SET);
        close(fd2);
        dup2(fd3, fd2);
        printf("parent fd: %d\n", fd2);
        while ((n = read(fd2, buf, 1024)) > 0) {
            write(STDOUT_FILENO, buf, n);
        }
        if (n < 0) {
            perror("read failed");
        }
    }
    return 0;
}