#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/select.h>

#ifndef PIPE_MAX
#define PIPE_MAX 1024
#endif

int isPrime(int num);
int deleteFromArray(int fdreads[], int length, int fd);
int childHandle(int fd, int bottom, int top);
void parentHandle(int *fdreads, int fd_num);

int main(int argc, char *argv[])
{
    int pid;
    int i;
    int fd[2];
    int fdreads[PIPE_MAX];
    int pipenum = 0, fifonum = 0;

    for (i = 0; i < argc - 1; ++i) {
//        if (i % 2 == 0) {
            if (pipe(fd) == -1) {
                perror("pipe filed!");
                break;
            }
//        }
        pid = fork();
        if (pid <= 0) {
            close(fd[0]);
            break;
        }
        else {
            close(fd[1]);
            fdreads[pipenum++] = fd[0];
        }
    }

    if (pid < 0) {
        fprintf(stderr, "Fork failed!\n");
        exit(-1);
    } else if (pid == 0) { /* child */
        int bottom = i == 0 ? 2 : atoi(argv[i]) + 1;
        int top = atoi(argv[i + 1]);
        printf("child %d: bottom=%d, top=%d, fd: %d\n", getpid(), bottom, top, fd[1]);
        int ret = childHandle(fd[1], bottom, top);
        close(fd[1]);
        printf("child over\n");
    } else { /* parent */
        parentHandle(fdreads, pipenum);
        int stat;
        while((pid = wait(&stat)) > 0) { 
            printf("child %d terminated\n", pid); 
        }
    }

    return 0;
}

int isPrime(int num) {
    int i;
    for (i = 2; i <= num / 2; ++i) {
        if (num % i == 0) return 0;
    }
    return 1;
}

int deleteFromArray(int fdreads[], int length, int fd) {
    int i, j = 0;
    int ret = length;
    for (i = 0; i < length; ++i) {
        if (fdreads[i] != fd) {
            fdreads[j] = fdreads[i];
            ++j;
        } else {
            --ret;
        }
    }
    return ret;
}

int childHandle(int fd, int bottom, int top) {
    int num, ret = 0;
    for (num = bottom; num <= top; ++num) {
        if (isPrime(num)) {
            ++ret;
            printf("child %d: fd: %d, primer: %d\n", getpid(), fd, num);
            write(fd, &num, sizeof(int));
        }
    }
    return ret;
}

void parentHandle(int *fdreads, int fd_num) {
    int i, n, prime, fd_max;

    fd_set fds;
    FD_ZERO(&fds);
    for (i = 0; i < fd_num; i++) {
        FD_SET(fdreads[i], &fds);
    }
    while (1) {
        fd_max = fdreads[fd_num - 1];
        n = select(fd_max + 1, &fds, NULL, NULL, NULL);
        printf("select return: %d, fd_num: %d\n", n, fd_num);

        for (i = 0; i <= fd_max; ++i) {
            if (FD_ISSET(i, &fds)) {
                int n = read(i, &prime, sizeof(int));
                if (n < 0) {
                    perror("read");
                } else if (n == 0) {
                    close(i);
                    fd_num = deleteFromArray(fdreads, fd_num, i);
                } else {
                    printf("parent: fd: %d, prime: %d, size: %d\n", i, prime, n);
                }
            }
        }
        FD_ZERO(&fds);
        for (i = 0; i < fd_num; i++) {
            FD_SET(fdreads[i], &fds);
        }
        if (fd_num == 0) break;
    }
}