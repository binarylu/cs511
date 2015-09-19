#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/select.h>
#include <sys/stat.h>

#ifndef PIPE_MAX
#define PIPE_MAX 1024
#endif

#define NAME_BUF 256

typedef struct {
    int pid;
    int count;
} _pidCnt;

int childHandle(int fd, int bottom, int top);
void parentHandle(int *fdreads, int childnum, _pidCnt *pidcnt);
void usage();
int isPrime(int num);
int deleteFromArray(int fdreads[], int length, int fd);
int setPidCnt(int pid, _pidCnt *pidcnt, int length);
int getPidCnt(int pid, _pidCnt *pidcnt, int length);

int main(int argc, char *argv[])
{
    if (argc < 2) {
        usage();
    }
    int pid;
    int i;
    int fd[2];
    int fdreads[PIPE_MAX];
    int childnum = 0;

    char fifo_name[NAME_BUF];
    _pidCnt *pidcnt = (_pidCnt *)malloc(sizeof(_pidCnt) * (argc - 1));

    for (i = 0; i < argc - 1; ++i) {
        if (i % 2 == 0) {
            if (pipe(fd) == -1) {
                perror("pipe failed!");
                exit(EXIT_FAILURE);
            }
        } else {
            snprintf(fifo_name, NAME_BUF, "fifo_%d", i);
            if (mkfifo(fifo_name, 0666) == -1) {
                perror("mkfifo failed!");
                exit(EXIT_FAILURE);
            }
        }
        pid = fork();
        if (pid < 0) {
            perror("fork error");
            exit(EXIT_FAILURE);
        } else if (pid == 0) {
            if (i % 2 == 0) {
                close(fd[0]);
            } else {
                if ((fd[1] = open(fifo_name, O_WRONLY)) == -1) {
                    perror("open fifo failed");
                    exit(EXIT_FAILURE);
                }
                unlink(fifo_name);
            }
            break;
        } else {
            if (i % 2 == 0) {
                close(fd[1]);
                fdreads[childnum++] = fd[0];
            } else {
                if ((fdreads[childnum++] = open(fifo_name, O_RDONLY)) == -1) {
                    perror("open fifo failed"); 
                    exit(EXIT_FAILURE);
                }
            }
            pidcnt[i].pid = pid;
            pidcnt[i].count = 0;
        }
    }

    if (pid < 0) {
        perror("fork error");
        exit(EXIT_FAILURE);
    } else if (pid == 0) { /* child */
        int bottom = i == 0 ? 2 : atoi(argv[i]) + 1;
        int top = atoi(argv[i + 1]);
        printf("child %d: bottom=%d, top=%d\n", getpid(), bottom, top);
        int ret = childHandle(fd[1], bottom, top);
        exit(ret);
    } else { /* parent */
        parentHandle(fdreads, childnum, pidcnt);
        int stat;
        while((pid = wait(&stat)) > 0) { 
            if (WIFEXITED(stat)) {
                if (WEXITSTATUS(stat) == getPidCnt(pid, pidcnt, childnum)) {
                    printf("child %d exited correctly\n", pid);
                } else {
                    printf("child %d exited incorrectly\n", pid);
                }
            } else if (WIFSIGNALED(stat)) {
                printf("killed by signal %d\n", WTERMSIG(stat));
            } else if (WIFSTOPPED(stat)) {
                printf("stopped by signal %d\n", WSTOPSIG(stat));
            } else if (WIFCONTINUED(stat)) {
                printf("continued\n");
            }

        }
    }

    return 0;
}

int childHandle(int fd, int bottom, int top) {
    int num, ret = 0;
    int buf[2];
    buf[0] = getpid();
    for (num = bottom; num <= top; ++num) {
        if (isPrime(num)) {
            ++ret;
            buf[1] = num;
            write(fd, buf, sizeof(int) * 2);
        }
    }
    close(fd);
    return ret;
}

void parentHandle(int *fdreads, int childnum, _pidCnt *pidcnt) {
    int i, fdmax, fdnum = childnum;
    int buf[2];
    int ret;

    fd_set fds;
    FD_ZERO(&fds);
    for (i = 0; i < fdnum; i++) {
        FD_SET(fdreads[i], &fds);
    }
    while (1) {
        fdmax = fdreads[fdnum - 1];
        ret = select(fdmax + 1, &fds, NULL, NULL, NULL);
        if (ret == -1) {
            perror("select error");
            exit(EXIT_FAILURE);
        }

        for (i = 0; i <= fdmax; ++i) {
            if (FD_ISSET(i, &fds)) {
                int n = read(i, &buf, sizeof(int) * 2);
                if (n < 0) {
                    perror("read");
                } else if (n == 0) {
                    close(i);
                    fdnum = deleteFromArray(fdreads, fdnum, i);
                } else {
                    int childid = buf[0];
                    int prime = buf[1];
                    printf("%d is prime\n", prime);
                    if (setPidCnt(childid, pidcnt, childnum) == -1) {
                        perror("can not find the child pid in the pidcnt");
                        exit(EXIT_FAILURE);
                    }
                }
            }
        }
        FD_ZERO(&fds);
        for (i = 0; i < fdnum; i++) {
            FD_SET(fdreads[i], &fds);
        }
        if (fdnum == 0) break;
    }
}

void usage() {
    fprintf(stderr, "usage: ./primes <increasing positive integers>\n");
    exit(EXIT_FAILURE);
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

int setPidCnt(int pid, _pidCnt *pidcnt, int length) {
    int i;
    for (i = 0; i < length; ++i) {
        if (pid == pidcnt[i].pid) {
            ++pidcnt[i].count;
            return 0;
        }
    }
    if (i == length) return -1;
    return 0;
}

int getPidCnt(int pid, _pidCnt *pidcnt, int length) {
    int i;
    for (i = 0; i < length; ++i) {
        if (pid == pidcnt[i].pid) {
            return pidcnt[i].count;
        }
    }
    return 0;
}