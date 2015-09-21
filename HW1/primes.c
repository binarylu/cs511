/*
 * CS511 Asxsignment #1: Practice with UNIX Process Mechanisms
 * Author: Xiakun Lu
 * Email:  xlu9@stevens.edu
 * Data:   09/21/2015
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/select.h>
#include <sys/stat.h>

#ifndef PIPE_MAX
#define PIPE_MAX 255
#endif

#define NAME_BUF 256

typedef struct {
    int pid;
    int fd;
    int count;
} _childInfo;

int childHandle(int fd, int bottom, int top);
void parentHandle(_childInfo *children, int childnum);
void usage();
int isPrime(int num);
int deleteFromArray(int fd, _childInfo *children, int length);
int setPrimeCnt(int pid, _childInfo *children, int childnum);
int getPrimeCnt(int pid, _childInfo *children, int childnum);
int getChildidByfd(int fd, _childInfo *children, int childnum);

int main(int argc, char *argv[])
{
    int pid;
    int i;
    int fd[2];
    int childnum = 0;
    char fifo_name[NAME_BUF];
    _childInfo *children;

    if (argc < 2) {
        usage();
    } else if (argc - 1 > PIPE_MAX) {
        fprintf(stderr, "Too many arguments, at most %d\n", PIPE_MAX);
        exit(EXIT_FAILURE);
    }

    /* for parent process to record prime number that each child process finds and fd of each child process*/
    children = (_childInfo *)malloc(sizeof(_childInfo) * (argc - 1));

    /* 
     * Create child processes, pipes and fifos
     */
    for (i = 0; i < argc - 1; ++i) {
        if (i % 2 == 0) { /* Odd-numbered child process use pipe, i = 0 means the first child process */
            if (pipe(fd) == -1) {
                perror("pipe failed!");
                exit(EXIT_FAILURE);
            }
        } else { /* Even-numbered child process use FIFO */
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
        } else if (pid == 0) { /* child process */
            if (i % 2 == 0) { /* for odd-numbered child process close read pipe fd */
                close(fd[0]);
            } else { /* for even-numbered child process open a write FIFO descriptor */
                if ((fd[1] = open(fifo_name, O_WRONLY)) == -1) {
                    perror("open fifo write failed");
                    exit(EXIT_FAILURE);
                }
                /* remove FIFO files after parent process exiting */
                unlink(fifo_name);
            }
            break; /* avoid the child process to fork again */
        } else { /* parent process */
            if (i % 2 == 0) { /* parent process close the write pipe fd */
                close(fd[1]);
                children[childnum++].fd = fd[0];
            } else { /* parent process open a read FIFO descriptor */
                if ((children[childnum++].fd = open(fifo_name, O_RDONLY)) == -1) {
                    perror("open fifo read failed"); 
                    exit(EXIT_FAILURE);
                }
            }
            /* initialize the prime number that each child process finds */
            children[i].pid = pid;
            children[i].count = 0;
        }
    }

    if (pid < 0) {
        perror("fork error");
        exit(EXIT_FAILURE);
    } else if (pid == 0) { /* child */
        int ret;
        int bottom = i == 0 ? 2 : atoi(argv[i]) + 1;
        int top = atoi(argv[i + 1]);

        /* return the number of primes the child process has found */
        ret = childHandle(fd[1], bottom, top);
        exit(ret);
    } else { /* parent */
        parentHandle(children, childnum);
    }
    free(children);

    return 0;
}

int childHandle(int fd, int bottom, int top) {
    int i, size, ret = 0;
    int buf[2];
    printf("child %d: bottom=%d, top=%d\n", getpid(), bottom, top);
    buf[0] = getpid();
    for (i = bottom; i <= top; ++i) {
        if (isPrime(i)) { /* write pipe/FIFO after finding a prime */
            ++ret;
            buf[1] = i;
            if ((size = write(fd, buf, sizeof(int) * 2)) == -1) {
                perror("write");
            }
        }
    }
    close(fd);
    return ret; /* return the prime number it has found */
}

void parentHandle(_childInfo *children, int childnum) {
    int i, fdmax, fdnum = childnum;
    int buf[2];
    int ret;

    fd_set fds;
    while (1) {
        FD_ZERO(&fds);
        for (i = 0; i < fdnum; i++) {
            FD_SET(children[i].fd, &fds);
        }
        fdmax = children[fdnum - 1].fd;
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
                    exit(EXIT_FAILURE);
                } else if (n == 0) { /* child process closes its fd (zero indicates end of file) */
                    int pid, status, w;

                    if ((pid = getChildidByfd(i, children, childnum)) == -1) {
                        perror("can not find the child pid by fd");
                        exit(EXIT_FAILURE);
                    }

                    w = waitpid(pid, &status, 0);
                    if (w == -1) {
                        perror("waitpid");
                    }

                    if (WIFEXITED(status)) {
                        /* verify the received exit code is indeed the number of primes that the child produced */
                        int primenum = getPrimeCnt(pid, children, childnum);
                        if (WEXITSTATUS(status) == primenum) {
                            printf("child %d exited correctly\n", pid);
                        } else {
                            printf("child %d exited incorrectly: status:%d, get:%d\n", pid, WEXITSTATUS(status), primenum);
                        }
                    } else if (WIFSIGNALED(status)) {
                        printf("killed by signal %d\n", WTERMSIG(status));
                    } else if (WIFSTOPPED(status)) {
                        printf("stopped by signal %d\n", WSTOPSIG(status));
                    } else if (WIFCONTINUED(status)) {
                        printf("continued\n");
                    }

                    close(i);
                    /* delete this fd from the fd array */
                    fdnum = deleteFromArray(i, children, fdnum);
                    if (fdnum == 0) return;
                } else {
                    int childid = buf[0];
                    int prime = buf[1];
                    printf("%d is prime\n", prime);
                    /* record the prime number that each child process finds */
                    if (setPrimeCnt(childid, children, childnum) == -1) {
                        perror("can not find the child pid");
                        exit(EXIT_FAILURE);
                    }
                }
            }
        }
    }
}

void usage() {
    fprintf(stderr, "usage: ./primes <increasing positive integers>\n");
    exit(EXIT_FAILURE);
}

/* judge the num is a prime number */
int isPrime(int num) {
    int i;
    for (i = 2; i < num; ++i) {
        if (num % i == 0) return 0;
    }
    return 1;
}

/* delete a specified number from the array */
int deleteFromArray(int fd, _childInfo *children, int length) {
    int i, j = 0;
    int ret = length;
    for (i = 0; i < length; ++i) {
        if (children[i].fd != fd) {
            children[j] = children[i];
            ++j;
        } else {
            --ret;
        }
    }
    return ret;
}

/* given pid, increase its prime number count by 1 */
int setPrimeCnt(int pid, _childInfo *children, int childnum) {
    int i;
    for (i = 0; i < childnum; ++i) {
        if (pid == children[i].pid) {
            ++children[i].count;
            return 0;
        }
    }
    if (i == childnum) return -1;
    return 0;
}

/* given pid, return its prime number count */
int getPrimeCnt(int pid, _childInfo *children, int childnum) {
    int i;
    for (i = 0; i < childnum; ++i) {
        if (pid == children[i].pid) {
            return children[i].count;
        }
    }
    return 0;
}

/* given fd, return the child id that the fd is connected with */
int getChildidByfd(int fd, _childInfo *children, int childnum) {
    int i;
    for (i = 0; i < childnum; ++i) {
        if (fd == children[i].fd) {
            return children[i].pid;
        }
    }
    return -1;
}