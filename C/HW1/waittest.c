#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

int main(int argc, char *argv[])
{
    int pid = fork();
    if (pid < 0) exit(1);
    else if (pid == 0) {
        exit(12);
    } else {
        int p, stat;
        while((p = wait(&stat)) > 0) {
            if (WIFEXITED(stat)) {
                printf("exited, status=%d\n", WEXITSTATUS(stat));
            } else if (WIFSIGNALED(stat)) {
                printf("killed by signal %d\n", WTERMSIG(stat));
            } else if (WIFSTOPPED(stat)) {
                printf("stopped by signal %d\n", WSTOPSIG(stat));
            } else if (WIFCONTINUED(stat)) {
                printf("continued\n");
            }
        }
        printf("wait return: %d\n", p);
    }
    return 0;
}