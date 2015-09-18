#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

int main(int argc, char *argv[])
{
    int pid;

    pid = fork();
    if (pid < 0) {
        perror("fork error");
        exit(1);
    } else if (pid == 0) {
        /* child */
        /* exit code must be 0 ~ 255 */
        execl("./newprogram", "", NULL);
        printf("never\n");
    } else {
        /* parent */
        int status;
        int w = waitpid(pid, &status, 0);
        if (w == -1) {
            perror("waitpid");
            exit(2);
        }
        if (WIFEXITED(status)) {
            printf("exited, status=%d\n", WEXITSTATUS(status));
        } else if (WIFSIGNALED(status)) {
            printf("killed by signal %d\n", WTERMSIG(status));
        } else if (WIFSTOPPED(status)) {
            printf("stopped by signal %d\n", WSTOPSIG(status));
        } else if (WIFCONTINUED(status)) {
            printf("continued\n");
        }
        printf("Parent process: child exit: %d\n", status);
    }

    return 0;
}