#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <pthread.h>
#include <semaphore.h>

sem_t sem;

static void *thread_func(void *arg)
{
    int *n;
    n = (int *)malloc(sizeof(int));
    char *buffer = (char *)arg;
    sem_wait(&sem);
    *n = printf("%s\n", buffer);
    sem_post(&sem);
    return n;
}


int main(int argc, char *argv[])
{
    int i;
    char buffer[1024];
    pthread_t th;
    void *ret;

    if (sem_init(&sem, 0, 1) == -1) {
        perror("sem_init");
        exit(-1);
    }

    sem_wait(&sem);
    snprintf(buffer, 1024, "%d", argc);
    for (i = 1; i < argc; ++i)
        strcat(buffer, argv[i]);
    sem_post(&sem);

    pthread_create(&th, NULL, thread_func, buffer);
    pthread_join(th, &ret);
    printf("return code: %d\n", *(int *)ret);
    free(ret);

    return 0;
}