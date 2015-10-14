/*
 * CS511 Asxsignment #2: Using Semaphores for Buffer Synchronization (Part 1)
 * Description: In this version, needless schedulings will happen. Because
 *              the initial value of semaphore spaces is CUBF_CAPACITY,
 *              every time write a line to the buffer, more than one spaces
 *              are occupied but in P operation semaphore spaces only
 *              decrease by 1. To eliminate it, semaphore spaces need to be
 *              decreased by the length of line, that is to say do lenght of
 *              line times P operations.
 * Author:  Xiakun Lu
 * Email:   xlu9@stevens.edu
 * Data:    13/10/2015
 */
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <semaphore.h>
#include <string.h>

#include "cbuf.h"

#define HANDLE_ERROR(msg) \
    do { \
        perror(msg); \
        exit(EXIT_FAILURE); \
    } while (0)

typedef struct {
    FILE *f;
    useconds_t time;
} _thread_arg;

static void *fill_func(void *argument);
static void *drain_func(void *argument);

sem_t sem, spaces, items;

int main(int argc, char *argv[])
{
    char *src, *dst;
    FILE *fsrc, *fdst;
    int fill_sleep, drain_sleep;
    pthread_t thread_fill, thread_drain;
    _thread_arg fill_arg, drain_arg;

    if (argc != 5) {
        fprintf(stderr, "usage: ./transfer1 <input file> <output file> <fill sleep time> <drain sleep time>\n");
        exit(EXIT_FAILURE);
    }

    src = argv[1];
    dst = argv[2];
    fill_sleep = atoi(argv[3]);
    drain_sleep = atoi(argv[4]);

    if ((fsrc = fopen(src, "r")) == NULL)
        HANDLE_ERROR("fopen source");
    if ((fdst = fopen(dst, "w")) == NULL)
        HANDLE_ERROR("fopen destination");

    if (sem_init(&sem, 0, 1) == -1)
        HANDLE_ERROR("sem_init");
    if (sem_init(&spaces, 0, CBUF_CAPACITY) == -1)
        HANDLE_ERROR("sem_init spaces");
    if (sem_init(&items, 0, 0) == -1)
        HANDLE_ERROR("sem_init items");

    fill_arg.f = fsrc;
    fill_arg.time = fill_sleep;
    drain_arg.f = fdst;
    drain_arg.time = drain_sleep;

    printf("buffer size = %d\n", CBUF_CAPACITY);
    cbuf_init();

    if (pthread_create(&thread_fill, NULL, fill_func, &fill_arg) != 0)
        HANDLE_ERROR("pthread_create fill");
    if (pthread_create(&thread_drain, NULL, drain_func, &drain_arg) != 0)
        HANDLE_ERROR("pthread_create drain");

    if (pthread_join(thread_fill, NULL) != 0)
        HANDLE_ERROR("pthread_join fill");
    if (pthread_join(thread_drain, NULL) != 0)
        HANDLE_ERROR("pthread_join drain");

    if (sem_destroy(&sem) == -1)
        HANDLE_ERROR("sem_destroy");
    if (sem_destroy(&spaces) == -1)
        HANDLE_ERROR("sem_destroy spaces");
    if (sem_destroy(&items) == -1)
        HANDLE_ERROR("sem_destroy items");

    cbuf_terminate();
    fclose(fsrc);
    fclose(fdst);
    return 0;
}

static void *
fill_func(void *argument)
{
    _thread_arg *arg;
    int *ret;
    FILE *f;
    size_t line_len;
    char *line, *line_t;
    char *quit = "QUIT";
    ssize_t readn, readn_t;
    useconds_t sleep_time;
    int space_remain, len_copy_in;

    arg = (_thread_arg *)argument;
    f = arg->f;
    sleep_time = arg->time;

    if ((ret = (int *)malloc(sizeof(int))) == NULL)
        HANDLE_ERROR("fill_thread: malloc");
    *ret = 0;

    line = NULL;
    while (1) {
        readn_t = readn = getline(&line, &line_len, f);
        line_t = line;
        if (readn == -1) {
            readn_t = strlen(quit);
            line_t = quit;
        }
        if (sem_wait(&spaces) == -1)
            HANDLE_ERROR("sem_wait spaces");
        while (1) {
            if (usleep(sleep_time) == -1)
                HANDLE_ERROR("usleep");
            if (sem_wait(&sem) == -1)
                HANDLE_ERROR("sem_wait");

            space_remain = cbuf_space_available();
            if (space_remain < readn_t + 1) {
                printf("fill thread: could not write [%s] -- not enough space (%d)\n", line_t, space_remain);
                if (sem_post(&sem) == -1)
                    HANDLE_ERROR("sem_post");
            } else {
                len_copy_in = cbuf_copy_in(line_t);
                printf("fill thread: wrote [%s] into buffer (nwritten=%d)\n", line_t, len_copy_in);
                if (sem_post(&sem) == -1)
                    HANDLE_ERROR("sem_post");
                break;
            }
        }
        if (sem_post(&items) == -1)
            HANDLE_ERROR("sem_post items");
        if (readn == -1)
            break;
    }

    free(line);
    return (void *)ret;
}

static void *
drain_func(void *argument)
{
    _thread_arg *arg;
    int *ret;
    FILE *f;
    char *line;
    useconds_t sleep_time;
    int len_copy_out;

    arg = (_thread_arg *)argument;
    f = arg->f;
    sleep_time = arg->time;

    if ((ret = (int *)malloc(sizeof(int))) == NULL)
        HANDLE_ERROR("drain_thread: malloc");
    *ret = 0;

    if ((line = (char *)malloc(sizeof(char) * CBUF_CAPACITY)) == NULL)
        HANDLE_ERROR("drain_thread: malloc line");

    if (sem_wait(&items) == -1)
        HANDLE_ERROR("sem_wait items");
    while (1) {
        if (usleep(sleep_time) == -1)
            HANDLE_ERROR("usleep");
        if (sem_wait(&sem) == -1)
            HANDLE_ERROR("sem_wait");

        if (!cbuf_data_is_available()) {
            printf("drain thread: no new string in buffer\n");
            if (sem_post(&sem) == -1)
                HANDLE_ERROR("sem_post");
        } else {
            len_copy_out = cbuf_copy_out(line);
            printf("drain thread: read [%s] from buffer (nread=%d)\n", line, len_copy_out);
            if (sem_post(&sem) == -1)
                HANDLE_ERROR("sem_post");
            if (strcmp(line, "QUIT") == 0)
                break;
            fwrite(line, len_copy_out - 1, 1, f);
        }
    }
    if (sem_post(&spaces) == -1)
        HANDLE_ERROR("sem_post sapces");

    free(line);
    return (void *)ret;
}