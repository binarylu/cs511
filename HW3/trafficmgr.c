#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>

#include "q.h"
#include "monitor.h"

#define HANDLE_ERROR(msg) \
    do { \
        perror(msg); \
        exit(EXIT_FAILURE); \
    } while (0)

static void *thread_func(void *argument);

pthread_barrier_t barrier;

int main(int argc, char *argv[])
{
    int i;
    pthread_t thread_n, thread_s, thread_e, thread_w;
    if (argc != 2) {
        fprintf(stderr, "usage: ./trafficmgr directions\n");
        exit(EXIT_FAILURE);
    }

    q_init();
    monitor_init();
    for (i = 0; i < strlen(argv[1]); ++i) {
        if (argv[1][i] != 'n' && argv[1][i] != 'w' &&
            argv[1][i] != 's' && argv[1][i] != 'e')
            continue;
        q_putCart(argv[1][i]);
    }
    q_print('n');
    q_print('w');
    q_print('s');
    q_print('e');

    if (pthread_barrier_init(&barrier, NULL, 4) != 0)
        HANDLE_ERROR("pthread_barrier_init");

    if (pthread_create(&thread_n, NULL, thread_func, "n") != 0)
        HANDLE_ERROR("pthread_create north");
    if (pthread_create(&thread_s, NULL, thread_func, "s") != 0)
        HANDLE_ERROR("pthread_create north");
    if (pthread_create(&thread_e, NULL, thread_func, "e") != 0)
        HANDLE_ERROR("pthread_create north");
    if (pthread_create(&thread_w, NULL, thread_func, "w") != 0)
        HANDLE_ERROR("pthread_create north");

    if (pthread_join(thread_n, NULL) != 0)
        HANDLE_ERROR("pthread_join north");
    if (pthread_join(thread_s, NULL) != 0)
        HANDLE_ERROR("pthread_join south");
    if (pthread_join(thread_e, NULL) != 0)
        HANDLE_ERROR("pthread_join east");
    if (pthread_join(thread_w, NULL) != 0)
        HANDLE_ERROR("pthread_join west");

    q_shutdown();
    monitor_shutdown();

    (void) pthread_barrier_destroy(&barrier);

    return 0;
}

static void *thread_func(void *argument) {
    char *arg = (char *)argument;
    char direction = arg[0];
    struct cart_t *cart;

    fprintf(stderr, "thread for direction %c starts\n", direction);
    pthread_barrier_wait(&barrier);

    cart = q_getCart(direction);
    while (cart != NULL) {
        fprintf(stderr, "thread for direction %c gets cart %i\n", direction, cart->num);
        monitor_arrive(cart);
        monitor_cross(cart);
        monitor_leave(cart);
        free(cart);
        cart = q_getCart(direction);
    }
    monitor_finish(direction);

    pthread_barrier_wait(&barrier);
    fprintf(stderr, "thread for direction %c exits\n", direction);
    return 0;
}
