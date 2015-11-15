#include "monitor.h"

static pthread_mutex_t mutex;
static pthread_cond_t interSection;

static char next;

int monitor_init() {
    int ret;
    if ((ret = pthread_mutex_init(&mutex, NULL)) != 0) {
        fprintf(stderr, "mutex init filed: %s\n", strerror(ret));
        return -1;
    }
    if ((ret = pthread_cond_init(&interSection, NULL)) != 0) {
        fprintf(stderr, "condition variable init failed: %s\n", strerror(ret));
        return -1;
    }
    next = 0;
    return 0;
}

void monitor_arrive(struct cart_t *cart) {
    fprintf(stderr, "Cart #%d from %c arrives at interSection\n", cart->num, cart->dir);
    pthread_mutex_lock(&mutex);
    while (next != 0 && next != cart->dir) {
        fprintf(stderr, "Cart #%d from %c waits before entering interSection\n", cart->num, cart->dir);
        pthread_cond_wait(&interSection, &mutex);
    }
    fprintf(stderr, "Cart #%d from %c is allowed to proceed into interSection\n", cart->num, cart->dir);
}

void monitor_cross(struct cart_t *cart) {
    int i;
    fprintf(stderr, "Cart #%d from %c enters interSection\n", cart->num, cart->dir);

    for (i = 0; i < CROSS_TIME; ++i) {
        fprintf(stderr, "cart #%d from %c is crossing the intersection\n", cart->num, cart->dir);
        sleep(0);
    }
}

void monitor_leave(struct cart_t *cart) {
    char p = cart->dir;
    do {
        switch (p) {
            case 'n': next = 'w'; break;
            case 'w': next = 's'; break;
            case 's': next = 'e'; break;
            case 'e': next = 'n'; break;
            default: next = 0;
        }
        p = next;
    } while (!q_cartIsWaiting(next) && p != cart->dir);

    fprintf(stderr, "cart #%d from %c leaves the intersection\n", cart->num, cart->dir);
    pthread_cond_broadcast(&interSection);
    if (next != cart->dir)
        fprintf(stderr, "thread for direction %c signals another, next cart is %c\n", cart->dir, next);
    else
        fprintf(stderr, "thread for direction %c signals aonther, no next cart\n", cart->dir);
    pthread_mutex_unlock(&mutex);
}

void monitor_finish(char direction) {
    pthread_mutex_lock(&mutex);
    if (next == direction) {
        char p = direction;
        do {
            switch (p) {
                case 'n': next = 'w'; break;
                case 'w': next = 's'; break;
                case 's': next = 'e'; break;
                case 'e': next = 'n'; break;
                default: next = 0;
            }
            p = next;
        } while (!q_cartIsWaiting(next) && p != direction);
        if (next != direction) {
            fprintf(stderr, "thread for direction %c signals another, next cart is %c\n", direction, next);
            pthread_cond_broadcast(&interSection);
        }
    }
    q_cartHasEntered(direction);
    fprintf(stderr, "Queue from %c finish\n", direction);
    pthread_mutex_unlock(&mutex);
}

void monitor_shutdown() {
    (void) pthread_mutex_destroy(&mutex);
    (void) pthread_cond_destroy(&interSection);
}
