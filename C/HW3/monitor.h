#ifndef __MONITOR_H__
#define __MONITOR_H__

#include <unistd.h>
#include <pthread.h>

#include <stdio.h>
#include <string.h>

#include "cart.h"
#include "q.h"

#define CROSS_TIME 2

extern int monitor_init();
extern void monitor_arrive(struct cart_t *cart);
extern void monitor_cross(struct cart_t *cart);
extern void monitor_leave(struct cart_t *cart);
extern void monitor_finish(char direction);
extern void monitor_shutdown();

#endif /* __MONITOR_H__ */
