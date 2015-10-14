/*
 * CS511 Asxsignment #2: Using Semaphores for Buffer Synchronization (Part 0)
 * Author:  Xiakun Lu
 * Email:   xlu9@stevens.edu
 * Data:    13/10/2015
 */
#include <stdio.h>
#include <stdlib.h>

#include "cbuf.h"

#define HANDLE_ERROR(msg) \
    do { \
        perror(msg); \
        exit(EXIT_FAILURE); \
    } while (0)

void usage();

int main(int argc, char *argv[])
{
    FILE *in, *out;
    char *src, *dst;
    char *line;
    size_t buf_len;
    ssize_t readn;
    char buffer[CBUF_CAPACITY];
    int cbuf_n;

    if (argc < 3)
        usage();

    src = argv[1];
    dst = argv[2];

    if ((in = fopen(src, "r")) == NULL)
        HANDLE_ERROR("fopen source");
    if ((out = fopen(dst, "w")) == NULL)
        HANDLE_ERROR("fopen destination");

    line = NULL;
    cbuf_init();
    while ((readn = getline(&line, &buf_len, in)) != -1) {
        cbuf_copy_in(line);
        cbuf_n = cbuf_copy_out(buffer);
        fwrite(buffer, cbuf_n - 1, 1, out);
    }

    cbuf_terminate();
    free(line);
    fclose(in);
    fclose(out);
    return 0;
}

void
usage()
{
    fprintf(stderr, "usage: ./rw <input file> <output file>\n");
    exit(EXIT_FAILURE);
}