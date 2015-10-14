BIN = transfer1
OBJ = transfer1.o \
	  cbuf.o

CFLAGS = -g -Wall -pedantic-errors
LIB = -pthread

$(BIN): $(OBJ)
	$(CC) -o $(BIN) $(CFLAGS) $(LIB) $^

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

.PHONY: clean
clean:
	-rm -fr transfer1 $(OBJ)