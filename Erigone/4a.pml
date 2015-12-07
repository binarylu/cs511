#define NORTH   0
#define SOUTH   1
#define EAST    2
#define WEST    3

#define EMPTY   100
#define PASSED  101

byte launchPad = EMPTY;
byte intersection = EMPTY;
byte cartnum = 0;

proctype cart_cross(byte direction) {
    byte i = 0;
    byte cart[4];
    cart[0] = direction * 4 + 0;
    cart[1] = direction * 4 + 1;
    cart[2] = direction * 4 + 2;

    do
    :: i > 2 -> break
    :: else  ->
        /*printf("pid(%d): cart%d from %d crosses intersection.\n", _pid, cart[i], direction);*/
        
        /* arrive */
        atomic {
            launchPad == EMPTY ->
                launchPad = cart[i]
        }

        /* cross */
        atomic {
            intersection == EMPTY ->
                intersection = cart[i];
            launchPad = EMPTY;
            cartnum = cartnum + 1
        }
        
        assert(cartnum == 1);

        /* leave */
        atomic {
            intersection = EMPTY;
            cartnum = cartnum - 1
        }

        cart[i] = PASSED;

        i = i + 1
    od;

    i = 0;
    do
    :: i > 2 -> break;
    :: else  ->
        assert(cart[i] == PASSED);
        i = i + 1
    od
}

init {
    atomic {
        run cart_cross(NORTH);
        run cart_cross(SOUTH);
        run cart_cross(EAST);
        run cart_cross(WEST)
    }
    _nr_pr == 1 ->
        printf("All carts passed!\n");
    assert(cartnum == 0);
    printf("simulation passed assertion!\n");
}
