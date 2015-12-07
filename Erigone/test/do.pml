int x;
int y;

proctype GCD() {
    int a = x, b = y;
    do
    :: a > b   -> a = a - b
    :: b > a   -> b = b - a
    :: a == b  -> break
    od;
    printf("GCD(%d, %d) = %d\n", x, y, a);
}

init {
    x = 32;
    y = 128;
    run GCD();
}
