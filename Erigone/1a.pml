byte n;

proctype incrementUpToN() {
    byte N = 5;
    byte tmp;
    byte i = 1;

    do
    :: i > N -> break
    :: else  ->
        tmp = n;
        n = tmp + 1;
        printf("process %d, i=%d: n changed from %d to %d\n", _pid, i , tmp, n);
        i = i + 1
    od
}

init {
    n = 0;
    atomic {
        run incrementUpToN();
        run incrementUpToN()
    }
    (_nr_pr == 1) -> printf("at end of simulation n = %d\n", n);
    assert(5 <= n && n <= 10);
    printf("simulation passed assertion!\n");
}
