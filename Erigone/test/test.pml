proctype A(byte state; short foo) {
    (state == 1) -> state = foo;
    printf("%d\n", state);
}

init {
    run A(1, 3)
}
