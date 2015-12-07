byte b = 1;
byte a = 4;
byte c = 2;
byte disc;

active [5] proctype real_root() {
    disc = b*b - 4*a*c;
    if
    :: disc > 0 ->
        printf("two real roots\n")
    :: disc < 0 ->
        printf("no real roots\n")
    :: disc == 0 ->
        printf("duplicate real roots\n")
    fi
}

/*init {
    b = 1;
    a = 4;
    c = 2;
    run real_root();
}*/
