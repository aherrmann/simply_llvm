#include <stdlib.h>
#include <stdio.h>

extern const int __main_argc;
extern int __entry_point(int *);

const char *usage =
    "Usage: %s ARGS...\n"
    "\n"
    "Expects exactly %d integer arguments.\n";

int main(int argc, char **argv) {
    /* Handle wrong number of arguments */
    if (argc - 1 != __main_argc) {
        fprintf(stderr, usage, argv[0], __main_argc);
        fprintf(stderr, "\nError: %d arguments passed.\n", argc - 1);
        exit(1);
    }

    /* Convert argument list */
    int arglist[__main_argc];
    //int *arglist = malloc(sizeof(int) * __main_argc);
    int i;
    for (i = 0; i < __main_argc; ++i) {
        char *argstr = argv[i + 1];
        arglist[i] = atoi(argstr);
    }

    /* Call entry-point */
    int result;
    result = __entry_point(arglist);

    /* Print result */
    printf("%d\n", result);

    return 0;
}
