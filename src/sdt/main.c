#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>

#include "scanner.h"
#include "parser.h"

int main(int argc, char *argv[]) {
    struct token *t = init_token('>', NULL);
    free_token(t);

    return EXIT_SUCCESS;
}
