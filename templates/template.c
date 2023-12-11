#include <stdio.h>
#include <stdlib.h>

#define LINE_LENGTH 140
#define LINE_COUNT 140


int error(char* msg) {
    fprintf(stderr, "Error: %s\n", msg);
    return 1;
}


int main(int argc, char* argv[]) {
    if (argc <= 1) {
        return error("Please specify an input file.");
    }

    FILE* fp = fopen(argv[1], "r");
    if (fp == NULL) {
        // this leaks memory. oh well.
        char* err_msg;
        asprintf(&err_msg, "File not found: %s", argv[1]);
        return error(err_msg);
    }

    char* lines[LINE_COUNT];

    for (int i = 0; i < LINE_COUNT; i++) {
        char* buffer = malloc(LINE_LENGTH * sizeof(char));
        fgets(buffer, LINE_LENGTH, fp);
        lines[i] = buffer;
    }

    // do stuff here

    for (int i = 0; i < LINE_COUNT; i++) {
        free(lines[i]);
    }

    fclose(fp);

    return 0;
}
