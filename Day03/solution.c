#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#define LINE_LENGTH 142  // 140 characters + newline + \0
#define LINE_COUNT 140
#define CHUNK_SIZE 32


int error(char* msg) {
    fprintf(stderr, "Error: %s\n", msg);
    return 1;
}

typedef struct Location {
    int x;
    int y;
} Location;

typedef struct PartNumber {
    int number;
    int x;
    int y;
    int len;
} PartNumber;

typedef struct GearRatio {
    int x;
    int y;
    int adj_part_indices[2];
} GearRatio;


int issymbol(char ch) {
    return !isdigit(ch) && ch != '.' && ch != '\n';
}

int isgear(char ch) {
    return ch == '*';
}


Location search_(int x, int y, int (*predicate)(char), char** lines) {
    // search position x in row y and adjacent rows for a char that matches
    // the predicate
    if (x  < 0 || x >= LINE_LENGTH) {
        return (Location) {-1, -1};
    }
    int low_row = y != 0 ? y - 1 : y;
    int high_row = y != LINE_COUNT - 1 ? y + 1 : y;
    for (int row = low_row; row <= high_row; row++) {
        if (predicate(lines[row][x])) {
            return (Location) {x, row};
        }
    }
    return (Location) {-1, -1};
}

Location find_part(PartNumber part_num, int (*predicate)(char), char** lines) {
    int x = part_num.x;
    int y = part_num.y;
    int len = part_num.len;
    for (int offset = -1; offset < len + 1; offset++) {
        Location result = search_(x + offset, y, predicate, lines);
        if (result.x != -1 && result.y != -1) {
            return result;
        }
    }
    return (Location) {-1, -1};
}


int main(int argc, char* argv[]) {
    if (argc <= 1) {
        return error("Please specify an input file.");
    }

    FILE* fp = fopen(argv[1], "r");
    if (fp == NULL) {
        char* err_msg;
        asprintf(&err_msg, "File not found: %s", argv[1]);
        return error(err_msg);
    }

    char* lines[LINE_COUNT];
    PartNumber* parts = malloc(CHUNK_SIZE * sizeof(PartNumber));
    int num_parts = 0;
    int capacity = CHUNK_SIZE;

    for (int i = 0; i < LINE_COUNT; ++i) {
        char* buffer = malloc(LINE_LENGTH * sizeof(char));
        fgets(buffer, LINE_LENGTH, fp);
        lines[i] = buffer;

        int tmp = 0;
        for (int j = 0; j < LINE_LENGTH; j++) {
            char ch = buffer[j];
            if (isdigit(ch)) {
                tmp *= 10;
                tmp += (ch - '0');  // digit -> int
            }
            else if (tmp != 0) {
                if (num_parts == capacity) {
                    capacity += CHUNK_SIZE;
                    parts = realloc(parts, capacity * sizeof(PartNumber));
                }
                int num_len = (int) log10((float) tmp) + 1;
                parts[num_parts] = (PartNumber) {tmp, j - num_len, i, num_len};
                num_parts++;
                tmp = 0;
            }
        }
    }

    int valid_part_indices[num_parts];
    int valid_parts = 0;
    for (int i = 0; i < num_parts; i++) {
        Location result = find_part(parts[i], &issymbol, lines);
        if (result.x != -1 && result.y != -1) {
            valid_part_indices[valid_parts] = i;
            valid_parts++;
        }
    }
    
    int sum_valid_part_numbers = 0;
    for (int i = 0; i < valid_parts; i++) {
        sum_valid_part_numbers += parts[valid_part_indices[i]].number;
    }
    printf("Sum of valid part numbers: %d\n", sum_valid_part_numbers);

    // this code assumes a part number is in contact with at most 1 gear
    // (i think)
    GearRatio ratios[valid_parts]; 
    int num_ratios = 0;
    for (int i = 0; i < valid_parts; i++) {
        int part_idx = valid_part_indices[i];
        Location result = find_part(parts[part_idx], &isgear, lines);
        if (result.x != -1 && result.y != -1) {
            int found_gear = 0;
            // check if we already know of a gear here
            for (int j = 0; j < num_ratios; j++) {
                GearRatio ratio = ratios[j];
                if (ratio.x == result.x && ratio.y == result.y) {
                    ratios[j].adj_part_indices[1] = part_idx;
                    found_gear = 1;
                    break;
                }
            }
            if (!found_gear) {
                ratios[num_ratios] = (GearRatio) {result.x, result.y,
                                                  {part_idx, -1}};
                num_ratios++;
            }
        }
    }
    
    int valid_ratio_indices[num_ratios];
    int valid_ratios = 0;
    int sum_valid_ratios = 0;
    for (int i = 0; i < num_ratios; i++) {
        int idx_p0 = ratios[i].adj_part_indices[0];
        int idx_p1 = ratios[i].adj_part_indices[1];
        if (idx_p0 != -1 && idx_p1 != -1) {
            valid_ratio_indices[valid_ratios] = i;
            valid_ratios++;
            sum_valid_ratios += parts[idx_p0].number * parts[idx_p1].number;
        }
    }

    printf("Sum of gear ratios: %d\n", sum_valid_ratios);

    for (int i = 0; i < LINE_COUNT; i++) {
        free(lines[i]);
    }

    free(parts);
    fclose(fp);

    return 0;
}
