#include <stdio.h>

int main(int argc, char** argv) {
    FILE *myFile;
    myFile = fopen(argv[1], "r");
    int depth = -1;
    int previousDepth = -1;
    int deeperCount = 0;
    while (!feof(myFile)) {
        fscanf(myFile, "%d", &depth);
        if (depth == 0) {
            break;
        }
        if (previousDepth != -1 && depth > previousDepth) {
            deeperCount++;
        }
        previousDepth = depth;
    }
    printf("Answer: %i\n", deeperCount);
}