#include <stdio.h>

int main(int argc, char** argv) {
    FILE *myFile;
    myFile = fopen(argv[1], "r");
    int number = 0;
    int deeperCount = 0;
    int first = 0;
    int second = 0;
    int third = 0;
    fscanf(myFile, "%d", &first);
    fscanf(myFile, "%d", &second);
    fscanf(myFile, "%d", &third);
    int previousSum = first + second + third;
    int sum = previousSum - first;
    first = second;
    second = third;
    while (!feof(myFile))
    {
        fscanf(myFile, "%d", &number);
        if (number == 0) {
            break;
        }
        if (sum > previousSum) {
            deeperCount++;
        }
        previousSum = sum;
        sum -= first;
        first = second;
        second = third;
        third = number;
        sum += number;
    }
    if (sum > previousSum) {
        deeperCount++;
    }
    printf("Answer: %i\n", deeperCount);
}