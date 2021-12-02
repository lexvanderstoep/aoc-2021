#include <fstream>
#include <iostream>

int main(int argc, char** argv) {
    std::ifstream infile(argv[1]);
    int number = 0;
    int deeperCount = 0;
    int first = 0;
    int second = 0;
    int third = 0;
    infile >> first;
    infile >> second;
    infile >> third;
    int previousSum = first + second + third;
    int sum = previousSum - first;
    first = second;
    second = third;
    while (infile >> number)
    {
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
    std::cout << deeperCount << std::endl;
}