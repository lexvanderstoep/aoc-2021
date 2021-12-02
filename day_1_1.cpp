#include <fstream>
#include <iostream>

int main(int argc, char** argv) {
    std::ifstream infile(argv[1]);
    int depth = -1;
    int previousDepth = -1;
    int deeperCount = 0;
    while (infile >> depth)
    {
        if (previousDepth != -1 && depth > previousDepth) {
            deeperCount++;
        }
        previousDepth = depth;
    }
    std::cout << deeperCount << std::endl;
}