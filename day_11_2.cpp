#include <algorithm>
#include <array>
#include <fstream>
#include <iostream>
#include <set>
#include <vector>


std::array<std::array<int, 10>, 10> readInput(const std::string& inputFile) {
    std::ifstream input("input/input_11_2.txt");
    std::array<std::array<int, 10>, 10> energyLevels;
    std::string line;
    int row = 0;
    while(std::getline(input, line)) {
        for (int col = 0; col < 10; col++) {
            energyLevels[row][col] = line[col] - '0';
        }
        row++;
    }
    return energyLevels;
}

std::vector<int> generateFlashes(const std::array<std::array<int, 10>, 10>& energyLevels, const std::set<int>& doneFlashes) {
    std::vector<int> flashes;
    for (int row = 0; row < energyLevels.size(); row++) {
        for (int col = 0; col < energyLevels[row].size(); col++) {
            if (energyLevels[row][col] > 9 && doneFlashes.find(row*10+col) == doneFlashes.end()) {
                flashes.push_back(row*10+col);
            }
        }
    }
    return flashes;
}

void applyFlash(std::array<std::array<int, 10>, 10>& energyLevels, int row, int col) {
    if (row > 0) { // TOP
        energyLevels[row-1][col]++;
    }
    if (col > 0) { // LEFT
        energyLevels[row][col-1]++;
    }
    if (row > 0 && col > 0) { // TOP-LEFT
        energyLevels[row-1][col-1]++;
    }
    if (row < 9) { // BOTTOM
        energyLevels[row+1][col]++;
    }
    if (col < 9) { // RIGHT
        energyLevels[row][col+1]++;
    }
    if (row < 9 && col < 9) { // BOTTOM-RIGHT
        energyLevels[row+1][col+1]++;
    }
    if (row < 9 && col > 0) { // BOTTOM-LEFT
        energyLevels[row+1][col-1]++;
    }
    if (row > 0 && col < 9) { // TOP-RIGHT
        energyLevels[row-1][col+1]++;
    }
}

void applyFlashes(std::array<std::array<int, 10>, 10>& energyLevels, const std::vector<int>& flashes) {
    for (int i = 0; i < flashes.size(); i++) {
        int row = flashes[i] / 10;
        int col = flashes[i] % 10;
        applyFlash(energyLevels, row, col);
    }
}

int applyStep(std::array<std::array<int, 10>, 10>& energyLevels) {
    int numberOfFlashes = 0;

    for (int row = 0; row < energyLevels.size(); row++) {
        for (int col = 0; col < energyLevels[row].size(); col++) {
            energyLevels[row][col]++;
        }
    }

    std::set<int> doneFlashes;
    std::vector<int> flashes;
    do {
        flashes = generateFlashes(energyLevels, doneFlashes);
        numberOfFlashes += flashes.size();
        applyFlashes(energyLevels, flashes);
        for (int i = 0; i < flashes.size(); i++) {
            doneFlashes.insert(flashes[i]);
        }
    } while (!flashes.empty());

    for (std::set<int>::iterator it = doneFlashes.begin(); it != doneFlashes.end(); ++it) {
        int row = *it / 10;
        int col = *it % 10;
        energyLevels[row][col] = 0;
    }

    return numberOfFlashes;
}

int computeAnswer(std::array<std::array<int, 10>, 10>& input, int numberOfSteps) {
    int totalFlashes = 0;
    for (int step = 1; step <= numberOfSteps; step++) {
        int numberOfFlashes = applyStep(input);
        totalFlashes += numberOfFlashes;
        if (numberOfFlashes == 100) {
            std::cout << "All octopuses flashed simultaneously at step " << step << std::endl;
            return totalFlashes;
        }
    }
    return totalFlashes;
}

int main(int argc, char** argv) {
    std::array<std::array<int, 10>, 10> input = readInput(std::string(argv[2]));
    const int answer = computeAnswer(input, 1000);
    std::cout << "The answer is: " << answer << std::endl;
}