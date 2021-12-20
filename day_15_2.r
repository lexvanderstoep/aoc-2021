readInput <- function(filename) {
    input <- readLines(filename)
    input <- do.call(rbind, lapply(strsplit(input, ""), as.numeric))
    rows <- nrow(input)
    columns <- ncol(input)
    largeInput <- matrix(0, 5 * rows, 5 * columns)
    for (largeRow in 1:5) {
        for (largeColumn in 1:5) {
            rowMin <- (largeRow - 1) * rows + 1
            rowMax <- largeRow * rows
            columnMin <- (largeColumn - 1) * columns + 1
            columnMax <- largeColumn * columns
            largeInput[rowMin : rowMax, columnMin : columnMax] <- (input + (largeRow - 1) + (largeColumn - 1) - 1) %% 9 + 1
        }
    }
    return(largeInput)
}

computeMinimalRisk <- function(input) {
    riskLevels <- input
    for (row in rev(1:nrow(input))) {
        for (col in rev(1:ncol(input))) {
            if (row < nrow(input) & col < ncol(input)) {
                riskLevels[row, col] <- input[row, col] + min(riskLevels[row + 1, col], riskLevels[row, col + 1])
            } else if (row < nrow(input) & col == ncol(input)) {
                riskLevels[row, col] <- input[row, col] + riskLevels[row + 1, col]
            } else if (row == nrow(input) & col < ncol(input)) {
                riskLevels[row, col] <- input[row, col] + riskLevels[row, col + 1]
            } else {
                riskLevels[row, col] <- input[row, col]
            }
        }
    }
    return(riskLevels[1, 1] - input[1, 1])
}

input <- readInput("input/input_15_2.txt")

minimalRisk <- computeMinimalRisk(input)

print(minimalRisk)