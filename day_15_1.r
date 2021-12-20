readInput <- function(filename) {
    input <- readLines(filename)
    input <- do.call(rbind, lapply(strsplit(input, ""), as.numeric))
    return(input)
}

computeMinimalRisk <- function(input) {
    riskLevels <- input
    for (row in rev(1:nrow(input))) {
        for (col in rev(1:ncol(input))) {
            newRiskLevel <- riskLevels[row, col]
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

input <- readInput("input/input_15_1.txt")

minimalRisk <- computeMinimalRisk(input)

print(minimalRisk)