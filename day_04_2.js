console.log("Bleh");
var i = 1;
i = 2;
console.log(3);
file = console.log(process.argv[2]);
  
console.log(file);

var fs = require('fs');

function parseBoard(lines) {
    return lines.filter(val => val != "").map(line => line.split(" ").filter(val => val != "").map(num => parseInt(num, 10)));
}

function applyBall(board, num) {
    for (var line = 0; line < board.length; line++) {
        for (var column = 0; column < board[line].length; column++) {
            if (board[line][column] == num) {
                board[line][column] = null
            }
        }
    }
    return board;
}

function bingoRow(board, row) {
    return board[row].every(num => num == null);
}

function bingoColumn(board, col) {
    return board.map(row => row[col]).every(num => num == null);
}

function bingo(board) {
    for (var row = 0; row < board.length; row++) {
        if (bingoRow(board, row)) {
            return true;
        }
    }
    for (var col = 0; col < board[0].length; col++) {
        if (bingoColumn(board, col)) {
            return true;
        }
    }
    return false;
}

function score(board) {
    var sum = 0;
    for (var row = 0; row < board.length; row++) {
        for (var col = 0; col < board[row].length; col++) {
            if (board[row][col] != null) {
                sum += board[row][col];
            }
        }
    }
    return sum;
}

// Use fs.readFile() method to read the file
fs.readFile('/Users/lstoep/Documents/Personal/aoc-2021/input/input_4_1.txt', 'utf-8', (err, data) => {
    var arr = data.split("\n");
    var drawnNumbersString = arr[0].split(",");
    var drawnNumbers = drawnNumbersString.map(num => parseInt(num, 10));
    var boards = []
    for (var i = 2; i < arr.length; i+= 6)
    {
        boards.push(parseBoard(arr.slice(i, i + 6)));
    }
    
    bingoedBoards = boards.map(board => false);
    lastBingoed = -1;
    var numbersId = 0;
    while (!boards.every(bingo) & numbersId < drawnNumbers.length) {
        boards = boards.map(board => applyBall(board, drawnNumbers[numbersId]));
        numbersId++;
        for (var i = 0; i < boards.length; i++) {
            var board = boards[i];
            if (bingo(board)) {
                if (!bingoedBoards[i]) {
                    lastBingoed = i;
                    bingoedBoards[i] = true;
                }
            }
        }
    }

    var board = boards[lastBingoed];
    if (bingo(board)) {
        console.log("Bingo: " + score(board) + " " + drawnNumbers[numbersId-1] + " = " + (score(board) * drawnNumbers[numbersId-1]));
    }
});
  
console.log('Done.');