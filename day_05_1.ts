var fs = require('fs');

type vent = [[number, number], [number, number]];
type field = number[][];

function parseVent(ventString: string): vent {
    let coordinateStrings = ventString.split(" -> ");
    let firstCoordinateStrings = coordinateStrings[0].split(",");
    let secondCoordinateStrings = coordinateStrings[1].split(",");
    let x_1 = parseInt(firstCoordinateStrings[0]);
    let y_1 = parseInt(firstCoordinateStrings[1]);
    let x_2 = parseInt(secondCoordinateStrings[0]);
    let y_2 = parseInt(secondCoordinateStrings[1]);
    return [[x_1, y_1], [x_2, y_2]];
}

function getInitialField(vents: vent[]): field {
    const maxCoordinates = Math.max.apply(Math, vents.map(vent => Math.max(vent[0][0], vent[0][1], vent[1][0], vent[1][1])));
    var emptyField: field = new Array(maxCoordinates+1);
    for (var i = 0; i <= maxCoordinates; i++) {
        emptyField[i] = new Array(maxCoordinates+1);
        for (var j = 0; j <= maxCoordinates; j++) {
            emptyField[i][j] = 0;
        }
    }
    return emptyField;
}

function parseInput(input: string): vent[] {
    let ventStrings = input.split("\n");
    return ventStrings.filter(ventString => ventString != "").map(parseVent);
}

function getDeltaPosition(startPosition: [number, number], endPosition: [number, number]): [number, number] {
    var deltaX = endPosition[0] - startPosition[0];
    var deltaY = endPosition[1] - startPosition[1];
    if(deltaX != 0) {
        deltaX = deltaX / Math.abs(deltaX);
    }
    if(deltaY != 0) {
        deltaY = deltaY / Math.abs(deltaY);
    }
    return [deltaX, deltaY];
}

function equalPosition(pos1: [number, number], pos2: [number, number]): boolean {
    return pos1[0] == pos2[0] && pos1[1] == pos2[1];
}

function applyVent(field: field, vent: vent): field {
    const startPosition = vent[0];
    const endPosition = vent[1]
    const deltaPosition = getDeltaPosition(startPosition, endPosition);
    var position = startPosition;
    field[position[0]][position[1]] = field[position[0]][position[1]] + 1;
    while (!equalPosition(position, vent[1])) {
        position[0] += deltaPosition[0];
        position[1] += deltaPosition[1];
        field[position[0]][position[1]] = field[position[0]][position[1]] + 1;
    }
    return field;
}

function score(field: field): number {
    return field.reduce((sum, fieldRow) => sum + fieldRow.filter(element => element >= 2).length, 0);
}

function isDiagonalVent(vent: vent): boolean {
    return vent[0][0] != vent[1][0] && vent[0][1] != vent[1][1];
}

function computeAnswer(input: string): number {
    const vents = parseInput(input);
    var field = getInitialField(vents);
    for (const vent of vents) {
        if (!isDiagonalVent(vent)) {
            field = applyVent(field, vent);
        }
    }
    return score(field);
}

let input: string = fs.readFileSync('input/input_5_1.txt','utf8');

let answer: number = computeAnswer(input);
console.log(answer);
