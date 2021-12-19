function [field, instructions] = parseInput
    file = fileread("input/input_13_1.txt");
    splitFile = strsplit(file, "\n\n");
    splitMarks = strsplit(splitFile{1}, "\n");
    splitInstructions = strsplit(splitFile{2}, "\n");
    marks = zeros(size(splitMarks)(2), 2);
    instructions = zeros(size(splitInstructions)(2), 1);
    for i = 1:size(marks)(1)
        marks(i, 1) = str2num(strsplit(splitMarks{i}, ","){1});
        marks(i, 2) = str2num(strsplit(splitMarks{i}, ","){2});
    endfor
    x_max = max(marks(:, 1));
    y_max = max(marks(:, 2));
    field = zeros(y_max + 1, x_max + 1);
    for i = 1:size(marks)(1)
        field(marks(i,2)+1, marks(i,1)+1) = 1;
    endfor
    for i = 1:size(instructions)(1)
        instructionString = strsplit(splitInstructions{i}, " "){3};
        if instructionString(1) == 'y'
            instructions(i) = str2num(instructionString(3:end));
        else
            instructions(i) = -str2num(instructionString(3:end));
        endif
    endfor
endfunction

function newField = fold(field, instruction)
    if (instruction < 0)
        column = -instruction + 1;
        leftField = field(:, 1:column-1);
        leftField = fliplr(leftField);
        rightField = field(:, column + 1:end);
        leftFieldToAdd = zeros(size(rightField));
        leftFieldToAdd(:, 1:column-1) = leftField;
        newField = leftFieldToAdd + rightField;
    else
        row = instruction + 1;
        topField = field(1:row-1, :);
        bottomField = field(row+1:end, :);
        bottomField = flipud(bottomField);
        bottomFieldToAdd = zeros(size(topField));
        bottomFieldToAdd(size(bottomFieldToAdd)(1)-size(bottomField)(1)+1:end, :) = bottomField;
        newField = topField + bottomFieldToAdd;
    endif
endfunction

[field, instructions] = parseInput();

for i = 1:1
    instruction = instructions(i);
    field = fold(field, instruction);
endfor

answer = nnz(field)
