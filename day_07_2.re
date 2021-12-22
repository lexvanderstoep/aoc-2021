let splitInput(inp: string): list(string) = String.split_on_char(',',inp);

let parseChar(inp: string): int = int_of_string (inp);

let parseInput(inp: string): list(int) = List.map(c => int_of_string (c),(splitInput (inp)));

let sum(nums: list(int)): int = List.fold_left(((dis_1, dis_2) => dis_1 + dis_2), 0, nums);

let computeMean(crabs: list(int)) = {
  let s = sum(crabs);
  let c = List.length(crabs);
  int_of_float(floor(((float_of_int(s)/.float_of_int(c))+.0.5)))
};

let computeDistance(crabs: list(int), position): int =
  sum(List.map ((crab => (abs(crab - position)) * (abs(crab - position) + 1) / 2),crabs));

let computeAnswer(input_string: string): int = {
  let crabs = parseInput(input_string);
  let mean = computeMean(crabs);
  computeDistance(crabs, mean)
};

Js.log(string_of_int(computeAnswer(input_string)));