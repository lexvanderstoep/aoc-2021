let splitInput(inp: string): list(string) = String.split_on_char(',',inp);

let parseChar(inp: string): int = int_of_string (inp);

let parseInput(inp: string): list(int) = List.map(c => int_of_string (c),(splitInput (inp)));

let sum(nums: list(int)): int = List.fold_left(((dis_1, dis_2) => dis_1 + dis_2), 0, nums);

let computeMedian(crabs: list(int)) = {
	let sortedList = List.sort(Pervasives.compare,crabs);
	List.nth(sortedList, (List.length(sortedList))/2)
};

let computeDistance(crabs: list(int), position): int = sum(List.map((crab => abs(crab - position)),crabs));

let computeAnswer(input_string: string): int = {
  let crabs = parseInput(input_string);
  let median = computeMedian(crabs);
  computeDistance(crabs, median)
};

Js.log(string_of_int(computeAnswer(input_string)));