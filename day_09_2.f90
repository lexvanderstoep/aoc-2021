recursive function exploreBasin(row, column, visited, input) result(basinSize)
  integer :: row, column, height
  integer, dimension(100, 100) :: visited
  integer, dimension(100, 100) :: input
  integer :: basinSizeLeft, basinSizeTop, basinSizeRight, basinSizeBottom, basinSize
  height = input(row, column)
  if (visited(row, column) /= 0) then
    basinSize = 0
  else if (input(row, column) == 9) then
    basinSize = 0
  else
    visited(row, column) = 1
    if (column > 1) then
      basinSizeLeft = exploreBasin(row, column - 1, visited, input)
    else
      basinSizeLeft = 0
    end if
    if (column < 100) then
      basinSizeRight = exploreBasin(row, column + 1, visited, input)
    else
      basinSizeRight = 0
    end if
    if (row > 1) then
      basinSizeTop = exploreBasin(row - 1, column, visited, input)
    else
      basinSizeTop = 0
    end if
    if (row < 100) then
      basinSizeBottom = exploreBasin(row + 1, column, visited, input)
    else
      basinSizeBottom = 0
    end if
    basinSize = 1 + basinSizeLeft + basinSizeTop + basinSizeRight + basinSizeBottom
  end if
end function

program hello
  implicit none
  integer, dimension(100, 100) :: input
  character(len=100), dimension(100) :: lines
  integer :: row, column, exploreBasin
  integer, dimension(100, 100) :: visited
  integer :: answer, firstBasin, secondBasin, thirdBasin, basinSize
  firstBasin = 0
  secondBasin = 0
  thirdBasin = 0

  open (1, file = 'input/input_9_2.txt', status = 'old')

  do row = 1,100
      read(1,*) lines(row)
  end do

  do row = 1,100
    do column = 1,100
      read(lines(row)(column:column), *) input(row, column)
    end do
  end do

  do row = 1,100
    do column = 1,100
      basinSize = exploreBasin(row, column, visited, input) 
      if (basinSize > firstBasin) then
        thirdBasin = secondBasin
        secondBasin = firstBasin
        firstBasin = basinSize
      else if (basinSize > secondBasin) then
        thirdBasin = secondBasin
        secondBasin = basinSize
      else if (basinSize > thirdBasin) then
        thirdBasin = basinSize
      end if
    end do
  end do
  answer = firstBasin * secondBasin * thirdBasin
  print *, "Answer: ", answer
end program hello
