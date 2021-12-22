integer function isLowPoint(input, i, j) result(isLow)
  integer, dimension(100, 100) :: input
  integer :: i, j
  isLow = 1
  if (1 < i) then
    if (input(i-1, j) <= input(i,j)) then
      isLow = 0
    end if
  end if
  if (i < 100) then
    if (input(i+1, j) <= input(i,j)) then
      isLow = 0
    end if
  end if
  if (1 < j) then
    if (input(i, j-1) <= input(i,j)) then
      isLow = 0
    end if
  end if
  if (j < 100) then
    if (input(i, j+1) <= input(i,j)) then
      isLow = 0
    end if
  end if
end function isLowPoint

integer function computeRisk(input, i, j)
  integer, dimension(100, 100) :: input
  integer :: i, j
  if (isLowPoint(input, i, j) == 1) then
    computeRisk = input(i,j)+1
  else
    computeRisk = 0
  end if
end function

program hello
  implicit none
  integer, dimension(100, 100) :: input
  character(len=100), dimension(100) :: lines
  integer :: row, column, totalRisk, risk, computeRisk

  open (1, file = 'input/input_9_1.txt', status = 'old')

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
      risk = computeRisk(input, row, column)
      totalRisk = risk + totalRisk
    end do
  end do

  print *, "Answer: "
  print *, totalRisk
end program hello
