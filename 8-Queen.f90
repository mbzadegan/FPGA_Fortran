program eight_queens

  implicit none
  integer, parameter :: N = 8
  integer :: board(N), row
  integer :: solution_count = 0

  ! Initialize the board with 0 (no queens placed)
  board = 0

  ! Start solving the 8-Queens problem
  call solve_queens(board, 1, solution_count)
  print *, "Total number of solutions:", solution_count

contains

  ! Recursive function to place queens
  recursive subroutine solve_queens(board, row, solution_count)
    integer, intent(inout) :: board(N)  ! Board array to store queen positions
    integer, intent(in) :: row          ! Current row we are trying to place a queen in
    integer, intent(inout) :: solution_count  ! Count of found solutions

    integer :: col

    ! Base case: if all queens are placed, print the solution
    if (row > N) then
      solution_count = solution_count + 1
      call print_board(board, solution_count)
      return
    end if

    ! Try placing a queen in each column of the current row
    do col = 1, N
      if (is_safe(board, row, col)) then
        board(row) = col  ! Place queen in the column
        call solve_queens(board, row + 1, solution_count)  ! Recur to place the next queen
        board(row) = 0  ! Backtrack and remove the queen
      end if
    end do
  end subroutine solve_queens

  ! Function to check if placing a queen is safe
  logical function is_safe(board, row, col)
    integer, intent(in) :: board(N)
    integer, intent(in) :: row, col
    integer :: i

    ! Check previous rows for conflicts
    do i = 1, row - 1
      if (board(i) == col) return .false.  ! Check column conflict
      if (abs(board(i) - col) == abs(i - row)) return .false.  ! Check diagonal conflict
    end do

    is_safe = .true.  ! Safe to place queen here
  end function is_safe

  ! Subroutine to print the board with queen positions
  subroutine print_board(board, solution_number)
    integer, intent(in) :: board(N)
    integer, intent(in) :: solution_number
    integer :: i, j

    print *, "Solution number:", solution_number
    do i = 1, N
      do j = 1, N
        if (board(i) == j) then
          write(*, "(A)", advance="no") " Q"
        else
          write(*, "(A)", advance="no") " ."
        end if
      end do
      print *  ! Newline after each row
    end do
    print *  ! Newline after each solution
  end subroutine print_board

end program eight_queens
