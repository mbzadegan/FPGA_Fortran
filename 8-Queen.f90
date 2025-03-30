


program eight_queen
  implicit none
  integer, parameter :: n = 8
  integer :: board(n), solutions
  logical :: safe

  solutions = 0
  call solve_queens(board, 1, solutions)

  print *, 'Total Solutions: ', solutions
contains

  recursive subroutine solve_queens(board, row, solutions)
    integer, intent(inout) :: board(n)
    integer, intent(in) :: row
    integer, intent(out) :: solutions
    integer :: col

    if (row > n) then
      solutions = solutions + 1
      call print_board(board)
      return
    end if

    do col = 1, n
      call is_safe(board, row, col, safe)
      if (safe) then
        board(row) = col
        call solve_queens(board, row + 1, solutions)
      end if
    end do
  end subroutine

  subroutine is_safe(board, row, col, safe)
    integer, intent(in) :: board(n), row, col
    logical, intent(out) :: safe
    integer :: i

    safe = .true.
    do i = 1, row - 1
      if (board(i) == col .or. abs(board(i) - col) == abs(i - row)) then
        safe = .false.
        return
      end if
    end do
  end subroutine

  subroutine print_board(board)
    integer, intent(in) :: board(n)
    integer :: i, j

    print *, 'Solution:'
    do i = 1, n
      do j = 1, n
        if (board(i) == j) then
          write(*, '(A)', advance="no") 'Q'
        else
          write(*, '(A)', advance="no") '.'
        end if
      end do
      print *
    end do
    print *
  end subroutine

end program eight_queen
