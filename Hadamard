! Here’s a Fortran program that implements the construction of a Hadamard matrix using Sylvester's construction method for an order 𝑛 (which must be a power of 2).




program hadamard_matrix
  implicit none
  integer :: n
  integer, dimension(:,:), allocatable :: H

  ! Input: order of Hadamard matrix
  print*, "Enter the order (n) of the Hadamard matrix (must be a power of 2):"
  read*, n

  ! Validate that n is a power of 2
  if (modulo(n, 2) /= 0 .and. n /= 1) then
     print*, "Error: The order must be a power of 2."
     stop
  end if

  ! Allocate the matrix H with size n x n
  allocate(H(n, n))

  ! Generate the Hadamard matrix using Sylvester's construction
  call hadamard(H, n)

  ! Output the Hadamard matrix
  print*, "Hadamard matrix of order", n, ":"
  call print_matrix(H, n)

contains

  ! Recursive subroutine to generate Hadamard matrix
  subroutine hadamard(H, n)
    integer, dimension(:,:), intent(out) :: H
    integer :: n
    integer :: i, j

    if (n == 1) then
      H(1, 1) = 1
    else
      call hadamard(H, n/2)
      ! Construct the Hadamard matrix of size n from smaller matrix
      do i = 1, n/2
        do j = 1, n/2
          H(i, j) = H(i, j)
          H(i, j + n/2) = H(i, j)
          H(i + n/2, j) = H(i, j)
          H(i + n/2, j + n/2) = -H(i, j)
        end do
      end do
    end if
  end subroutine hadamard

  ! Subroutine to print the matrix
  subroutine print_matrix(M, n)
    integer, dimension(:,:), intent(in) :: M
    integer :: n
    integer :: i, j

    do i = 1, n
      write(*, "(10I3)") M(i, :)
    end do
  end subroutine print_matrix

end program hadamard_matrix
