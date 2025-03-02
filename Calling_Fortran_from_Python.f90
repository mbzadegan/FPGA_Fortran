! multiply two nxn matrices A and B

module matrix_multiply
  implicit none
contains
  ! Function to multiply two nxn matrices A and B
  subroutine multiply_matrices(A, B, C, n)
    integer, intent(in) :: n
    real(8), intent(in) :: A(n, n), B(n, n)
    real(8), intent(out) :: C(n, n)
    
    integer :: i, j, k

    ! Initialize the result matrix C with zeros
    C = 0.0d0

    ! Matrix multiplication: C = A * B
    do i = 1, n
      do j = 1, n
        do k = 1, n
          C(i, j) = C(i, j) + A(i, k) * B(k, j)
        end do
      end do
    end do
  end subroutine multiply_matrices
end module matrix_multiply
