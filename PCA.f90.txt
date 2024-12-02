!Below is an example where Fortran is used to analyze and solve a challenging data-analysis problem: Principal Component Analysis (PCA) on a dataset. PCA is a dimensionality-reduction technique used in statistics and machine learning to identify patterns in data and express it in a way that highlights similarities and differences. PCA requires calculating the covariance matrix, finding its eigenvalues and eigenvectors, and projecting the data onto principal components.



program pca_example
  implicit none
  integer, parameter :: n = 3, m = 4
  real(8), dimension(n, m) :: data
  real(8), dimension(n, n) :: cov_matrix, eig_vectors
  real(8), dimension(n) :: eig_values
  integer :: i, j, info

  ! Example dataset (n rows, m columns)
  data = reshape([1.0d0, 2.0d0, 3.0d0, 4.0d0, &
                  2.0d0, 4.0d0, 6.0d0, 8.0d0, &
                  3.0d0, 6.0d0, 9.0d0, 12.0d0], shape=[n, m])

  ! Step 1: Calculate covariance matrix
  call calculate_covariance(data, n, m, cov_matrix)

  ! Step 2: Compute eigenvalues and eigenvectors
  call diagonalize(cov_matrix, n, eig_values, eig_vectors, info)

  ! Output results
  print *, "Covariance Matrix:"
  do i = 1, n
    print *, cov_matrix(i, 1:n)
  end do

  print *, "Eigenvalues:"
  print *, eig_values

  print *, "Eigenvectors:"
  do i = 1, n
    print *, eig_vectors(i, 1:n)
  end do
end program pca_example

! Subroutine to calculate the covariance matrix
subroutine calculate_covariance(data, n, m, cov_matrix)
  implicit none
  integer, intent(in) :: n, m
  real(8), dimension(n, m), intent(in) :: data
  real(8), dimension(n, n), intent(out) :: cov_matrix
  real(8), dimension(n) :: mean
  integer :: i, j, k

  ! Calculate mean of each feature
  mean = 0.0d0
  do i = 1, n
    do j = 1, m
      mean(i) = mean(i) + data(i, j)
    end do
    mean(i) = mean(i) / m
  end do

  ! Calculate covariance matrix
  cov_matrix = 0.0d0
  do i = 1, n
    do j = 1, n
      do k = 1, m
        cov_matrix(i, j) = cov_matrix(i, j) + &
                           (data(i, k) - mean(i)) * (data(j, k) - mean(j))
      end do
      cov_matrix(i, j) = cov_matrix(i, j) / (m - 1)
    end do
  end do
end subroutine calculate_covariance

! Subroutine to diagonalize a matrix (eigenvalues and eigenvectors)
subroutine diagonalize(matrix, n, eig_values, eig_vectors, info)
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  integer, intent(in) :: n
  real(8), dimension(n, n), intent(inout) :: matrix
  real(8), dimension(n), intent(out) :: eig_values
  real(8), dimension(n, n), intent(out) :: eig_vectors
  integer, intent(out) :: info
  real(8), dimension(n) :: work
  integer :: lwork

  lwork = 3 * n
  work = 0.0d0
  eig_vectors = matrix

  ! Use LAPACK routine DSYEV to compute eigenvalues and eigenvectors
  call dsyev('V', 'U', n, eig_vectors, n, eig_values, work, lwork, info)
end subroutine diagonalize

! LAPACK routine for eigenvalues and eigenvectors
subroutine dsyev(jobz, uplo, n, a, lda, w, work, lwork, info) bind(c, name="dsyev_")
  use, intrinsic :: iso_c_binding, only: c_double, c_int, c_char
  character(kind=c_char), intent(in) :: jobz, uplo
  integer(kind=c_int), intent(in) :: n, lda, lwork
  real(c_double), intent(inout) :: a(lda, *), work(*)
  real(c_double), intent(out) :: w(*)
  integer(kind=c_int), intent(out) :: info
end subroutine dsyev
