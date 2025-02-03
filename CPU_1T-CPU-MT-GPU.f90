! Here's a Fortran program implementing matrix multiplication using three approaches:
!     CPU Single-threaded
!     OpenMP Multi-threaded
!     CUDA (GPU-based)
! This program initializes two random matrices, performs multiplication using the three methods, and then measures the execution time for each approach.


program matrix_multiplication
  use omp_lib
  implicit none
  integer, parameter :: N = 512
  real, dimension(N,N) :: A, B, C
  integer :: i, j, k
  real :: start, finish

  call random_seed()
  call random_number(A)
  call random_number(B)
  C = 0.0

  ! Single-threaded CPU matrix multiplication
  call cpu_time(start)
  do i = 1, N
     do j = 1, N
        do k = 1, N
           C(i,j) = C(i,j) + A(i,k) * B(k,j)
        end do
     end do
  end do
  call cpu_time(finish)
  print *, 'Single-thread CPU Time:', finish - start

  ! OpenMP Multi-threaded matrix multiplication
  C = 0.0
  call cpu_time(start)
  !$omp parallel do private(i,j,k) shared(A,B,C) schedule(static)
  do i = 1, N
     do j = 1, N
        do k = 1, N
           C(i,j) = C(i,j) + A(i,k) * B(k,j)
        end do
     end do
  end do
  !$omp end parallel do
  call cpu_time(finish)
  print *, 'OpenMP Multi-threaded CPU Time:', finish - start

  ! CUDA Kernel Call
  call gpu_matrix_multiply(A, B, C, N, start, finish)
  print *, 'CUDA GPU Time:', finish - start

contains
  subroutine gpu_matrix_multiply(A, B, C, N, start, finish)
    use cudafor
    implicit none
    integer, intent(in) :: N
    real, dimension(N,N), intent(in) :: A, B
    real, dimension(N,N), intent(out) :: C
    real, device, allocatable :: d_A(:,:), d_B(:,:), d_C(:,:)
    real, intent(out) :: start, finish

    allocate(d_A(N,N), d_B(N,N), d_C(N,N))
    d_A = A
    d_B = B
    d_C = 0.0

    call cpu_time(start)
    call matrix_mult_kernel<<<(N+15)/16, (N+15)/16>>>(d_A, d_B, d_C, N)
    call cudaDeviceSynchronize()
    call cpu_time(finish)

    C = d_C
    deallocate(d_A, d_B, d_C)
  end subroutine gpu_matrix_multiply

  attributes(global) subroutine matrix_mult_kernel(A, B, C, N)
    use cudafor
    implicit none
    integer, value :: N
    real, device :: A(N,N), B(N,N), C(N,N)
    integer :: i, j, k
    i = (blockIdx%x - 1) * blockDim%x + threadIdx%x
    j = (blockIdx%y - 1) * blockDim%y + threadIdx%y
    if (i <= N .and. j <= N) then
       C(i,j) = 0.0
       do k = 1, N
          C(i,j) = C(i,j) + A(i,k) * B(k,j)
       end do
    end if
  end subroutine matrix_mult_kernel

end program matrix_multiplication
