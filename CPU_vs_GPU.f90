
program matrix_multiplication_compare

  use cudafor
  implicit none
  
  integer, parameter :: n = 1024 ! Matrix size (n x n)
  real(8), dimension(n,n), allocatable :: A, B, C_cpu, C_gpu
  real(8) :: start_cpu, end_cpu, start_gpu, end_gpu
  integer :: i, j, k
  
  ! Allocate matrices
  allocate(A(n,n), B(n,n), C_cpu(n,n), C_gpu(n,n))
  
  ! Initialize matrices with random numbers
  call random_number(A)
  call random_number(B)
  
  ! ***** CPU Matrix Multiplication *****
  call cpu_time(start_cpu)
  C_cpu = 0.0
  do i = 1, n
     do j = 1, n
        do k = 1, n
           C_cpu(i,j) = C_cpu(i,j) + A(i,k) * B(k,j)
        end do
     end do
  end do
  call cpu_time(end_cpu)
  print *, 'CPU Time: ', end_cpu - start_cpu, ' seconds'

  ! ***** GPU Matrix Multiplication *****
  call cpu_time(start_gpu)

  ! Device matrices for GPU computation
  real(8), device, allocatable :: d_A(:,:), d_B(:,:), d_C(:,:)
  allocate(d_A(n,n), d_B(n,n), d_C(n,n))

  ! Copy matrices from host to device
  d_A = A
  d_B = B
  d_C = 0.0

  ! Perform matrix multiplication on the GPU
  call gpu_matrix_multiply(d_A, d_B, d_C, n)
  
  ! Copy result from device back to host
  C_gpu = d_C

  call cpu_time(end_gpu)
  print *, 'GPU Time: ', end_gpu - start_gpu, ' seconds'

  ! Compare CPU and GPU results for correctness (optional)
  if (maxval(abs(C_cpu - C_gpu)) < 1e-6) then
     print *, 'Results match!'
  else
     print *, 'Results do not match!'
  end if

  ! Deallocate memory
  deallocate(A, B, C_cpu, C_gpu, d_A, d_B, d_C)
  
contains

  ! GPU Matrix Multiplication Subroutine (CUDA)
  attributes(global) subroutine matmul_kernel(A, B, C, n)
    integer, value :: n
    real(8), dimension(n,n), intent(in) :: A, B
    real(8), dimension(n,n), intent(inout) :: C
    integer :: i, j, k
    real(8) :: sum

    i = blockidx%x
    j = threadidx%x

    if (i <= n .and. j <= n) then
      sum = 0.0
      do k = 1, n
         sum = sum + A(i,k) * B(k,j)
      end do
      C(i,j) = sum
    end if
  end subroutine matmul_kernel

  ! GPU Matrix Multiplication Subroutine (Host)

  subroutine gpu_matrix_multiply(d_A, d_B, d_C, n)
    integer, value :: n
    real(8), dimension(n,n), device :: d_A, d_B, d_C
    integer :: block_size, grid_size

    block_size = 16
    grid_size = (n + block_size - 1) / block_size

    ! Launch kernel
    call matmul_kernel<<<grid_size, block_size>>>(d_A, d_B, d_C, n)
    call cudaDeviceSynchronize() ! Ensure GPU work is complete
  end subroutine gpu_matrix_multiply

end program matrix_multiplication_compare
