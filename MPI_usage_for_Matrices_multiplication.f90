! Below is an example of Fortran code that demonstrates how to handle large matrices in parallel using MPI (Message Passing Interface), a common approach in high-performance computing.
! Compile the code using an MPI-compatible Fortran compiler (e.g., mpifort):
! mpifort -o matrix_multiply matrix_multiply.f90

! Run the code using mpirun with a specified number of processes (e.g., 4):
! mpirun -np 4 ./matrix_multiply




program matrix_multiply
  use mpi
  implicit none

  integer :: rank, size, ierr, n, i, j, k
  integer :: matrix_size
  real(8), allocatable :: A(:,:), B(:,:), C(:,:)
  real(8), allocatable :: local_A(:,:), local_B(:,:), local_C(:,:)
  integer :: local_rows, start_row, end_row

  ! Initialize MPI
  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, size, ierr)

  ! Define the matrix size
  n = 1000 ! Set the size of the matrix
  matrix_size = n * n

  ! Divide the work equally among processes
  local_rows = n / size
  start_row = rank * local_rows + 1
  end_row = start_row + local_rows - 1

  ! Allocate matrices
  allocate(A(n, n), B(n, n), C(n, n))
  allocate(local_A(local_rows, n), local_B(n, n), local_C(local_rows, n))

  ! Initialize matrices A and B (simple example, normally loaded from a file or other source)
  if (rank == 0) then
     call random_seed()
     A = 1.0d0
     B = 2.0d0
  end if

  ! Broadcast matrix B to all processes (as it's the same for all)
  call MPI_Bcast(B, matrix_size, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)

  ! Scatter matrix A to all processes
  call MPI_Scatter(A, local_rows * n, MPI_REAL8, local_A, local_rows * n, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)

  ! Initialize local_C to 0
  local_C = 0.0d0

  ! Perform the matrix multiplication locally
  do i = 1, local_rows
     do j = 1, n
        do k = 1, n
           local_C(i, j) = local_C(i, j) + local_A(i, k) * B(k, j)
        end do
     end do
  end do

  ! Gather the local results into the global matrix C
  call MPI_Gather(local_C, local_rows * n, MPI_REAL8, C, local_rows * n, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)

  ! Print part of the result (only on process 0)
  if (rank == 0) then
     print *, "First few elements of the result matrix C:"
     do i = 1, 5
        print *, C(i, 1:5)
     end do
  end if

  ! Finalize MPI
  call MPI_Finalize(ierr)

end program matrix_multiply
