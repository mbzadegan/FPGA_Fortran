! CPU-based SHA-256 Calculation: This will be done using standard Fortran and CPU resources.
! GPU-based SHA-256 Calculation: This will use OpenMP to offload the computation to a GPU.

! gfortran -fopenmp -foffload=nvptx -O3 sha256_fortran.f90 -o sha256_program
! ifort -qopenmp -offload-arch=sm_70 sha256_fortran.f90 -o sha256_program




program sha256_file
    implicit none
    integer :: ios
    integer, parameter :: n = 64
    character(len=100), parameter :: filename = 'input.txt'
    character(len=n) :: hash_cpu, hash_gpu
    real(8) :: t1, t2
    integer :: i
    character(len=1024) :: data
    logical :: file_exists

    ! Check if file exists
    call file_exists_check(filename, file_exists)
    if (.not. file_exists) then
        print *, 'File does not exist!'
        stop
    end if

    ! Read file contents
    call read_file(filename, data)

    ! Calculate SHA256 hash using CPU
    call cpu_sha256(data, hash_cpu, t1)

    ! Calculate SHA256 hash using GPU
    call gpu_sha256(data, hash_gpu, t2)

    ! Output results
    print *, 'SHA-256 Hash (CPU): ', hash_cpu
    print *, 'SHA-256 Hash (GPU): ', hash_gpu
    print *, 'CPU Time (t1): ', t1
    print *, 'GPU Time (t2): ', t2

contains

    ! Function to check if file exists
    subroutine file_exists_check(filename, exists)
        character(len=*) :: filename
        logical :: exists
        open(unit=10, file=filename, status='old', action='read', iostat=exists)
        close(10)
    end subroutine

    ! Function to read a file into a string (simplified)
    subroutine read_file(filename, data)
        character(len=*) :: filename
        character(len=1024) :: data
        integer :: unit, ios

        open(unit=unit, file=filename, status='old', action='read')
        read(unit, '(A1024)', IOSTAT=ios) data
        close(unit)
    end subroutine

    ! CPU SHA-256 computation (simplified)
    subroutine cpu_sha256(data, hash, elapsed_time)
        character(len=1024) :: data
        character(len=64) :: hash
        real(8) :: elapsed_time
        integer :: i, start_time, end_time

        ! Capture start time
        call cpu_time(start_time)

        ! Perform SHA-256 hashing (simplified)
        hash = '0000000000000000000000000000000000000000000000000000000000000000'  ! Dummy value

        ! Capture end time
        call cpu_time(end_time)

        elapsed_time = real(end_time - start_time)

    end subroutine

    ! GPU SHA-256 computation using OpenMP (simplified)
    subroutine gpu_sha256(data, hash, elapsed_time)
        character(len=1024) :: data
        character(len=64) :: hash
        real(8) :: elapsed_time
        integer :: start_time, end_time

        ! Capture start time
        call cpu_time(start_time)

        ! Offload SHA-256 computation to GPU (dummy implementation for illustration)
        !$omp target teams distribute parallel for
        do i = 1, 1  ! Single iteration (dummy for illustration)
            hash = '0000000000000000000000000000000000000000000000000000000000000000'  ! Dummy value
        end do

        ! Capture end time
        call cpu_time(end_time)

        elapsed_time = real(end_time - start_time)

    end subroutine

end program
