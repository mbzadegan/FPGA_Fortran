! Here's a Fortran program that reads random data from a file, performs basic statistical tests to assess its randomness, and outputs the results. The program calculates the mean, standard deviation, and runs test (for randomness in sequence)


program randomness_test
    implicit none
    integer, parameter :: nmax = 10000
    real :: data(nmax)
    integer :: i, n
    real :: mean, stddev, variance
    integer :: runs, pos, neg
    character(len=100) :: filename
    logical :: file_exists

    ! Get file name from user
    print *, 'Enter the name of the file containing random data:'
    read(*, '(A)') filename

    ! Check if file exists
    inquire(file=filename, exist=file_exists)
    if (.not. file_exists) then
        print *, 'Error: File not found!'
        stop
    end if

    ! Open the file and read the data
    open(unit=10, file=filename, status='old', action='read')
    n = 0
    do i = 1, nmax
        read(10, *, end=100) data(i)
        n = n + 1
    end do
100 continue
    close(10)

    if (n == 0) then
        print *, 'Error: No data read from the file.'
        stop
    end if

    ! Compute mean
    mean = sum(data(1:n)) / real(n)
    
    ! Compute variance and standard deviation
    variance = sum((data(1:n) - mean) ** 2) / real(n)
    stddev = sqrt(variance)
    
    ! Runs test for randomness
    runs = 1
    pos = 0
    neg = 0
    do i = 2, n
        if (data(i) > data(i-1)) then
            if (neg > 0) runs = runs + 1
            pos = pos + 1
            neg = 0
        else if (data(i) < data(i-1)) then
            if (pos > 0) runs = runs + 1
            neg = neg + 1
            pos = 0
        end if
    end do

    ! Print results
    print *, 'Number of data points read: ', n
    print *, 'Mean: ', mean
    print *, 'Standard Deviation: ', stddev
    print *, 'Number of Runs: ', runs

end program randomness_test
