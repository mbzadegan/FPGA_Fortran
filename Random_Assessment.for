program random_number_quality
    implicit none
    integer, parameter :: n = 1000         ! Number of random numbers
    real :: random_numbers(n)              ! Array to store random numbers
    real :: mean, variance, std_dev        ! Variables for mean, variance, standard deviation
    integer :: i, j                        ! Loop variables
    real :: sum, sum_squared               ! Sums for calculations
    real :: autocorr                       ! Autocorrelation variable
    integer, parameter :: lag = 1          ! Lag value for autocorrelation

    ! Seed the random number generator with the current time (optional)
    call random_seed()

    ! Generate random numbers between 0 and 1
    do i = 1, n
        call random_number(random_numbers(i))
    end do

    ! Calculate the mean
    sum = 0.0
    do i = 1, n
        sum = sum + random_numbers(i)
    end do
    mean = sum / n

    ! Calculate the variance
    sum_squared = 0.0
    do i = 1, n
        sum_squared = sum_squared + (random_numbers(i) - mean)**2
    end do
    variance = sum_squared / (n - 1)
    std_dev = sqrt(variance)

    ! Calculate autocorrelation at specified lag
    sum = 0.0
    do i = 1, n - lag
        sum = sum + (random_numbers(i) - mean) * (random_numbers(i + lag) - mean)
    end do
    autocorr = sum / ((n - lag) * variance)

    ! Output the results
    print *, "Statistics for the generated random numbers:"
    print *, "Mean          =", mean
    print *, "Variance      =", variance
    print *, "Std. Dev.     =", std_dev
    print *, "Autocorrelation at lag", lag, "=", autocorr

    ! Evaluate quality based on statistics
    if (abs(mean - 0.5) < 0.05 .and. abs(variance - 1.0/12.0) < 0.05) then
        print *, "Quality assessment: The random numbers seem to be of good quality."
    else
        print *, "Quality assessment: The random numbers may be biased."
    end if

end program random_number_quality
