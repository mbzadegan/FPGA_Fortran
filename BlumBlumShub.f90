# Pseudo random number generator using the Blum Blum Shub algorithm.

program blum_blum_shub
    implicit none
    integer :: i, num_random_numbers
    integer(8) :: p, q, m, x, random_bit
    integer(8), parameter :: seed = 12345  ! Initial seed (must be coprime with m)

    ! Choose two large prime numbers for p and q
    ! These values should be large primes (using smaller values here for simplicity)
    p = 499
    q = 547
    m = p * q  ! Modulus for BBS (should be a product of two primes)

    ! Initialize the starting value for the sequence
    x = mod(seed * seed, m)

    ! Number of random bits to generate
    num_random_numbers = 10

    print *, "Blum Blum Shub Random Numbers:"
    do i = 1, num_random_numbers
        ! Generate the next number in the sequence
        x = mod(x * x, m)
        
        ! Extract the least significant bit as a random bit (0 or 1)
        random_bit = mod(x, 2)
        
        print *, "Random Bit ", i, ": ", random_bit
    end do
end program blum_blum_shub
