! To calculate the entropy of a given binary file in Fortran, we need to compute the frequency of each byte value (0-255) and then use the Shannon entropy formula. 
! The program opens the binary file and reads its contents byte by byte.
! It uses the mod(i, 256) + 1 expression to keep track of the frequency of each byte value (from 0 to 255).


program calculate_entropy
    implicit none
    character(len=255) :: file_name
    integer :: iunit, ios, i, file_size
    integer, dimension(256) :: byte_freq
    real :: entropy_value, total_bytes
    real :: p

    ! Ask user for the file name
    print *, "Enter the path to the binary file:"
    read *, file_name

    ! Open the binary file
    open(unit=iunit, file=trim(file_name), status='old', access='stream', form='unformatted', action='read')
    inquire(unit=iunit, size=file_size)

    ! Initialize byte frequency array to zero
    byte_freq = 0

    ! Read file content and count byte frequencies
    total_bytes = 0
    do i = 1, file_size
        read(iunit) byte_freq(mod(i, 256) + 1)
        total_bytes = total_bytes + 1
    end do

    close(iunit)

    ! Calculate the entropy
    entropy_value = calculate_entropy_from_frequencies(byte_freq, total_bytes)

    ! Print the result
    print *, "Calculated entropy:", entropy_value

contains

    ! Function to calculate entropy from byte frequencies
    real function calculate_entropy_from_frequencies(freq, n)
        integer, dimension(256), intent(in) :: freq
        real, intent(in) :: n
        integer :: i
        real :: p, sum

        ! Initialize sum to zero
        sum = 0.0

        ! Compute the entropy using Shannon's formula
        do i = 1, 256
            if (freq(i) > 0) then
                p = real(freq(i)) / real(n)
                sum = sum - p * log(p) / log(2.0)  ! log base 2
            end if
        end do

        calculate_entropy_from_frequencies = sum
    end function calculate_entropy_from_frequencies

end program calculate_entropy
