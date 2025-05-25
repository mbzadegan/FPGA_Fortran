! Sample Input File: (input_data.txt)

! 1.0 2.0 3.0 4.0
! 5.0 6.0 NaN 8.0
! 9.0 10.0 11.0 12.0
! 13.0 Invalid 15.0 16.0

! Resulting Output File (cleaned_data.txt)
! 1.000    2.000    3.000    4.000
! 9.000   10.000   11.000   12.000




program data_cleaning
    implicit none
    character(len=100) :: input_file, output_file
    integer, parameter :: max_columns = 10
    real :: data_row(max_columns)
    integer :: iostat, i, num_columns
    logical :: valid_data
    character(len=200) :: line

    ! Input file and output file names
    input_file = "input_data.txt"
    output_file = "cleaned_data.txt"

    open(unit=10, file=input_file, status="old", action="read", iostat=iostat)
    if (iostat /= 0) then
        print *, "Error opening input file."
        stop
    end if

    open(unit=20, file=output_file, status="replace", action="write", iostat=iostat)
    if (iostat /= 0) then
        print *, "Error opening output file."
        stop
    end if

    ! Read and process each line
    do
        read(10, '(A)', iostat=iostat) line
        if (iostat /= 0) exit

        ! Parse the line into numerical data
        num_columns = 0
        valid_data = .true.

        do i = 1, max_columns
            read(line, *, iostat=iostat) data_row(i)
            if (iostat /= 0) then
                if (i == 1) valid_data = .false.  ! Entire row is invalid
                exit
            end if
            num_columns = num_columns + 1
        end do

        if (valid_data) then
            write(20, "(F8.3)", advance="no") data_row(1)
            do i = 2, num_columns
                write(20, "(1X,F8.3)", advance="no") data_row(i)
            end do
            write(20, *)
        end if
    end do

    close(10)
    close(20)

    print *, "Data cleaning completed. Cleaned data saved to:", output_file
end program data_cleaning
