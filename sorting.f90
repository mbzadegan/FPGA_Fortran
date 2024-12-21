! 3 sorting methods

program sort_compare
    implicit none
    integer, parameter :: n = 1000  ! Number of random numbers to sort
    real :: arr(n), arr_copy(n)
    integer :: i
    real :: t_bubble, t_insertion, t_quick

    ! Seed the random number generator and fill the array with random numbers
    call random_seed()
    call random_number(arr)

    ! Scale to a larger range (e.g., 0 to 1000)
    arr = arr * 1000

    ! Bubble Sort
    arr_copy = arr
    call cpu_time(t_bubble)
    call bubble_sort(arr_copy, n)
    call cpu_time(t_bubble)
    t_bubble = t_bubble - t_bubble

    ! Insertion Sort
    arr_copy = arr
    call cpu_time(t_insertion)
    call insertion_sort(arr_copy, n)
    call cpu_time(t_insertion)
    t_insertion = t_insertion - t_insertion

    ! Quick Sort
    arr_copy = arr
    call cpu_time(t_quick)
    call quick_sort(arr_copy, 1, n)
    call cpu_time(t_quick)
    t_quick = t_quick - t_quick

    ! Print processing times for each sorting method
    print *, 'Sorting ', n, ' random numbers:'
    print *, 'Bubble Sort Time: ', t_bubble, ' seconds'
    print *, 'Insertion Sort Time: ', t_insertion, ' seconds'
    print *, 'Quick Sort Time: ', t_quick, ' seconds'

contains

    ! Bubble Sort Subroutine
    subroutine bubble_sort(a, n)
        real, intent(inout) :: a(n)
        integer :: i, j
        real :: temp
        do i = 1, n - 1
            do j = 1, n - i
                if (a(j) > a(j+1)) then
                    temp = a(j)
                    a(j) = a(j+1)
                    a(j+1) = temp
                end if
            end do
        end do
    end subroutine bubble_sort

    ! Insertion Sort Subroutine
    subroutine insertion_sort(a, n)
        real, intent(inout) :: a(n)
        integer :: i, j
        real :: key
        do i = 2, n
            key = a(i)
            j = i - 1
            do while (j > 0 .and. a(j) > key)
                a(j+1) = a(j)
                j = j - 1
            end do
            a(j+1) = key
        end do
    end subroutine insertion_sort

    ! Quick Sort Subroutine
    recursive subroutine quick_sort(a, low, high)
        real, intent(inout) :: a(:)
        integer, intent(in) :: low, high
        integer :: pivot_index

        if (low < high) then
            call partition(a, low, high, pivot_index)
            call quick_sort(a, low, pivot_index - 1)
            call quick_sort(a, pivot_index + 1, high)
        end if
    end subroutine quick_sort

    ! Partition Subroutine for Quick Sort
    subroutine partition(a, low, high, pivot_index)
        real, intent(inout) :: a(:)
        integer, intent(in) :: low, high
        integer, intent(out) :: pivot_index
        real :: pivot, temp
        integer :: i, j

        pivot = a(high)
        i = low - 1
        do j = low, high - 1
            if (a(j) <= pivot) then
                i = i + 1
                temp = a(i)
                a(i) = a(j)
                a(j) = temp
            end if
        end do
        temp = a(i+1)
        a(i+1) = a(high)
        a(high) = temp
        pivot_index = i + 1
    end subroutine partition

end program sort_compare
