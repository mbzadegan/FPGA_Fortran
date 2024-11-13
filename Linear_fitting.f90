! Fortran Code for Linear Curve Fitting by input 2xn matrix


program curve_fitting
  implicit none
  integer :: i, n
  real :: sum_x, sum_y, sum_xy, sum_x2
  real :: a, b
  real, allocatable :: points(:,:)

  ! Prompt the user for the number of points
  print *, "Enter the number of points (n):"
  read *, n

  ! Allocate memory for a 2xn matrix to store the points
  allocate(points(2, n))

  ! Input the points
  print *, "Enter the points (x, y):"
  do i = 1, n
    print *, "Point ", i, ":"
    read *, points(1, i), points(2, i)
  end do

  ! Initialize sums
  sum_x = 0.0
  sum_y = 0.0
  sum_xy = 0.0
  sum_x2 = 0.0

  ! Calculate sums needed for linear regression
  do i = 1, n
    sum_x = sum_x + points(1, i)
    sum_y = sum_y + points(2, i)
    sum_xy = sum_xy + points(1, i) * points(2, i)
    sum_x2 = sum_x2 + points(1, i)**2
  end do

  ! Calculate coefficients for y = a + bx
  b = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x**2)
  a = (sum_y - b * sum_x) / n

  ! Output the result
  print *, "The best-fit line is: y = ", a, " + ", b, " * x"

  ! Deallocate matrix memory
  deallocate(points)

end program curve_fitting
