program bessel_function
  implicit none
  integer :: n
  real(8) :: x, result
  
  ! Prompt for order n and argument x
  print *, "Enter the order of the Bessel function (n):"
  read *, n
  print *, "Enter the argument (x):"
  read *, x
  
  ! Calculate the Bessel function J_n(x)
  result = bessel_jn(n, x)
  
  ! Print the result
  print *, "The Bessel function J_", n, "(", x, ") is: ", result
end program bessel_function
