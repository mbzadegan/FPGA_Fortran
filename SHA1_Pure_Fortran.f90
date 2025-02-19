program sha1_pure_fortran
  implicit none
  integer, parameter :: msg_length = 64  ! Maximum input length
  character(len=msg_length) :: input_string
  integer(kind=4), dimension(5) :: h = [  &
       int(Z'67452301', kind=4), &
       int(Z'EFCDAB89', kind=4), &
       int(Z'98BADCFE', kind=4), &
       int(Z'10325476', kind=4), &
       int(Z'C3D2E1F0', kind=4)  &
    ]
  integer :: i

  ! Read input
  write(*, '(A)', advance='no') 'Enter string: '
  read(*, '(A)') input_string

  ! Call SHA1 function
  call sha1_process(trim(input_string), h)

  ! Print SHA1 hash in hex
  write(*, '(A)', advance='no') 'SHA1: '
  do i = 1, 5
    write(*, '(Z8.8)', advance='no') h(i)
  end do
  write(*, *)

contains

  subroutine sha1_process(message, hash)
    implicit none
    character(len=*), intent(in) :: message
    integer(kind=4), dimension(5), intent(inout) :: hash

    integer(kind=4), parameter :: k(4) = [int(Z'5A827999',4), int(Z'6ED9EBA1',4), &
                                          int(Z'8F1BBCDC',4), int(Z'CA62C1D6',4)]
    integer(kind=4), dimension(80) :: w
    integer(kind=4) :: a, b, c, d, e, temp
    integer :: i, j, length
    character(len=64) :: padded_message
    integer(kind=4), dimension(16) :: block

    ! Padding the message
    length = len_trim(message)
    padded_message = message
    padded_message(length+1:length+1) = char(128)
    do j = length+2, 56
      padded_message(j:j) = char(0)
    end do
    write(padded_message(57:64), '(Z16.16)') length * 8

    ! Convert padded message to 16 32-bit words
    do i = 1, 16
      read(padded_message((i-1)*4+1:i*4), '(Z8.8)') block(i)
    end do

    ! Prepare message schedule
    do i = 1, 16
      w(i) = block(i)
    end do
    do i = 17, 80
      w(i) = ior(ishft(w(i-3) .xor. w(i-8) .xor. w(i-14) .xor. w(i-16), 1), 0)
    end do

    ! Initialize hash values
    a = hash(1)
    b = hash(2)
    c = hash(3)
    d = hash(4)
    e = hash(5)

    ! Main loop
    do i = 1, 80
      temp = ior(ishft(a, 5), ishft(a, -27)) + function_f(i, b, c, d) + e + w(i) + k((i-1)/20 + 1)
      e = d
      d = c
      c = ior(ishft(b, 30), ishft(b, -2))
      b = a
      a = temp
    end do

    ! Update hash
    hash(1) = hash(1) + a
    hash(2) = hash(2) + b
    hash(3) = hash(3) + c
    hash(4) = hash(4) + d
    hash(5) = hash(5) + e

  end subroutine sha1_process

  function function_f(t, b, c, d) result(f)
    implicit none
    integer(kind=4), intent(in) :: t, b, c, d
    integer(kind=4) :: f

    select case (t)
    case (1:20)
      f = ior(iand(b, c), iand(not(b), d))
    case (21:40)
      f = ieor(b, c, d)
    case (41:60)
      f = ior(iand(b, c), iand(b, d), iand(c, d))
    case (61:80)
      f = ieor(b, c, d)
    end select
  end function function_f

end program sha1_pure_fortran
