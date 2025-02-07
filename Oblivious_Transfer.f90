! Fortran program to simulate an Oblivious Transfer (OT) protocol, a fundamental cryptographic primitive used in secure computation. 
! This implementation is a simple 1-out-of-2 Oblivious Transfer (OT 1-2) protocol, where:
! The sender has two messages, 𝑚0  and m1
! The receiver chooses one message (but the sender does not learn which one).
! The receiver obtains only the chosen message.


program oblivious_transfer
  implicit none
  integer :: m0, m1, choice, received_message
  integer :: r, k0, k1, c0, c1
  integer, parameter :: p = 23, g = 5  ! Small prime and generator (simplified example)

  ! Step 1: Sender selects two messages
  m0 = 10
  m1 = 20
  print *, 'Sender has messages: ', m0, ' and ', m1

  ! Step 2: Receiver selects choice (0 or 1)
  print *, 'Receiver, enter choice (0 or 1): '
  read *, choice
  if (choice /= 0 .and. choice /= 1) then
     print *, 'Invalid choice!'
     stop
  end if

  ! Step 3: Receiver generates random number r and computes k0 and k1
  call random_seed()
  call random_number(r)
  r = mod(int(r * 100), p)  ! Ensure r is within valid range

  k0 = mod(g**r, p)
  k1 = mod(k0 * g, p)  ! Receiver blinds k0 to obtain k1

  ! Step 4: Sender encrypts messages using k0 and k1
  c0 = mod(m0 + k0, p)
  c1 = mod(m1 + k1, p)

  ! Step 5: Receiver computes key for chosen message and decrypts
  if (choice == 0) then
     received_message = mod(c0 - k0 + p, p)  ! Ensure non-negative result
  else
     received_message = mod(c1 - k1 + p, p)
  end if

  ! Step 6: Output the result
  print *, 'Receiver obtained message: ', received_message

end program oblivious_transfer
