! This Fortran program simulates a simple blockchain, generating blocks with an index, timestamp, and hash. It uses sha256sum from the shell to compute hashes.


program blockchain
  use iso_fortran_env
  implicit none

  integer, parameter :: max_blocks = 10
  type :: block
    integer :: index
    character(len=64) :: previous_hash
    character(len=64) :: hash
    character(len=20) :: timestamp
  end type block

  type(block), dimension(max_blocks) :: chain
  integer :: num_blocks = 1

  ! Initialize first block (Genesis block)
  chain(1)%index = 1
  chain(1)%previous_hash = "0"
  call get_timestamp(chain(1)%timestamp)
  call compute_hash(chain(1), chain(1)%hash)

  call print_block(chain(1))

contains

  subroutine get_timestamp(ts)
    character(len=20), intent(out) :: ts
    write(ts, '(I10)') int(etime([0.0, 0.0]))
  end subroutine get_timestamp

  subroutine compute_hash(b, hash)
    type(block), intent(in) :: b
    character(len=64), intent(out) :: hash
    character(len=100) :: data

    write(data, '(I0,A,A)') b%index, trim(b%previous_hash), trim(b%timestamp)
    call sha256(data, hash)
  end subroutine compute_hash

  subroutine sha256(input, output)
    character(len=*), intent(in) :: input
    character(len=64), intent(out) :: output
    character(len=128) :: command, result
    command = "echo -n '" // trim(input) // "' | sha256sum"
    call execute_command_line(command, result)
    output = trim(result)
  end subroutine sha256

  subroutine print_block(b)
    type(block), intent(in) :: b
    print *, "Block:", b%index
    print *, "Timestamp:", trim(b%timestamp)
    print *, "Previous Hash:", trim(b%previous_hash)
    print *, "Hash:", trim(b%hash)
    print *, "----------------------------------"
  end subroutine print_block

end program blockchain
