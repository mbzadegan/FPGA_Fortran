program oblivious_transfer
    implicit none
    integer :: K, K_receiver, choice
    character(len=20) :: M0, M1, Enc_M0, Enc_M1, Received_Message

    ! Sender's messages
    M0 = "Secret Message 0"
    M1 = "Secret Message 1"

    ! Generate a random key for encryption
    call random_seed()
    call random_number(K)
    K = int(K * 100) + 1  ! Generate an integer key between 1 and 100

    ! Encrypt messages using XOR with key K
    Enc_M0 = xor_encrypt(M0, K)
    Enc_M1 = xor_encrypt(M1, K)

    ! Receiver chooses which message to retrieve (0 for M0 or 1 for M1)
    print *, "Enter choice (0 or 1):"
    read *, choice

    ! Determine receiver's key based on choice
    if (choice == 0) then
        K_receiver = K  ! Receiver uses the actual key if they choose M0
    else
        K_receiver = K + 1  ! Receiver generates a different key for M1 (simulates inability to decrypt the other message)
    end if

    ! Decrypt the chosen message
    if (choice == 0) then
        Received_Message = xor_encrypt(Enc_M0, K_receiver)
    else
        Received_Message = xor_encrypt(Enc_M1, K_receiver)
    end if

    ! Display the results
    print *, "Sender's Messages: M0 = ", M0, ", M1 = ", M1
    print *, "Encrypted Messages: Enc_M0 = ", Enc_M0, ", Enc_M1 = ", Enc_M1
    print *, "Receiver's Choice:", choice
    print *, "Receiver's Retrieved Message:", trim(Received_Message)

contains

    ! Simple XOR encryption function for character strings
    function xor_encrypt(msg, key) result(enc_msg)
        character(len=*), intent(in) :: msg
        integer, intent(in) :: key
        character(len=len(msg)) :: enc_msg
        integer :: i

        do i = 1, len(msg)
            enc_msg(i:i) = char(iachar(msg(i:i)) xor key)
        end do
    end function xor_encrypt

end program oblivious_transfer
