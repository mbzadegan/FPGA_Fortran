! Principal Component Analysis (PCA). PCA is widely used for dimensionality reduction and feature extraction in data analytics.

! The following Fortran code performs PCA on a dataset, calculates the eigenvalues and eigenvectors of the covariance matrix, and projects the data onto the principal components.

! Compile this program with:   gfortran -o pca pca.f90 -llapack
! Then run it by: ./pca 




program PCA
    implicit none

    ! Parameters
    integer, parameter :: n = 5   ! Number of features
    integer, parameter :: m = 10  ! Number of data points

    ! Variables
    real :: data(m, n), mean(n), covariance(n, n), eigenvalues(n), eigenvectors(n, n)
    real :: standardizedData(m, n), transformedData(m, n)
    integer :: i, j

    ! Data (replace with real-world data or input from a file)
    data = reshape([ &
        2.5, 2.4, 0.5, 1.2, 3.3, &
        0.5, 0.7, 1.1, 0.3, 0.8, &
        2.2, 2.9, 2.3, 2.8, 1.6, &
        1.9, 2.2, 2.0, 1.1, 1.3, &
        3.1, 3.0, 1.8, 3.3, 3.0, &
        2.3, 2.7, 2.6, 1.5, 1.4, &
        2.7, 3.6, 3.1, 2.9, 3.5, &
        1.8, 1.9, 2.2, 0.8, 2.4, &
        3.0, 3.2, 2.5, 3.4, 3.3, &
        2.4, 2.5, 2.0, 2.1, 2.7  &
    ], [m, n])

    ! Step 1: Calculate the mean of each feature
    mean = 0.0
    do j = 1, n
        do i = 1, m
            mean(j) = mean(j) + data(i, j)
        end do
        mean(j) = mean(j) / m
    end do

    ! Step 2: Standardize the data
    do j = 1, n
        do i = 1, m
            standardizedData(i, j) = data(i, j) - mean(j)
        end do
    end do

    ! Step 3: Compute the covariance matrix
    covariance = 0.0
    do i = 1, n
        do j = 1, n
            covariance(i, j) = sum(standardizedData(:, i) * standardizedData(:, j)) / (m - 1)
        end do
    end do

    ! Step 4: Perform eigen decomposition of the covariance matrix
    call eig(n, covariance, eigenvalues, eigenvectors)

    ! Step 5: Project the data onto the principal components
    transformedData = matmul(standardizedData, eigenvectors)

    ! Output results
    print *, "Mean of each feature:"
    print *, mean

    print *, "Covariance matrix:"
    print *, covariance

    print *, "Eigenvalues:"
    print *, eigenvalues

    print *, "Eigenvectors (columns are principal components):"
    print *, eigenvectors

    print *, "Transformed Data (projection onto principal components):"
    print *, transformedData

contains
    subroutine eig(n, A, eigvals, eigvecs)
        ! Eigen decomposition using LAPACK's DSYEV routine
        integer, intent(in) :: n
        real, intent(inout) :: A(n, n)
        real, intent(out) :: eigvals(n), eigvecs(n, n)
        integer :: info, lwork
        real, allocatable :: work(:)

        lwork = max(1, 3 * n - 1)
        allocate(work(lwork))

        ! Copy A to eigvecs (LAPACK overwrites input matrix)
        eigvecs = A

        ! Call LAPACK DSYEV (symmetric eigenvalue decomposition)
        call dsyev('V', 'U', n, eigvecs, n, eigvals, work, lwork, info)

        if (info /= 0) then
            print *, "Error in eigen decomposition. Info = ", info
            stop
        end if

        deallocate(work)
    end subroutine eig

end program PCA
