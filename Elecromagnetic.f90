! Here’s a Fortran code example to solve a problem of calculating the eigenvalues and eigenfunctions of TE (Transverse Electric) modes in a rectangular waveguide using the finite difference method (FDM).


! Program: TE_Mode_Solver.f90
! Purpose: Calculate TE modes in a rectangular waveguide using FDM
PROGRAM TE_Mode_Solver
  IMPLICIT NONE
  INTEGER, PARAMETER :: dp = KIND(1.0D0)  ! Double precision
  INTEGER, PARAMETER :: Nx = 50, Ny = 50  ! Grid size
  INTEGER :: i, j, iter, max_iter
  REAL(dp) :: dx, dy, beta_sq, tol, err
  REAL(dp), DIMENSION(0:Nx, 0:Ny) :: V, V_new, Laplace

  ! Waveguide dimensions (meters)
  REAL(dp), PARAMETER :: a = 0.05_dp, b = 0.02_dp
  ! Convergence parameters
  tol = 1.0D-6
  max_iter = 10000

  ! Grid spacing
  dx = a / Nx
  dy = b / Ny

  ! Initialize potential array
  V = 0.0_dp
  Laplace = 0.0_dp

  ! Boundary conditions for TE mode (e.g., zero at edges)
  V(:, 0) = 0.0_dp
  V(:, Ny) = 0.0_dp
  V(0, :) = 0.0_dp
  V(Nx, :) = 0.0_dp

  ! Main iterative solver (FDM)
  iter = 0
  err = 1.0D10
  DO WHILE (err > tol .AND. iter < max_iter)
     err = 0.0_dp
     DO i = 1, Nx - 1
        DO j = 1, Ny - 1
           Laplace(i, j) = (V(i-1, j) + V(i+1, j)) / dx**2 + &
                           (V(i, j-1) + V(i, j+1)) / dy**2
           V_new(i, j) = (Laplace(i, j)) / (2.0D0 * (1.0D0 / dx**2 + 1.0D0 / dy**2))
           err = MAX(err, ABS(V_new(i, j) - V(i, j)))
           V(i, j) = V_new(i, j)
        END DO
     END DO
     iter = iter + 1
  END DO

  ! Calculate eigenvalue (beta^2)
  beta_sq = 0.0_dp
  DO i = 1, Nx - 1
     DO j = 1, Ny - 1
        beta_sq = beta_sq + (V(i, j) * Laplace(i, j)) * dx * dy
     END DO
  END DO

  ! Output results
  PRINT *, "Converged after iterations: ", iter
  PRINT *, "Eigenvalue (Beta^2): ", beta_sq
  PRINT *, "Potential distribution saved to 'TE_Mode_Potential.dat'"

  ! Write potential to file
  OPEN(UNIT=10, FILE="TE_Mode_Potential.dat", STATUS="REPLACE")
  DO i = 0, Nx
     WRITE(10, *) (V(i, j), j = 0, Ny)
  END DO
  CLOSE(10)

END PROGRAM TE_Mode_Solver
