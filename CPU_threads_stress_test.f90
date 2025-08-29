! Here is a Fortran program to stress test the CPU, utilizing all cores and threads with OpenMP, and reporting the time consumed:
! You can change the max_iteration number on line 20
! Compile it with OpenMP support:
!        gfortran -fopenmp -fno-range-check -o CPU_threads_stress_test CPU_threads_stress_test.f90
! Run the program:
!        ./cpu_stress_test
! You can install the latest gfortran release by installing the latest GCC.(GCC13 is the latest stable realease as of January 2025)


PROGRAM CPUSressTest
  USE omp_lib
  IMPLICIT NONE

  INTEGER :: num_threads, thread_id, i
  INTEGER(8) :: start_time, end_time, clock_rate
  REAL(8) :: result, sum
  INTEGER :: max_iterations

  ! Define number of iterations for the stress test
  max_iterations = 10000000000  ! Adjust as needed for your test

  ! Get clock rate (ticks per second)
  CALL SYSTEM_CLOCK(COUNT_RATE=clock_rate)

  PRINT *, "Starting CPU Stress Test..."
  PRINT *, "Using OpenMP with maximum threads available."

  CALL SYSTEM_CLOCK(start_time)

  ! Parallel region
  result = 0.0D0
  !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(thread_id, i, sum) NUM_THREADS(OMP_GET_MAX_THREADS())
  thread_id = OMP_GET_THREAD_NUM()
  num_threads = OMP_GET_NUM_THREADS()
  sum = 0.0D0

  DO i = thread_id + 1, max_iterations, num_threads
    sum = sum + SIN(DBLE(i)) * COS(DBLE(i))  ! Arbitrary computation
  END DO

  !$OMP CRITICAL
  result = result + sum
  !$OMP END CRITICAL
  !$OMP END PARALLEL

  CALL SYSTEM_CLOCK(end_time)

  PRINT *, "Stress test completed."
  PRINT *, "Number of threads used: ", num_threads
  PRINT *, "Result of computation (arbitrary): ", result
  PRINT *, "Time consumed (seconds): ", REAL(end_time - start_time) / clock_rate
END PROGRAM CPUSressTest
