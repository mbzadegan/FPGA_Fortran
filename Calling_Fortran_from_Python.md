# Python_Fortran-HPC

## Calling a Fortran library in the Python

This example demonstrates using Fortran for performance-critical tasks like matrix multiplication while leveraging Python for high-level operations and easy integration. Using ctypes allows Python to call Fortran subroutines directly, making it a seamless and powerful combination for scientific computing tasks.

The Fortran program performs matrix multiplication, compiles it into a shared library, and then uses Python (with the ctypes library) to call the Fortran function and perform the matrix multiplication of two random 𝑛×𝑛 matrices.

- Linux -> `gfortran -shared -fPIC -o libmatrix_multiply.so matrix_multiply.f90`
- Windows -> `gfortran -shared -o libmatrix_multiply.dll matrix_multiply.f90`

