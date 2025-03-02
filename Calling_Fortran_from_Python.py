import numpy as np
import ctypes

# Load the Fortran shared library
lib = ctypes.CDLL("./libmatrix_multiply.so")  # Or use libmatrix_multiply.dll on Windows

# Define the function signature for the Fortran subroutine
# The Fortran subroutine multiply_matrices(A, B, C, n) is of type:
# subroutine multiply_matrices(A, B, C, n)
#   - A, B, C are pointers to n x n matrices (arrays of floats)
#   - n is an integer (size of the matrix)

# Set the argument and return types for the ccall
lib.multiply_matrices.argtypes = [ctypes.POINTER(ctypes.c_double), 
                                  ctypes.POINTER(ctypes.c_double), 
                                  ctypes.POINTER(ctypes.c_double), 
                                  ctypes.c_int]
lib.multiply_matrices.restype = None

def multiply_matrices(A, B):
    n = A.shape[0]
    C = np.zeros((n, n), dtype=np.float64)
    
    # Convert numpy arrays to ctypes pointers
    A_ctypes = A.ctypes.data_as(ctypes.POINTER(ctypes.c_double))
    B_ctypes = B.ctypes.data_as(ctypes.POINTER(ctypes.c_double))
    C_ctypes = C.ctypes.data_as(ctypes.POINTER(ctypes.c_double))
    
    # Call the Fortran subroutine
    lib.multiply_matrices(A_ctypes, B_ctypes, C_ctypes, n)
    
    return C

# Generate two random nxn matrices A and B
n = 5  # For example, 5x5 matrices
A = np.random.rand(n, n)
B = np.random.rand(n, n)

# Perform the matrix multiplication
C = multiply_matrices(A, B)

# Print the matrices and the result
print("Matrix A:")
print(A)

print("\nMatrix B:")
print(B)

print("\nMatrix C (A * B):")
print(C)
