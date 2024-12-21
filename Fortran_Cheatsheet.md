# Fortran Cheat Sheet

## Basics

### Program Structure
```fortran
PROGRAM ProgramName
  IMPLICIT NONE
  ! Variable declarations

  ! Code

END PROGRAM ProgramName
```

### Comments
- Single-line comment: `! This is a comment`

### Variable Declaration
```fortran
INTEGER :: i
REAL :: x
DOUBLE PRECISION :: y
LOGICAL :: flag
CHARACTER(LEN=10) :: name
```

## Data Types

| Type             | Description                      |
|------------------|----------------------------------|
| INTEGER          | Whole numbers                   |
| REAL             | Single-precision floating point |
| DOUBLE PRECISION | Double-precision floating point |
| LOGICAL          | Boolean (TRUE/FALSE)            |
| CHARACTER        | Strings                         |

## Control Structures

### If-Else
```fortran
IF (condition) THEN
  ! Code
ELSE IF (another_condition) THEN
  ! Code
ELSE
  ! Code
END IF
```

### Loops

#### Do Loop
```fortran
DO i = 1, 10
  ! Code
END DO
```

#### While Loop
```fortran
DO WHILE (condition)
  ! Code
END DO
```

#### Exit and Cycle
```fortran
DO
  IF (condition) EXIT
  IF (another_condition) CYCLE
END DO
```

## Arrays

### Declaration
```fortran
REAL, DIMENSION(10) :: array1D
REAL, DIMENSION(3, 3) :: array2D
```

### Initialization
```fortran
array1D = (/1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0/)
array2D = RESHAPE((/1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0/), SHAPE(array2D))
```

### Accessing Elements
```fortran
array1D(1) = 5.0
value = array2D(2, 3)
```

## Functions and Subroutines

### Function
```fortran
FUNCTION Add(a, b)
  REAL :: Add
  REAL, INTENT(IN) :: a, b
  Add = a + b
END FUNCTION Add
```

### Subroutine
```fortran
SUBROUTINE PrintMessage(message)
  CHARACTER(LEN=*), INTENT(IN) :: message
  PRINT *, message
END SUBROUTINE PrintMessage
```

## Input/Output

### Read from Input
```fortran
READ(*, *) variable
```

### Print to Output
```fortran
PRINT *, "Hello, World!"
```

### File I/O
```fortran
OPEN(UNIT=10, FILE='data.txt', STATUS='OLD', ACTION='READ')
READ(10, *) variable
CLOSE(10)
```

## Intrinsic Functions

| Function    | Description                   |
|-------------|-------------------------------|
| `ABS(x)`    | Absolute value                |
| `SQRT(x)`   | Square root                   |
| `SIN(x)`    | Sine                          |
| `COS(x)`    | Cosine                        |
| `LOG(x)`    | Natural logarithm             |
| `EXP(x)`    | Exponential                   |
| `MOD(x, y)` | Modulus                       |
| `MAX(a, b)` | Maximum of two numbers        |
| `MIN(a, b)` | Minimum of two numbers        |

## Common Compiler Commands

### Compile
```bash
$ gfortran -o program program.f90
```

### Run
```bash
$ ./program
```

### Debug
```bash
$ gfortran -g -o program program.f90
$ gdb ./program
