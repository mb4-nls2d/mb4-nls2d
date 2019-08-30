# mb4-nls2d

## Brief description
This is a FORTRAN90 implementation of the energy-preserving integrator MB4 applied to the nonlinear Schrodinger equation on a 2-dimensional regular grid. The MB4 method has 3-way parallelism and hence the code is designed to run on three MPI processes. The code uses a sparse direct solver PARDISO (as implemented in Intel MKL) to solve the linear simultaneous equations for time stepping.

## Structure of the code
nls2d_mb4spmpi.f90: a file containing the main routine  
mkl_pardiso_solver.f90, mkl_pardiso_solver.f90, csrchk.c: subroutines  
micc: makefile  

## Prerequisite software
Intel FORTRAN compiler (mpiifort)  
Intel C compiler (mpiicc)  
LAPACK library as implemented in Intel Math Kernel Library  
PARDISO sparse direct solver as implemented in Intel Math Kernel Library  

## Compilation
Type "make -f micc".  

## Execution
Type "mpirun -np 3 ./a.out XX YY ZZ".  
This code is specialized for three MPI processes. XX (integer) denotes the number of grid points in each direction, so the total number of grid points is XX*XX. YY (real number) is the ending time of the simulation. Since the time step is fixed to 0.01 (see the main routine), YY/0.01 steps of time evolution will be performed. ZZ is a character string specifying the output file name. When the code is executed, the norm of the wave function, the total energy and the participation ratio will be output to the standard output at each time step. More detailed information will be written into the ZZ file.
