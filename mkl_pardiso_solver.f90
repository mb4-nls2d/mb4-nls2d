include "mkl_pardiso.f90"
module mkl_pardiso_solver 
  use mkl_pardiso
  use sparse_type
  implicit none

  logical, parameter :: use_zero_index = .false.

  type solver_t
    type(MKL_PARDISO_HANDLE) :: pt(64)
    integer :: mtype, solver, iparam(64)
    integer :: m, ncol, msglvl
  end type
contains
  subroutine solver_initialize(slv)
    type(solver_t), intent(out) :: slv

    slv%mtype = 1 ! unsymmetric but syymetric structure real matrices
    slv%solver = 0 ! sparse direct solver
    slv%iparam = 0
    slv%iparam(1) = 1 ! don't use default values
    slv%iparam(2) = 2 ! parallel nested dissection
    slv%iparam(10) = 13 ! pivot perturbation
    slv%iparam(11) = 0 ! no diagonal scaling
    slv%iparam(13) = 0 ! no matching
    slv%iparam(24) = 1 ! parallel factorization
    call pardisoinit(slv%pt, slv%mtype, slv%iparam)
  end subroutine

  subroutine solver_symbolicf(slv, A, ncol, msglvl)
    type(solver_t), intent(out) :: slv
    integer, intent(in) :: ncol, msglvl
    type(dcsr_t), intent(in):: A

    integer :: error, idum(1)
    real(8) :: dum(1)

    slv%m = A%m
    slv%ncol= ncol
    slv%msglvl = msglvl

    call pardiso(slv%pt, 1, 1, slv%mtype, 11, A%m, A%v, A%r, A%c, &
      idum, slv%ncol, slv%iparam, slv%msglvl, dum, dum, error)
    call solver_error_check(error)
  end subroutine

  subroutine solver_finalize_factor(slv)
    type(solver_t), intent(in) :: slv
    integer :: idum
    ! do nothing
    idum = slv%mtype ! supperess warning
  end subroutine

  subroutine solver_numericf(slv, A)
    type(solver_t), intent(inout) :: slv
    type(dcsr_t), intent(in) :: A

    integer :: idum(1), error
    real(8) :: dum(1)

    call pardiso(slv%pt, 1, 1, slv%mtype, 22, A%m, A%v, A%r, A%c, &
      idum, slv%ncol, slv%iparam, slv%msglvl, dum, dum,  error)
    call solver_error_check(error)
  end subroutine

  subroutine solver_solve(slv, A, B, X)
    ! solve AX = B. This breaks B after call.
    use sparse_utils
    use sparse_blas
    type(solver_t), intent(inout) :: slv
    type(dcsr_t), intent(in) :: A
    real(8), intent(inout) :: B(A%m, slv%ncol)
    real(8), intent(out) :: X(A%m, slv%ncol)

    integer :: idum(1) = 0, error
    real(8) :: t(A%m)
    
    call pardiso(slv%pt, 1, 1, slv%mtype, 33, A%m, A%v, A%r, A%c, &
      idum, slv%ncol, slv%iparam, slv%msglvl, B, X, error)
    call solver_error_check(error)
  end subroutine

  subroutine solver_finalize(slv)
    type(solver_t), intent(inout) :: slv

    integer :: idum(1), error
    real(8) :: dum(1)

    call pardiso(slv%pt, 1, 1, slv%mtype, -1, 1, dum, idum, idum, &
      idum, slv%ncol, slv%iparam, slv%msglvl, dum, dum, error)
    call solver_error_check(error)
  end subroutine

  subroutine solver_error_check(error)
    integer(4), intent(in) :: error

    if(error .ne. 0) then
      write (*,fmt='(a)',advance='no') "pardiso error: "
      select case (error)
      case (-1)
        print *, 'input inconsistent.'
      case (-2)
        print *, 'not enough memory.'
      case (-3)
        print *, 'reordering problem.'
      case (-4)
        print *, 'zero pivot, numerical fact. or iterative refinement problem.'
      case (-5)
        print *, 'unclassified (internal) error.'
      case (-6)
        print *, 'preordering failed.'
      case (-7)
        print *, 'diagonal matrix problem.'
      case (-8)
        print *, '32-bit integer overflow problem.'
      case (-10)
        print *, 'no license file found.'
      case (-11)
        print *, 'license is expired.'
      case (-12)
        print *, 'wrong username or hostname.'
      case (-100)
        print *, 'reached maximum number of Krylov-subspace iteration in iterative solver.'
      case (-101)
        print *, 'no sufficient convergence in Krylov-subspace iteration within 25 iterations.'
      case (-102)
        print *, 'error in krylov-subspace iteration.'
      case (-103)
        print *, 'break-down in Krylov-subspace iteration.'
      case default
        print *, 'unknown error??'
      end select

      call abort
    end if
  end subroutine

end module

