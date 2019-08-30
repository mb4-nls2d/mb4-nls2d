! most of subroutined are for testing. I don't recommend to use this program for serious projects.

module sparse_type
  implicit none
  type dcsr_t
    integer :: m, n
    logical :: is_cr_owner = .true.
    real(8), pointer :: v(:) => NULL()
    integer, pointer :: c(:) => NULL(), r(:) => NULL()
  end type
end module

module dcsr_builder
  use sparse_type
  implicit none
  integer :: numelems, allocsz, currentrow, currentcol
contains
  subroutine initialize(A, m, n, sz)
    type(dcsr_t), intent(inout) :: A
    integer, intent(in) :: m, n
    integer, optional :: sz

    if(associated(A%v)) deallocate(A%v)
    if(associated(A%c)) deallocate(A%c)
    if(associated(A%r)) deallocate(A%r)

    A%m = m
    A%n = n
    A%is_cr_owner = .true.

    currentrow = 1
    currentcol = 0
    numelems = 0
    allocsz = 0
    
    ! check for null matrix.
    if(m .eq. 0) return
    
    allocate(A%r(m+1))
    A%r(1) = 1
   
    if(n .eq. 0) then
      ! null matrix with logical rows.
      A%r(2:m+1) = 1
      return
    end if
    if(present(sz) .and. sz .ge. 1) then
      allocate(A%v(sz), A%c(sz))
      allocsz = sz
    end if
  end subroutine

  subroutine share_cr(A, B)
    ! build matrix B which has same zero-structure with A
    type(dcsr_t), intent(in) :: A
    type(dcsr_t), intent(out) :: B
    
    allocate(B%v(A%r(A%m+1)-1))
    B%m = A%m
    B%n = A%n
    B%is_cr_owner = .false.
    B%c => A%c
    B%r => A%r
  end subroutine

  subroutine append_row(A, i, d)
    type(dcsr_t), intent(inout) :: A
    integer, intent(in) :: i
    real(8), intent(in) :: d

    if( i .le. currentcol ) error stop
    if( .not. associated(A%v) .or. numelems .eq. allocsz ) then
      call resize(A)
    end if
    numelems = numelems + 1
    currentcol = i
    A%v(numelems) = d
    A%c(numelems) = i
  end subroutine

  subroutine nextrow(A)
    type(dcsr_t), intent(in) :: A

    if(currentrow == A%m + 1) error stop
    currentrow = currentrow + 1
    currentcol = 0
    A%r(currentrow) = numelems + 1
  end subroutine

  subroutine resize(A)
    type(dcsr_t), intent(inout) :: A
    real(8), pointer :: vtemp(:)
    integer, pointer :: ctemp(:)
    integer :: sztemp

    if(allocsz == 0) then
      if(associated(A%v)) deallocate(A%v)
      if(associated(A%c)) deallocate(A%c)
      allocate(A%v(1), A%c(1))
      allocsz = 1
    else
      sztemp = allocsz * 2
      allocate(vtemp(sztemp), ctemp(sztemp))
      vtemp(1:allocsz) = A%v(1:allocsz)
      ctemp(1:allocsz) = A%c(1:allocsz)
      deallocate(A%v)
      deallocate(A%c)
      A%v => vtemp
      A%c => ctemp
      allocsz = sztemp
    end if
  end subroutine

  subroutine finalize(A)
    type(dcsr_t), intent(inout) :: A
    if(associated(A%v)) deallocate(A%v)
    if(A%is_cr_owner .and. associated(A%c)) deallocate(A%c)
    if(A%is_cr_owner .and. associated(A%r)) deallocate(A%r)
  end subroutine
end module

module sparse_blas
  use sparse_type
  implicit none
#ifdef USEMKL
include "mkl.fi"
#endif

contains
  subroutine spmv_test(M, x, y)
    type(dcsr_t), intent(in) :: M
    real(8), intent(in) :: x(M%n)
    real(8), intent(out) :: y(M%m)

#ifdef USEMKL
    call mkl_dcsrmv('n', M%m, M%n, 1d0, 'giifii', &
      M%v, M%c, M%r, M%r(2:), x, 1d0, y)
#else
    integer :: i, j
    real(8) :: t
    do i = 1, M%m 
      t = 0d0
      if(M%r(i) .eq. M%r(i+1)) continue
      do j = M%r(i), M%r(i+1) - 1
        t = t + M%v(j) * x(M%c(j))
      end do
      y(i) = t
    end do
#endif
  end subroutine

  subroutine spmv_T(M, x, y)
    type(dcsr_t), intent(in) :: M
    real(8), intent(in) :: x(M%n)
    real(8), intent(out) :: y(M%m)

#ifdef USEMKL
    call mkl_dcsrmv('t', M%n, M%m, 1d0, 'giifii', &
      M%v, M%c, M%r, M%r(2:), x, 1d0, y)
#else
    integer :: i, j
    y = 0d0
    do i = 1, M%m 
      if(M%r(i) .eq. M%r(i+1)) continue
      do j = M%r(i), M%r(i+1) - 1
        y(M%c(j)) = y(M%c(j)) + M%v(j) * x(i)
      end do
    end do
#endif
  end subroutine

   subroutine spmm_test(A, x, ldx, ncol, y, ldy)
    type(dcsr_t), intent(in) :: A
    integer, intent(in) :: ncol, ldx, ldy
    real(8), intent(in) :: x(ldx, ncol)
    real(8), intent(inout) :: y(ldy, ncol)

#ifdef USEMKL
    call mkl_dcsrmm('n', A%m, ncol, A%n, 1d0, 'giifii', &
      A%v, A%c, A%r, A%r(2:), x, ldx, 0d0, y, ldy)
#else
    integer :: i
    do i=1, ncol
      call spmv_test(A, x(:,i), y(:,i))
    end do
#endif
  end subroutine
  
  subroutine spmm_T(A, x, ldx, ncol, y, ldy)
    type(dcsr_t), intent(in) :: A
    integer, intent(in) :: ncol, ldx, ldy
    real(8), intent(in) :: x(ldx, ncol)
    real(8), intent(inout) :: y(ldy, ncol)

#ifdef USEMKL
    call mkl_dcsrmm('t', A%m, ncol, A%n, 1d0, 'giifii', &
      A%v, A%c, A%r, A%r(2:), x, ldx, 0d0, y, ldy)
#else
    integer :: i
    do i=1, ncol
      call spmv_T(A, x(:,i), y(:,i))
    end do
#endif
  end subroutine

  subroutine kron_dencesq_eye(m, A, n, B)
    use dcsr_builder
    integer, intent(in) :: m, n
    real(8), intent(in) :: A(m, m)
    type(dcsr_t), intent(out) :: B 

    integer :: i, j, e

    call initialize(B, m*n, m*n, m*m*n)

    do i = 1, m*n
      e = (i-1)*m 
      B%r(i) = e + 1
      do j = 1, m
        B%v(e+j) = A((i-1)/n+1, j)
        B%c(e+j) = mod(i-1,n) + 1 + (j-1)*n
      end do
    end do
    B%r(m*n+1) = m*m*n + 1
  end subroutine

  subroutine kron_sp_eye(A, n, B)
    use dcsr_builder
    integer, intent(in) :: n
    type(dcsr_t), intent(in) :: A
    type(dcsr_t), intent(out) :: B

    integer :: sz, nrbl
    integer :: i, ii, j, p, q, pr, nc
    sz = A%r(A%m+1) - 1
    nrbl = A%m

    call initialize(B, A%m * n, A%n * n, sz*n)

    do ii = 1, nrbl
      pr = A%r(ii)
      nc = A%r(ii+1) - pr
      q = (pr-1) * n + 1
      !print *, '**', ii, q, nc
      do i = 1, n
        B%r((ii-1)*n + i) = q
        p = pr
        do j = 1, nc
          B%v(q) = A%v(p)
          B%c(q) = (A%c(p)-1) * n + i
          p = p + 1
          q = q + 1
          !print *, (ii-1)*n+i, j, B%v(q-1), B%c(q-1)
        end do
      end do
    end do
    B%r(B%m+1) = sz*n + 1
  end subroutine

  subroutine kron_eye_spsq(n, A, B)
    use dcsr_builder
    integer, intent(in) :: n
    type(dcsr_t), intent(in) :: A
    type(dcsr_t), intent(out) :: B

    integer :: ii, sz, rbl
    rbl = A%m
    sz = A%r(A%m+1) - 1

    call initialize(B, A%m * n, A%n * n, sz*n+1)

    B%r(1) = 1

    do ii = 1, n
      B%v((ii-1)*sz+1:ii*sz) = A%v(1:sz)
      B%c((ii-1)*sz+1:ii*sz) = A%c(1:sz) + (ii-1)*rbl
      B%r((ii-1)*rbl+2:ii*rbl+1) = A%r(2:rbl+1) + (ii-1)*sz
    end do
    
  end subroutine

  function num_unique(x, m, y, n)
    integer :: num_unique
    integer, intent(in) :: x(:), y(:)
    integer, intent(in) :: m, n

    integer :: i, j, num
    i = 1
    j = 1
    num = 0

    do
      if(i .eq. m+1 .or. j .eq. n+1) exit
      if(x(i) .eq. y(j)) then
        num = num + 1
        i = i + 1
        j = j + 1
      else if(x(i) .le. y(j)) then
        num = num + 1
        i = i + 1
      else
        num = num + 1
        j = j + 1
      end if
    end do
    num_unique = num + (m+1-i) + (n+1-j)
  end function

  subroutine spadd(A, alpha, B, beta, C)
    use dcsr_builder
    type(dcsr_t), intent(in) :: A, B
    real(8), intent(in) :: alpha, beta
    type(dcsr_t), intent(out) :: C
    
    integer :: i, sz=0, m, n, rsza, rszb, p, q, ca, cb

    if(A%m .ne. B%m .or. A%n .ne. B%n) error stop
    m = A%m
    n = A%n
    
    ! count the size of C
    do i=1,m
      rsza = A%r(i+1) - A%r(i)
      rszb = B%r(i+1) - B%r(i)
      sz = sz + num_unique(A%c(A%r(i):A%r(i+1)-1), rsza, B%c(B%r(i):B%r(i+1)-1), rszb)
    end do

    call initialize(C, m, n, sz)

    do i=1, m
      rsza = A%r(i+1)
      rszb = B%r(i+1)
      p = A%r(i)
      q = B%r(i)
      do
        if(p .eq. rsza .or. q .eq. rszb) exit
        ca = A%c(p)
        cb = B%c(q)
        if(ca .eq. cb) then
          !print *, '$$AB', ca, A%v(p), cb, B%v(q)
          call append_row(C, ca, A%v(p) * alpha + B%v(q) * beta)
          p = p + 1
          q = q + 1
        else if(ca .lt. cb) then
          !print *, '$$A1', ca, A%v(p)
          call append_row(C, ca, A%v(p) * alpha)
          p = p + 1
        else
          !print *, '$$B1', cb, B%v(q)
          call append_row(C, cb, B%v(q) * beta)
          q = q + 1
        end if
      end do
      do
        if(p .eq. rsza) exit
        !print *, '$$A2', ca, A%v(p)
        ca = A%c(p)
        call append_row(C, ca, A%v(p) * alpha)
        p = p + 1
      end do
      do
        if(q .eq. rszb) exit
        !print *, '$$B2', cb, B%v(q)
        cb = B%c(q)
        call append_row(C, cb, B%v(q) * beta)
        q = q + 1
      end do
      call nextrow(C)
    end do
  end subroutine

end module

module sparse_utils
  use sparse_type
  implicit none

  
contains
  subroutine printsp(M)
    type(dcsr_t), intent(in) :: M
    integer :: i, j,  pr, e
    e = 1
    do i = 1, M%m
      pr = M%r(i)
      if( M%r(i+1) .eq. pr ) continue
      do j = 1, M%c(M%r(i+1)-1)
        if( j .eq. M%c(e) ) then
          write (*,fmt='(a)',advance='no') "*"
          e = e + 1
        else
          write (*,fmt='(a)',advance='no') " "
        end if
      end do
      print *, " "
    end do
  end subroutine

  subroutine f2cindex(A)
    type(dcsr_t), intent(inout) ::A
    A%r = A%r - 1
    A%c = A%c - 1
  end subroutine
  subroutine c2findex(A)
    type(dcsr_t), intent(inout) :: A
    A%r = A%r + 1
    A%c = A%c + 1
  end subroutine

  subroutine dnscsr(m, n, A, lda, B)
    use dcsr_builder
    integer, intent(in) :: m, n, lda
    real(8), intent(in) :: A(lda, n)
    type(dcsr_t), intent(out) :: B

    integer :: numelem, i, j
    numelem = 0

    ! count non-zeros, allocate and build csr.
    do j = 1, n
      do i = 1, m
        if( A(j, i) .ne. 0d0 ) numelem = numelem + 1
      end do
    end do

    call initialize(B, m, n, numelem)
    B%r(m+1) = numelem + 1
    
    numelem = 1
    do j = 1, n
      B%r(j) = numelem
      do i = 1, m
        if( A(j, i) .ne. 0d0 ) then
          B%v(numelem) = A(j, i)
          B%c(numelem) = i
          numelem = numelem + 1
        end if
      end do
    end do
  end subroutine

  subroutine csrdns(A, B, lda)
    type(dcsr_t), intent(in) :: A
    integer, intent(in) :: lda
    real(8), intent(out) :: B(lda, A%n)

    integer :: i, j

    B(1:A%m, 1:A%n) = 0d0

    do i=1,A%m
      if(A%r(i) .eq. A%r(i+1)) continue
      do j=A%r(i),A%r(i+1)-1
        B(i,A%c(j)) = A%v(j)
      end do
    end do
  end subroutine

end module


#ifdef UNIT_TEST
program main
  use sparse_type
  use dcsr_builder
  use sparse_blas
  use sparse_utils
  use pardiso_solver

  implicit none

  type(dcsr_t) :: A, B, C, D, E, F, G
  real(8) :: x(4), y(4), M(4,4), s(4)

  real(8) :: T(2,2)

  type(solver_init_t) :: env
  type(solver_t) :: slv
  integer :: error

  A%m = 4
  A%n = 4
  allocate(A%v(7), A%c(7), A%r(5))
  A%v = (/ 1d0, 2d0, 1d0, 2d0, 1d0, 2d0, 1d0 /)
  A%c = (/ 1, 2, 2, 3, 3, 4, 4 /)
  A%r = (/ 1, 3, 5, 7, 8 /)
  x = (/ 1d0, 2d0, 3d0, 4d0 /)
  call printsp(A)

  call spmv_test(A, x, y)
  print *, y

  M(:,:) = 0.
  M(1,1) = 1.
  M(1,2) = 2.
  M(2,2) = 1.
  M(2,3) = 2.
  M(3,3) = 1.
  M(3,4) = 2.
  M(4,4) = 1.
  call dnscsr(4, 4, M, 4, B)
  call printsp(B)
  print *, int(B%v)
  print *, B%c
  print *, B%r

  T(1,1) = 2.
  T(1,2) = 1.
  T(2,1) = 3.
  T(2,2) = 4.
  call kron_dencesq_eye(2,T,5,C)
  call printsp(C)
  print *, int(C%v)
  print *, C%c
  print *, C%r

  call kron_sp_eye(A, 3, D)
  call printsp(D)
  print *, int(D%v)
  print *, D%c
  print *, D%r

  call kron_eye_spsq(3, A, E)
  call printsp(E)
  print *, int(E%v)
  print *, E%c
  print *, E%r

  call spadd(D, 1d0, E, 2d0, G)
  call printsp(G)
  print *, int(G%v)
  print *,  G%c
  print *, G%r

  call initialize(F, 4, 4)
  call append_row(F, 1, 3d0)
  call append_row(F, 4, 1d0)
  call nextrow(F)
  call append_row(F, 1, 1d0)
  call append_row(F, 3, 3d0)
  call nextrow(F)
  call append_row(F, 4, 1d0)
  call nextrow(F)
  call append_row(F, 1, 1d0)
  call append_row(F, 4, 4d0)
  call nextrow(F)
  call printsp(F)
  print *, int(F%v)
  print *, F%c
  print *, F%r

  call solver_initialize(env)
  call pardiso_chkmatrix(11, 4, A%v, A%c, A%r, error)
  print *, error
  call pardiso_printstats(11, 4, A%v, A%c, A%r, 1, y, error)
  print *, error
  call solver_analyze(env, slv, 1, A, 1, 1)
  call solver_factorize(env, slv, 1, A)
  s = y
  call solver_solve(env, slv, 1, A, s)
  print *, int(x)
  print *, int(y)
  print *, int(s)
  call solver_finalize(env, slv)
end program

#endif


