! vim: set tabstop=1 sts=2 expandtab:
#ifdef USEMKL
include 'mkl_service.f90'
#endif

module mb4_m
  use sparse_utils
  use sparse_blas
  use omp_lib
  use mpi
#ifdef USEMKL
  !use mkl_service
#endif
  implicit none
    !include 'mpif.h'

  real(8), parameter :: mb4a1(60)=(/&
    582600d0, 70800d0, -307800d0, 680400d0, 70800d0, 43200d0, 43200d0, 243000d0, -388800d0, 680400d0,&
    582600d0, 47200d0, -205200d0, 453600d0, 23600d0, 14400d0, 14400d0, 81000d0, -129600d0, 226800d0,&
    582600d0, 680400d0, -307800d0, 680400d0, -388800d0, 243000d0,&
    23600d0, 47200d0, 14400d0, 14400d0, 582600d0, 453600d0, -205200d0, 226800d0, -129600d0, 81000d0,&
    -729000d0, -874800d0, -874800d0,&
    -102600d0, 14400d0, 162000d0, -129600d0, 226800d0, 453600d0, -129600d0, -729000d0, -583200d0, -291600d0,&
    -729000d0,&
    226800d0, 14400d0, -129600d0, 453600d0, -102600d0, -129600d0, 162000d0, -291600d0, -583200d0, -729000d0/)
  real(8), parameter :: mb4b1(60)=(/&
    -33111d0, -4716d0, 22761d0, -55728d0, 756d0, 6156d0, -13284d0, -6561d0, 61236d0, -78732d0,&
    -33111d0, -3144d0, 15174d0, -37152d0, 252d0, 2052d0, -4428d0, -2187d0, 20412d0, -26244d0,&
    9549d0, 12960d0, -6723d0, 14580d0, 2916d0, -9477d0,&
    -1572d0, 504d0, 2052d0, -4428d0, 9549d0, 8640d0, -4482d0, 4860d0, 972d0, -3159d0,&
    -6561d0, -52488d0, 52488d0,&
    7587d0, 2052d0, -4374d0, 20412d0, 4320d0, 9720d0, 972d0, -6561d0, -34992d0, 17496d0,&
    -137781d0,&
    -18576d0, -4428d0, 20412d0, -52488d0, -2241d0, 972d0, -6318d0, -17496d0, 34992d0, -137781d0/)
  real(8), parameter :: mb4a2(60)=(/&
    582600d0, 23600d0, -102600d0, 226800d0,&
    47200d0, 47200d0, 14400d0, 14400d0,&
    -205200d0, 14400d0, 162000d0, -129600d0,&
    453600d0, 14400d0, -129600d0, 453600d0,&
    23600d0, 582600d0, 226800d0, -102600d0,&
    14400d0, 453600d0, 453600d0, -129600d0,&
    14400d0, -205200d0, -129600d0, 162000d0,&
    81000d0, 226800d0, -729000d0, -291600d0,&
    -129600d0, -129600d0, -583200d0, -583200d0,&
    226800d0, 81000d0, -291600d0, -729000d0,&
    582600d0, 70800d0, -307800d0, 680400d0,&
    70800d0, 43200d0, 43200d0, 243000d0, -388800d0, 680400d0, 582600d0,&
    680400d0, -307800d0, 680400d0, -388800d0, 243000d0, -729000d0, -874800d0, -874800d0, -729000d0/)
  real(8), parameter :: mb4b2(60)=(/&
    -33111d0, -1572d0, 7587d0, -18576d0,&
    -3144d0, 504d0, 2052d0, -4428d0,&
    15174d0, 2052d0, -4374d0, 20412d0,&
    -37152d0, -4428d0, 20412d0, -52488d0,&
    252d0, 9549d0, 4320d0, -2241d0,&
    2052d0, 8640d0, 9720d0, 972d0,&
    -4428d0, -4482d0, 972d0, -6318d0,&
    -2187d0, 4860d0, -6561d0, -17496d0,&
    20412d0, 972d0, -34992d0, 34992d0,&
    -26244d0, -3159d0, 17496d0, -137781d0,&
    -33111d0, -4716d0, 22761d0, -55728d0,&
    756d0, 6156d0, -13284d0, -6561d0, 61236d0, -78732d0,&
    9549d0, 12960d0, -6723d0, 14580d0, 2916d0, -9477d0, -6561d0, -52488d0, 52488d0, -137781d0/)
  real(8), parameter :: mb4a3(60)=(/&
    145650d0, 17700d0, -76950d0, 170100d0, 17700d0, 10800d0, 10800d0, 60750d0, -97200d0, 170100d0,&
    145650d0, 11800d0, -51300d0, 113400d0, 5900d0, 3600d0, 3600d0, 20250d0, -32400d0, 56700d0,&
    145650d0, 170100d0, -76950d0, 170100d0, -97200d0, 60750d0,&
    5900d0, 11800d0, 3600d0, 3600d0, 145650d0, 113400d0, -51300d0, 56700d0, -32400d0, 20250d0,&
    -182250d0, -218700d0, -218700d0,&
    -25650d0, 3600d0, 40500d0, -32400d0, 56700d0, 113400d0, -32400d0, -182250d0, -145800d0, -72900d0,&
    -182250d0,&
    56700d0, 3600d0, -32400d0, 113400d0, -25650d0, -32400d0, 40500d0, -72900d0, -145800d0, -182250d0/)
  real(8), parameter :: mb4b3(60)=(/&
    11223d0, 1674d0, -7695d0, 19278d0, 306d0, -648d0, 4212d0, 3645d0, -23328d0, 27702d0,&
    11223d0, 1116d0, -5130d0, 12852d0, 102d0, -216d0, 1404d0, 1215d0, -7776d0, 9234d0,&
    558d0, 2106d0, -324d0, 4374d0, -8748d0, 4374d0,&
    558d0, 204d0, -216d0, 1404d0, 558d0, 1404d0, -216d0, 1458d0, -2916d0, 1458d0,&
    19683d0, 13122d0, -13122d0,&
    -2565d0, -216d0, 2430d0, -7776d0, 702d0, 2916d0, -2916d0, 19683d0, 8748d0, -4374d0,&
    52488d0,&
    6426d0, 1404d0, -7776d0, 18468d0, -108d0, -2916d0, 2916d0, 4374d0, -8748d0, 52488d0/)
  real(8), parameter :: mb4a4(60)=(/&
    145650d0, 5900d0, -25650d0, 56700d0,&
    11800d0, 11800d0, 3600d0, 3600d0,&
    -51300d0, 3600d0, 40500d0, -32400d0,&
    113400d0, 3600d0, -32400d0, 113400d0,&
    5900d0, 145650d0, 56700d0, -25650d0,&
    3600d0, 113400d0, 113400d0, -32400d0,&
    3600d0, -51300d0, -32400d0, 40500d0,&
    20250d0, 56700d0, -182250d0, -72900d0,&
    -32400d0, -32400d0, -145800d0, -145800d0,&
    56700d0, 20250d0, -72900d0, -182250d0,&
    145650d0, 17700d0, -76950d0, 170100d0,&
    17700d0, 10800d0, 10800d0, 60750d0, -97200d0, 170100d0,&
    145650d0, 170100d0, -76950d0, 170100d0, -97200d0, 60750d0, -182250d0, -218700d0, -218700d0, -182250d0/)
  real(8), parameter :: mb4b4(60)=(/&
    11223d0, 558d0, -2565d0, 6426d0,&
    1116d0, 204d0, -216d0, 1404d0,&
    -5130d0, -216d0, 2430d0, -7776d0,&
    12852d0, 1404d0, -7776d0, 18468d0,&
    102d0, 558d0, 702d0, -108d0,&
    -216d0, 1404d0, 2916d0, -2916d0,&
    1404d0, -216d0, -2916d0, 2916d0,&
    1215d0, 1458d0, 19683d0, 4374d0,&
    -7776d0, -2916d0, 8748d0, -8748d0,&
    9234d0, 1458d0, -4374d0, 52488d0,&
    11223d0, 1674d0, -7695d0, 19278d0,&
    306d0, -648d0, 4212d0, 3645d0, -23328d0, 27702d0,&
    558d0, 2106d0, -324d0, 4374d0, -8748d0, 4374d0, 19683d0, 13122d0, -13122d0, 52488d0/)
  real(8), parameter :: mb4a5(54)=(/&
    357d0, 60d0, -243d0, 648d0, 60d0, 108d0, 108d0, 243d0, -972d0, 972d0,&
    357d0, 40d0, -162d0, 432d0, 20d0, 36d0, 36d0, 81d0, -324d0, 324d0,&
    357d0, 648d0, -243d0, 972d0, -972d0, 243d0,&
    20d0, 40d0, 36d0, 36d0, 357d0, 432d0, -162d0, 324d0, -324d0, 81d0,&
    2187d0,&
    -81d0, 36d0, 162d0, -324d0, 216d0, 648d0, -324d0, 2187d0,&
    2187d0,&
    216d0, 36d0, -324d0, 648d0, -81d0, -324d0, 162d0, 2187d0/)
  real(8), parameter :: mb4a6(54)=(/&
    357d0, 20d0, -81d0, 216d0,&
    40d0, 40d0, 36d0, 36d0,&
    -162d0, 36d0, 162d0, -324d0,&
    432d0, 36d0, -324d0, 648d0,&
    20d0, 357d0, 216d0, -81d0,&
    36d0, 432d0, 648d0, -324d0,&
    36d0, -162d0, -324d0, 162d0,&
    81d0, 324d0, 2187d0,&
    -324d0, -324d0,&
    324d0, 81d0, 2187d0,&
    357d0, 60d0, -243d0, 648d0, 60d0, 108d0, 108d0, 243d0, -972d0, 972d0,&
    357d0, 648d0, -243d0, 972d0, -972d0, 243d0,&
    2187d0, 2187d0/) 

contains

  subroutine set_forward_difference_matrix(n, d, FD)
    ! MATLAB: FD=1/d*spdiags([ones(n,1),-ones(n,1),ones(n,1)],[-n+1,0,1],n,n);
    use dcsr_builder
    integer, intent(in) :: n
    real(8), intent(in) :: d
    type(dcsr_t), intent(out) :: FD

    integer :: i
    real(8) :: x1, x2
    x1 = -1d0/d; x2 = -x1

    call initialize(FD, n, n, 2*n)
    do i=1, n-1
      call append_row(FD, i, x1)
      call append_row(FD, i+1, x2)
      call nextrow(FD)
    end do
    call append_row(FD, 1, x2)
    call append_row(FD, n, x1)
    call nextrow(FD)
  end subroutine

  subroutine set_second_difference_matrix(n, d, D2)
    ! MATLAB: D2=1/(d^2)*spdiags([ones(n,1),ones(n,1),-2*ones(n,1),ones(n,1),ones(n,1)],[-n+1,-1,0,1,n-1],n,n);
    use dcsr_builder
    integer, intent(in) :: n
    real(8), intent(in) :: d
    type(dcsr_t), intent(out) :: D2

    integer :: i
    real(8) :: x1, x2
    x1 = -2d0/d**2; x2 = 1d0/d**2
    call initialize(D2, n, n, 3*n)
    
    call append_row(D2, 1, x1)
    call append_row(D2, 2, x2)
    call append_row(D2, n, x2)
    call nextrow(D2)

    do i=2, n-1
      call append_row(D2, i-1, x2)
      call append_row(D2, i, x1)
      call append_row(D2, i+1, x2)
      call nextrow(D2)
    end do
    call append_row(D2, 1, x2)
    call append_row(D2, n-1, x2)
    call append_row(D2, n, x1)
    call nextrow(D2)
  end subroutine

  subroutine set_difference_xy_matrix(nx, ny, D2x, D2y, A)
    use dcsr_builder
    use sparse_blas
    ! MATLAB: A = -(kron(eye(ny),D2x)+kron(D2y,eye(nx)));
    integer, intent(in) :: nx, ny
    type(dcsr_t), intent(in) :: D2x, D2y
    type(dcsr_t), intent(out) :: A

    type(dcsr_t) :: k1, k2

    call kron_eye_spsq(ny, D2x, k1)
    call kron_sp_eye(D2y, nx, k2)
    call spadd(k1, -1d0, k2, -1d0, A)

    call finalize(k1)
    call finalize(k2)
  end subroutine

  subroutine add_potential_matrix(nx, ny, A0, V, A, V0)
    use dcsr_builder
    use sparse_blas
    ! MATLAB: A = -(kron(eye(ny),D2x)+kron(D2y,eye(nx)));
    integer, intent(in) :: nx, ny
    type(dcsr_t), intent(inout) :: A0, V
    type(dcsr_t), intent(out) :: A
    real(8), intent(in) :: V0
    integer :: i, x, y, j
    double precision :: dx, dy

    type(dcsr_t) :: k1, k2

    call initialize(V, nx*ny, nx*ny, nx*ny)
    
    ! call append_row(V, 1, -10d0)
    ! call nextrow(V)

    do j=1, ny
      do i=1, nx
        if (j == ny/2+1 .and. i == nx/2+1) then
          call append_row(V, i+(j-1)*nx, V0)
        else
          call append_row(V, i+(j-1)*nx, 0d0)
        end if
        call nextrow(V)
      end do
    end do

    !do i=1, nx*ny
       !y=(i-1)/nx
       !x=mod(i-1,nx)
       !dy=y/dble(ny)
       !dx=x/dble(nx)
      !call append_row(V, i, -5d0*exp(-2d0*(dx-0.5)**2-2d0*(dy-0.5)**2))
      !call nextrow(V)
    !end do

    call spadd(A0, 1d0, V, 1d0, A)

    call finalize(A0)
    !call finalize(V)
  end subroutine

 subroutine set_initial(nx, ny, dx, dy, U)
    ! MATLAB: U = ones(M1,M2) + 2*repmat(cos(x'),[1,M2])+2*repmat(cos(y'),[1,M1])';
    integer, intent(in) :: nx, ny
    real(8), intent(in) :: dx, dy
    complex(8), intent(out) :: U(nx, ny)

    real(8) :: x, y
    integer :: i, j

    do i = 1, nx
      do j = 1, ny
        x = dx * (i - 1)
        y = dy * (j - 1)
        U(i, j) = 1d0 + 2d0 * cos(x) + 2d0 * cos(y)
      end do
    end do
  end subroutine set_initial

  subroutine integrate_gen_matE_integrand(c1, c2, c3, Mmat, Emat)
    real(8), intent(in) :: c1, c2, c3, Mmat(3, 3)
    real(8), intent(out) :: Emat(3, 3)

    integer, parameter :: n = 1000
    real(8), parameter :: h = 1d0 / real(n, 8)
    integer :: i, j, k
    real(8) :: acc

    do i = 1, 3
      do j = 1, 3
        acc = (gen_matE_integrand(c1, c2, c3, Mmat, i, j, 0d0) + &
             gen_matE_integrand(c1, c2, c3, Mmat, i, j, 1d0)) / 2d0
        do k = 1, n - 1
          acc = acc + gen_matE_integrand(c1, c2, c3, Mmat, i, j, h * k)
        end do
        Emat(i, j) = h * acc
      end do
    end do
  end subroutine integrate_gen_matE_integrand

  real(8) function gen_matE_integrand(c1, c2, c3, Mmat, i, j, s)
    ! MATLAB (Mmat by eta, a, b, c, d, e, f is pre-calculated in gen_matE):
    !l1 = @(s) s./c1.*(s-c2)./(c1-c2).*(s-c3)./(c1-c3);
    !l2 = @(s) s./c2.*(s-c1)./(c2-c1).*(s-c3)./(c2-c3);
    !l3 = @(s) s./c3.*(s-c1)./(c3-c1).*(s-c2)./(c3-c2);
    !
    !eta = 300*theta-1;
    !a = -12-12*eta;
    !b = 6 + 6*eta;
    !c = -2 - 2*eta;
    !d = -12 - 18*eta;
    !e = 3*eta;
    !f = 3-eta;
    !
    !A = @(t,s) a.*t.^3.*s.^2 + 2.*b.*t.^3.*s + c.*t.^3 + 3.*b.*t.^2.*s.^2 + d.*t.^2.*s + e.*t.^2 ...
    !    + 3.*c.*t.*s.^2 + 2.*e.*t.*s + f.*t;
    !
    !c11 = @(s) A(c1,s).*l1(s);
    !c12 = @(s) A(c1,s).*l2(s);
    !c13 = @(s) A(c1,s).*l3(s);
    !
    !c21 = @(s) A(c2,s).*l1(s);
    !c22 = @(s) A(c2,s).*l2(s);
    !c23 = @(s) A(c2,s).*l3(s);
    !
    !c31 = @(s) A(c3,s).*l1(s);
    !c32 = @(s) A(c3,s).*l2(s);
    !c33 = @(s) A(c3,s).*l3(s);
    real(8), intent(in) :: c1, c2, c3, Mmat(3, 3), s
    integer, intent(in) :: i, j

    real(8) :: lj, taus(3), ss(3), c

    if (j == 1) then
      lj = (s / c1) * (s - c2) / (c1 - c2) * (s - c3) / (c1 - c3)
    else if (j == 2) then
      lj = (s / c2) * (s - c1) / (c2 - c1) * (s - c3) / (c2 - c3)
    else if (j == 3) then
      lj = (s / c3) * (s - c1) / (c3 - c1) * (s - c2) / (c3 - c2)
    end if

    if (i == 1) then
      c = c1
    else if (i == 2) then
      c = c2
    else if (i == 3) then
      c = c3
    end if

    taus(1) = c
    taus(2) = c * c / 2d0
    taus(3) = c * c * c / 3d0
    ss(1) = 1d0
    ss(2) = s
    ss(3) = s * s
    gen_matE_integrand = dot_product(taus, matmul(Mmat, ss)) * lj
  end function gen_matE_integrand

  subroutine mb4func_left(n, A, theta, p0, q0, PQ, Y, work)
    integer, intent(in) :: n
    real(8), intent(in) :: theta, p0(n), q0(n), PQ(n,6)
    real(8), intent(out) :: Y(n, 6), work(n, 6)
    type(dcsr_t), intent(in) :: A

    real(8) :: c1(4), c2(4), c3(4)
    real(8) :: b11088(4) = (/ 1d0, 1d0, -1d0, -1d0 /) * 1108800d0
    real(8) :: b27220(4) = (/ 1d0, 1d0, -1d0, -1d0 /) * 277200d0
    integer :: i

    c1 = (/-68376d0, 12936d0, 16632d0, -182952d0/) + b11088 * theta
    c2 = (/24024d0, 3696d0, 16632d0, 66528d0/) + b27220 * theta
    c3 = (/840d0, 840d0, 2520d0, 2520d0/)

    do concurrent(i=1:n)
      work(i, 1) = c1(1) * q0(i) + c1(2) * PQ(i,6)+ c1(3) * PQ(i,4) + c1(4) * PQ(i,2)
      work(i, 3) = c2(1) * q0(i) + c2(2) * PQ(i,6)+ c2(3) * PQ(i,4) + c2(4) * PQ(i,2)
      work(i, 5) = c3(1) * q0(i) + c3(2) * PQ(i,6)+ c3(3) * PQ(i,4) + c3(4) * PQ(i,2)
    end do

    do concurrent(i=1:n)
      work(i, 2) = c1(1) * p0(i) + c1(2) * PQ(i,5) + c1(3) * PQ(i,3)+ c1(4) * PQ(i,1)
      work(i, 4) = c2(1) * p0(i) + c2(2) * PQ(i,5) + c2(3) * PQ(i,3)+ c2(4) * PQ(i,1)
      work(i, 6) = c3(1) * p0(i) + c3(2) * PQ(i,5) + c3(3) * PQ(i,3)+ c3(4) * PQ(i,1)
    end do
    call spmm_test(A, work, n, 6, Y, n)
  end subroutine

  pure real(8) function poly4x2(x, a, b, c, d)
    ! second order polynomial of 4 term
    real(8), intent(in) :: x(10), a, b, c, d
    poly4x2 = &
      a * (x(1)*a + x(2)*b + x(3)*c + x(4)*d) + &
      b * (x(5)*b + x(6)*c + x(7)*d) + &
      c * (x(8)*c + x(9)*d) + &
      x(10) * (d**2)
  end function
  pure real(8) function poly3x2(x, a, b, c)
    real(8), intent(in) :: x(6), a, b, c
    poly3x2 = &
      a * (x(1)*a + x(2)*b + x(3)*c) + &
      b * (x(4)*b + x(5)*c) + &
      x(6) * (c**2)
  end function
  pure real(8) function poly2x2(x, a, b)
    real(8), intent(in) :: x(3), a, b
    poly2x2 = a * (x(1)*a + x(2)*b) + x(3)*(b**2)
  end function

  pure real(8) function poly4x1(x, a, b, c, d)
    real(8), intent(in) :: x(4), a, b, c, d
    poly4x1 = x(1)*a + x(2)*b + x(3)*c + x(4)*d
  end function

  pure real(8) function poly4x3(x, a, b, c, d)
    real(8), intent(in) :: x(20), a, b, c, d
    poly4x3 = &
      a*(a*(x(1)*a + x(2)*b + x(3)*c + x(4)*d) +&
        b*(x(5)*b + x(6)*c + x(7)*d) +&
        c*(x(8)*c + x(9)*d) + x(10)*d**2) +&
      b*(b*(x(11)*b + x(12)*c + x(13)*d) +&
        c*(x(14)*c + x(15)*d) + x(16)*d**2) +&
      c*(c*(x(17)*c + x(18)*d) + x(19)*d**2) +&
      x(20)*d**3
  end function

  subroutine mb4func_right(n, theta, p0, q0, PQ, Y)
    integer, intent(in) :: n
    real(8), intent(in) :: theta, p0(n), q0(n), PQ(n,6)
    real(8), intent(out) :: Y(n, 6)
    integer :: i
    real(8) :: c1(60), c2(60), c3(60), c4(60)
    c1 = mb4a1*theta + mb4b1
    c2 = mb4a2*theta + mb4b2
    c3 = mb4a3*theta + mb4b3
    c4 = mb4a4*theta + mb4b4

    do concurrent(i=1:n)
    !do i=1,n
      Y(i,1) = &
        q0(i) * (&
          poly4x2(c1(1:10), q0(i), PQ(i,6), PQ(i,4), PQ(i,2)) + &
          poly4x2(c1(11:20), p0(i), PQ(i,5), PQ(i,3), PQ(i,1))) + &
        PQ(i,6)*(&
          poly3x2(c1(21:26), PQ(i,6), PQ(i,4), PQ(i,2)) + &
          poly4x2(c1(27:36), p0(i), PQ(i,5), PQ(i,3), PQ(i,1))) + &
        PQ(i,4)*(&
          poly2x2(c1(37:39), PQ(i,4), PQ(i,2)) + &
          poly4x2(c1(40:49), p0(i), PQ(i,5), PQ(i,3), PQ(i,1))) + &
        PQ(i,2) * (&
          c1(50) * (PQ(i,2)**2) + &
          poly4x2(c1(51:60), p0(i), PQ(i,5), PQ(i,3), PQ(i,1)))
      end do
      do concurrent(i=1:n)
      Y(i,2) = &
        q0(i)*(&
          q0(i) * poly4x1(c2(1:4), p0(i), PQ(i,5), PQ(i,3), PQ(i,1)) + &
          PQ(i,6) * poly4x1(c2(5:8), p0(i), PQ(i,5), PQ(i,3), PQ(i,1)) + &
          PQ(i,4) * poly4x1(c2(9:12), p0(i), PQ(i,5), PQ(i,3), PQ(i,1)) + &
          PQ(i,2) * poly4x1(c2(13:16), p0(i), PQ(i,5), PQ(i,3), PQ(i,1))) + &
        PQ(i,6)*(&
          PQ(i,6) * poly4x1(c2(17:20), p0(i), PQ(i,5), PQ(i,3), PQ(i,1)) + &
          PQ(i,4) * poly4x1(c2(21:24), p0(i), PQ(i,5), PQ(i,3), PQ(i,1)) + &
          PQ(i,2) * poly4x1(c2(25:28), p0(i), PQ(i,5), PQ(i,3), PQ(i,1))) + &
        PQ(i,4)*(&
          PQ(i,4) * poly4x1(c2(29:32), p0(i), PQ(i,5), PQ(i,3), PQ(i,1)) + &
          PQ(i,2) * poly4x1(c2(33:36), p0(i), PQ(i,5), PQ(i,3), PQ(i,1))) + &
        PQ(i,2)*(&
          PQ(i,2) * poly4x1(c2(37:40), p0(i), PQ(i,5), PQ(i,3), PQ(i,1))) + &
        p0(i) * poly4x2(c2(41:50), p0(i), PQ(i,5), PQ(i,3), PQ(i,1)) + &
        PQ(i,5) * poly3x2(c2(51:56), PQ(i,5), PQ(i,3), PQ(i,1)) + &
        PQ(i,3) * poly2x2(c2(57:59), PQ(i,3), PQ(i,1)) + &
        c2(60) * PQ(i,1) ** 3
      end do
      do concurrent(i=1:n)
      Y(i,3) = &
        q0(i) * (&
          poly4x2(c3(1:10), q0(i), PQ(i,6), PQ(i,4), PQ(i,2)) + &
          poly4x2(c3(11:20), p0(i), PQ(i,5), PQ(i,3), PQ(i,1))) + &
        PQ(i,6)*(&
          poly3x2(c3(21:26), PQ(i,6), PQ(i,4), PQ(i,2)) + &
          poly4x2(c3(27:36), p0(i), PQ(i,5), PQ(i,3), PQ(i,1))) + &
        PQ(i,4)*(&
          poly2x2(c3(37:39), PQ(i,4), PQ(i,2)) + &
          poly4x2(c3(40:49), p0(i), PQ(i,5), PQ(i,3), PQ(i,1))) + &
        PQ(i,2) * (&
          c3(50) * (PQ(i,2)**2) + &
          poly4x2(c3(51:60), p0(i), PQ(i,5), PQ(i,3), PQ(i,1)))
      end do
      do concurrent(i=1:n)
      Y(i,4) = &
        q0(i)*(&
          q0(i) * poly4x1(c4(1:4), p0(i), PQ(i,5), PQ(i,3), PQ(i,1)) + &
          PQ(i,6) * poly4x1(c4(5:8), p0(i), PQ(i,5), PQ(i,3), PQ(i,1)) + &
          PQ(i,4) * poly4x1(c4(9:12), p0(i), PQ(i,5), PQ(i,3), PQ(i,1)) + &
          PQ(i,2) * poly4x1(c4(13:16), p0(i), PQ(i,5), PQ(i,3), PQ(i,1))) + &
        PQ(i,6)*(&
          PQ(i,6) * poly4x1(c4(17:20), p0(i), PQ(i,5), PQ(i,3), PQ(i,1)) + &
          PQ(i,4) * poly4x1(c4(21:24), p0(i), PQ(i,5), PQ(i,3), PQ(i,1)) + &
          PQ(i,2) * poly4x1(c4(25:28), p0(i), PQ(i,5), PQ(i,3), PQ(i,1))) + &
        PQ(i,4)*(&
          PQ(i,4) * poly4x1(c4(29:32), p0(i), PQ(i,5), PQ(i,3), PQ(i,1)) + &
          PQ(i,2) * poly4x1(c4(33:36), p0(i), PQ(i,5), PQ(i,3), PQ(i,1))) + &
        PQ(i,2)*(&
          PQ(i,2) * poly4x1(c4(37:40), p0(i), PQ(i,5), PQ(i,3), PQ(i,1))) + &
        poly4x3(c4(41:60), p0(i), PQ(i,5), PQ(i,3), PQ(i,1))
      end do
      do concurrent(i=1:n)
      Y(i,5) = &
        q0(i) * (&
          poly4x2(mb4a5(1:10), q0(i), PQ(i,6), PQ(i,4), PQ(i,2)) +&
          poly4x2(mb4a5(11:20), p0(i), PQ(i,5), PQ(i,3), PQ(i,1))) + &
        PQ(i,6) * (&
          poly3x2(mb4a5(21:26), PQ(i,6), PQ(i,4), PQ(i,2)) + &
          poly4x2(mb4a5(27:36), p0(i), PQ(i,5), PQ(i,3), PQ(i,1))) + &
        PQ(i,4) * (&
          mb4a5(37) * PQ(i,4) ** 2 +&
          p0(i) * (mb4a5(38)*p0(i) + mb4a5(39)*PQ(i,5) + mb4a5(40)*PQ(i,3) + mb4a5(41)*PQ(i,1)) +&
          PQ(i,5) * (mb4a5(42)*PQ(i,5) + mb4a5(43)*PQ(i,3) + mb4a5(44)*PQ(i,1)) +&
          mb4a5(45) * PQ(i,3)**2) + &
        PQ(i,2) * (&
          mb4a5(46) * PQ(i,2)**2 + &
          p0(i) * (mb4a5(47)*p0(i) + mb4a5(48)*PQ(i,5) + mb4a5(49)*PQ(i,3) + mb4a5(50)*PQ(i,1)) +&
          PQ(i,5) * (mb4a5(51)*PQ(i,5) + mb4a5(52)*PQ(i,3) + mb4a5(53)*PQ(i,1)) +&
          mb4a5(54) * PQ(i,1)**2)
      end do
      do concurrent(i=1:n)
      Y(i,6) = &
        q0(i) * (&
          q0(i) * poly4x1(mb4a6(1:4), p0(i), PQ(i,5), PQ(i,3), PQ(i,1)) + &
          PQ(i,6) * poly4x1(mb4a6(5:8), p0(i), PQ(i,5), PQ(i,3), PQ(i,1)) + &
          PQ(i,4) * poly4x1(mb4a6(9:12), p0(i), PQ(i,5), PQ(i,3), PQ(i,1)) + &
          PQ(i,2) * poly4x1(mb4a6(13:16), p0(i), PQ(i,5), PQ(i,3), PQ(i,1))) +& 
        PQ(i,6) * (&
          PQ(i,6) * poly4x1(mb4a6(17:20), p0(i), PQ(i,5), PQ(i,3), PQ(i,1)) + &
          PQ(i,4) * poly4x1(mb4a6(21:24), p0(i), PQ(i,5), PQ(i,3), PQ(i,1)) + &
          PQ(i,2) * poly4x1(mb4a6(25:28), p0(i), PQ(i,5), PQ(i,3), PQ(i,1))) + &
        PQ(i,4) * (&
          PQ(i,4) * (mb4a6(29)*p0(i) + mb4a6(30)*PQ(i,5) + mb4a6(31)*PQ(i,3)) +&
          PQ(i,2) * (mb4a6(32)*p0(i) + mb4a6(33)*PQ(i,5))) +&
        PQ(i,2) ** 2 * (mb4a6(34)*p0(i) + mb4a6(35)*PQ(i,5) + mb4a6(36)*PQ(i,1)) +&
        p0(i) * poly4x2(mb4a6(37:46), p0(i), PQ(i,5), PQ(i,3), PQ(i,1)) +&
        PQ(i,5) * poly3x2(mb4a6(47:52), PQ(i,5), PQ(i,3), PQ(i,1))+&
        mb4a6(53) * PQ(i,3) ** 3 +&
        mb4a6(54) * PQ(i,1) ** 3
    end do
  end subroutine

  subroutine mb4func(n, A, eps, dt, theta, p0, q0, PQ, Phi, work)
    use sparse_blas
    integer, intent(in) :: n
    type(dcsr_t), intent(in) :: A
    real(8), intent(in) :: eps, dt, theta
    real(8), intent(in) :: p0(n), q0(n), PQ(n,6)
    real(8), intent(out) :: Phi(n,6), work(n,6)

    integer :: i

    call mb4func_left(n, A, theta, p0, q0, PQ, Phi, work)
    call mb4func_right(n, theta, p0, q0, PQ, work)

    do i=1,n
      Phi(i,1) = PQ(i,1) - p0(i) - (-Phi(i,1) - eps * work(i,1)) * (dt / 665280d0)
      Phi(i,2) = PQ(i,2) - q0(i) - ( Phi(i,2) + eps * work(i,2)) * (dt / 665280d0)
      Phi(i,3) = PQ(i,3) - p0(i) - ( Phi(i,3) + eps * work(i,3)) * (dt / 166320d0)
      Phi(i,4) = PQ(i,4) - q0(i) - (-Phi(i,4) - eps * work(i,4)) * (dt / 166320d0)
      Phi(i,5) = PQ(i,5) - p0(i) - ( Phi(i,5) + eps * work(i,5)) * (dt / 6720d0)
      Phi(i,6) = PQ(i,6) - q0(i) - (-Phi(i,6) - eps * work(i,6)) * (dt / 6720d0)
    end do
  end subroutine mb4func

  subroutine reshape_to_vector(nx, ny, U, P, Q)
   integer, intent(in) :: nx, ny
    complex(8), intent(in) :: U(nx, ny)
    real(8), intent(out) :: P(nx, ny), Q(nx, ny)

    P = real(U)
    Q = aimag(U)
  end subroutine reshape_to_vector

  subroutine gen_matE(theta, c1, c2, c3, quadtol, Emat, Tmat, Tinv, lambdas)
    ! quadtol is unused in this code.
    real(8), intent(in) :: theta, c1, c2, c3, quadtol
    real(8), intent(out) :: Emat(3, 3), Tmat(3, 3), Tinv(3, 3), lambdas(3)

    integer :: info, ipiv(3)
    real(8) :: Mmat(3, 3), a, a6, a36, lambdas_imag(3), work(12), &
        mat_work(3,3), dum(1)

    a = -300d0 * theta
    a6 = a * 6d0
    a36 = a * 36d0
    Mmat(1, 1) = a + 4d0
    Mmat(2, 1) = -a6 - 6d0
    Mmat(3, 1) = a6
    Mmat(2, 2) = a36 + 12d0
    Mmat(3, 2) = -a36
    Mmat(3, 3) = a36
    Mmat(1, 2) = Mmat(2, 1)
    Mmat(1, 3) = Mmat(3, 1)
    Mmat(2, 3) = Mmat(3, 2)
    call integrate_gen_mate_integrand(c1, c2, c3, Mmat, Emat)

    mat_work = Emat
    call dgeev('N', 'V', 3, mat_work, 3, lambdas, lambdas_imag, &
         dum, 1, Tmat, 3, work, 12, info)

    mat_work = Tmat
    call set_eye(3, Tinv)
    call dgesv(3, 3, mat_work, 3, ipiv, Tinv, 3, info)

    print *, 'lambdas', lambdas
    print *, 'lambdas_imag (must be zero)', lambdas_imag
  end subroutine gen_matE

  subroutine set_jacobian(n, eps, p0, q0, A, Jf)
    ! MATLAB: Jf = [2*eps*diag(p0.*q0), A + eps * diag(p0.^2+3*q0.^2);
    !               -A - eps * diag(p0.^2+3*q0.^2), 2*eps*diag(p0.*q0)];
    integer, intent(in) :: n
    real(8), intent(in) :: eps, p0(n), q0(n), A(n, n)
    real(8), intent(out) :: Jf(n * 2, n * 2)

    integer :: i
    real(8) :: jfd1(n), jfd2(n)


    Jf(:, :) = 0d0
    Jf(1 : n, n + 1 : n * 2) = A(1 : n, 1 : n)
    Jf(n + 1 : n * 2, 1 : n) = -A(1 : n, 1 : n)

    jfd1 = 2d0 * eps * (p0 * q0)
    jfd2 = eps * ((p0**2) + 3d0 * (q0**2))
    do i = 1, n
      Jf(i, i) = jfd1(i)
      Jf(n + i, n + i) = jfd1(i)
      Jf(i, n + i) = Jf(i, n + i) + jfd2(i)
      Jf(n + i, i) = Jf(n + i, i) - jfd2(i)
    end do
  end subroutine set_jacobian

  subroutine set_jcr(n, A, Sym)
    use dcsr_builder
    type(dcsr_t), intent(in) :: A
    type(dcsr_t), intent(out) :: Sym
    integer, intent(in) :: n

    integer :: i, sz
    sz = A%r(n+1)-1

    call initialize(Sym, n*2, n*2, n*2 + sz*2)
    Sym%v(1:n*2+sz*2) = 1d0 ! dummy values

    Sym%r(1) = 1
    do i=1,n
      ! [diag, A]
      Sym%r(i+1) = A%r(i+1) + i
      Sym%c(Sym%r(i)) = i
      Sym%c(Sym%r(i)+1:Sym%r(i+1)-1) = A%c(A%r(i):A%r(i+1)-1) + n
      !print *, A%r(i+1), Sym%r(i+1), 'a', A%c(A%r(i):A%r(i+1)-1), 's', Sym%c(Sym%r(i):Sym%r(i+1)-1)
    end do

    do i=1,n
      ! [A, diag]
      Sym%r(n+i+1) = A%r(i+1) + i + sz + n
      Sym%c(Sym%r(n+i):Sym%r(n+i+1)-2) = A%c(A%r(i):A%r(i+1)-1)
      Sym%c(Sym%r(n+i+1)-1) = n + i
      !print *, A%r(i+1), Sym%r(n+i+1), 'a', A%c(A%r(i):A%r(i+1)-1), 's', Sym%c(Sym%r(n+i):Sym%r(n+i+1)-1)
    end do

  end subroutine

  subroutine set_threej(n, eps, p0, q0, lambda, A, M1, M2, M3)
    integer, intent(in) :: n
    real(8), intent(in) :: eps, p0(n), q0(n), lambda(3)
    type(dcsr_t), intent(in) :: A
    type(dcsr_t), intent(inout) :: M1, M2, M3

    integer :: i, j, k, p
    real(8) :: d(n), e(n), x, f(n)
    ! M1 = I - lambda(1) * [2*eps*diag(p0.*q0), A+eps*diag(p0.^2+3*q0.^2); -A-eps*diag(p0.^2+3*q0.^2), 2*eps*diag(p0.*q0)]

    d = 2*eps*p0 * q0
    e = eps*((p0**2) + 3*(q0**2))
    f = -eps*(3*(p0**2)+(q0**2))

    do concurrent(i=1:n)
      j = M1%r(i)
      M1%v(j) = 1d0 - lambda(1) * d(i)
      M2%v(j) = 1d0 - lambda(2) * d(i)
      M3%v(j) = 1d0 - lambda(3) * d(i)
      do k=A%r(i),A%r(i+1)-1
        if(i .eq. A%c(k)) then
          x = A%v(k)+e(i)
        else 
          x = A%v(k)
        end if
        p = k - A%r(i)
        M1%v(j+p+1) = -lambda(1) * x
        M2%v(j+p+1) = -lambda(2) * x
        M3%v(j+p+1) = -lambda(3) * x
      end do
    end do

    do concurrent(i=1:n)
      j = M1%r(n+i)
      do k=A%r(i),A%r(i+1)-1
        if(i .eq. A%c(k)) then
          x = -A%v(k)+f(i)
        else 
          x = -A%v(k)
        end if
        p = k - A%r(i)
        M1%v(j+p) = -lambda(1) * x
        M2%v(j+p) = -lambda(2) * x
        M3%v(j+p) = -lambda(3) * x
      end do
      j = M1%r(n+i+1) - 1
      M1%v(j) = 1d0 + lambda(1) * d(i)
      M2%v(j) = 1d0 + lambda(2) * d(i)
      M3%v(j) = 1d0 + lambda(3) * d(i)
    end do
  end subroutine

  subroutine set_eye(n, X)
    ! set identity matrix with size n.
    integer, intent(in) :: n
    real(8), intent(out) :: X(n, n)

    integer :: i

    X(:, :) = 0d0
    do i = 1, n
      X(i, i) = 1d0
    end do
  end subroutine set_eye

  subroutine reshape_to_matrix(nx, ny, P, Q, U)
    ! MATLAB: U = reshape(P + sqrt(-1)*Q,[nx,ny]);
    integer, intent(in) :: nx, ny
    real(8), intent(in) :: P(nx, ny), Q(nx, ny)
    complex(8), intent(out) :: U(nx, ny)

    U = cmplx(P, Q, 8)
  end subroutine reshape_to_matrix

  subroutine kronTmv(n, T, X, Y)
    integer, intent(in) :: n
    real(8), intent(in) :: T(3, 3), X(3*n)
    real(8), intent(out) :: Y(3*n)
    integer :: i
    do concurrent (i=1:n)
      Y(i    ) = T(1,1) * X(i) + T(1,2) * X(i+n) + T(1,3) * X(i+2*n)
      Y(i+n  ) = T(2,1) * X(i) + T(2,2) * X(i+n) + T(2,3) * X(i+2*n)
      Y(i+2*n) = T(3,1) * X(i) + T(3,2) * X(i+n) + T(3,3) * X(i+2*n)
    end do
  end subroutine

  subroutine print_result(nx, ny, iter, sniter, t, P, Q, norm, energy, iunit, dif_energy, nl_energy, v_energy)
    integer, intent(in) :: nx, ny, iter, sniter, iunit
    real(8), intent(in) :: t, norm, energy, dif_energy, nl_energy, v_energy
    real(8), intent(in) :: P(nx, ny), Q(nx, ny)

    integer :: ix, iy

    write(iunit, *) '##step'
    write(iunit, *) 'nx', nx
    write(iunit, *) 'ny', ny
    write(iunit, *) 'iter', iter
    write(iunit, *) 'simple_newton_iter', sniter
    write(iunit, '(A, E24.16e3)') 't', t
    write(iunit, '(A, E24.16e3)') 'norm', norm
    write(iunit, '(A, E24.16e3)') 'all_energy', energy
    write(iunit, '(A, E24.16e3)') 'dif_energy', dif_energy
    write(iunit, '(A, E24.16e3)') 'nl_energy', nl_energy
    write(iunit, '(A, E24.16e3)') 'v_energy', v_energy
    write(iunit, '(A)') '# ix iy Re Im'
    do iy = 1, ny
      do ix = 1, nx
        write(iunit, '(2I6, 2E25.16e3)') ix, iy, P(ix, iy), Q(ix, iy)
      end do
       write (iunit, '(A)') ' '
    end do
  end subroutine print_result

  real(8) function time_duration(cstart, cout)
    integer(8), intent(in) :: cstart
    integer(8), intent(out), optional :: cout
    integer(8) :: cur, crate, cmax
    call system_clock(cur, crate, cmax)
    if(present(cout)) cout = cur
    if(cur < cstart) then 
      time_duration = real((cmax-cstart) + cur + 1, 8) / crate
    else
      time_duration = real(cur-cstart, 8) / crate
    end if
  end function

#ifndef USEMKL
  subroutine mkl_set_num_threads(nt)
    integer, intent(in) :: nt
    integer :: idum
    idum = nt
  end subroutine
#endif


  subroutine mb4(dx, dy, n, nx, ny, FDx, FDy, A, eps, TolX, quadtol, dt, t_end, Uinit, norms, energies, pratios, iunit, V, dif_energies, nl_energies, v_energies)
    use dcsr_builder
#ifdef __GFORTRAN__
    use pardiso_solver
    !use umfpack_solver
#else
    use mkl_pardiso_solver
    !use mkl_dss_solver
#endif
    ! n == nx * ny
    integer, intent(in) :: nx, ny, iunit
    real(8), intent(in) :: dx, dy, eps, TolX, quadtol, dt, t_end
    type(dcsr_t), intent(in)  :: FDx, FDy, A, V
    complex(8), intent(in) :: Uinit(nx, ny)
    real(8), intent(out) :: norms(:), energies(:), pratios(:)
    real(8), intent(inout) :: dif_energies(:), nl_energies(:), v_energies(:)

    integer :: i, n, iter, simple_newton_iter, rank, ierr
    real(8), parameter :: theta = 19d0 / 8d0, c1 = 1d0 / 3d0, c2 = 2d0 / 3d0, c3 = 1d0
    real(8) :: Emat(3, 3), Tmat(3, 3), Tinv(3, 3), lambdas(3), t, ctol
    real(8), allocatable :: p0(:), q0(:), PQ(:), modmb4(:), rho(:), phi(:)
    type(dcsr_t) :: M(3)
    type(solver_t) :: slv(3)

    integer(8) :: cm, cstart, citer, csymbolic, cnumeric, csolve, cnewton, cnrmene, cio, &
      csetmat, cmb4, ckronmv, cag
    real(8) :: tm, time_all, time_iteration, time_symbolic, time_numeric, time_solve, time_newton, &
      time_nrmene, time_io, time_setmat, time_mb4, time_kronmv, time_ag

    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)


    call system_clock(cstart)
    tm = 0; time_all = 0; time_iteration = 0; time_symbolic = 0; time_numeric = 0; time_solve = 0; 
    time_newton = 0; time_nrmene = 0; time_io = 0; time_setmat = 0; time_mb4 = 0; time_newton = 0;
    time_ag = 0;


    allocate(p0(n), q0(n))
    allocate(PQ(n*6), modmb4(n*6), rho(n*6), phi(n*2))

    do i=1,3
      call solver_initialize(slv(i))
    end do

    call gen_matE(theta, c1, c2, c3, quadtol, Emat, Tmat, Tinv, lambdas)

    call set_jcr(n, A, M(1))
    call share_cr(M(1), M(2))
    call share_cr(M(1), M(3))

    iter = 1
    simple_newton_iter = 0
    t = 0d0
    call reshape_to_vector(nx, ny, Uinit, p0, q0)

    call system_clock(citer)
    do
      call system_clock(cnrmene)
      norms(iter) = get_normv(nx, ny, p0, q0)
      pratios(iter) = get_pratio(nx, ny, p0, q0)/(norms(iter)**2)
      energies(iter) = get_energye(nx, ny, eps, FDx, FDy, p0, q0, rho, modmb4, V, dif_energies(iter), nl_energies(iter), v_energies(iter))
      time_nrmene = time_nrmene + time_duration(cnrmene, cio)

      if (rank .eq. 0) then
        !print *, 'iteration ', iter, ', time ', t, ', (norm, energy) = (', norms(iter), energies(iter), ')'
        print *, 'iteration ', iter, ', time ', t, ', (norm, energy, pratio) = (', norms(iter), energies(iter), pratios(iter), ')'
        call print_result(nx, ny, iter, simple_newton_iter, t, p0, q0, norms(iter), energies(iter), iunit, dif_energies(iter), nl_energies(iter), v_energies(iter))
      end if
      time_io = time_io + time_duration(cio)

      if (t >= t_end) then
        exit
      end if
      ! Initial value for iteration.
      PQ(   1:  n) = p0
      PQ(  n+1:2*n) = q0
      PQ(2*n+1:3*n) = p0
      PQ(3*n+1:4*n) = q0
      PQ(4*n+1:5*n) = p0
      PQ(5*n+1:6*n) = q0

      call system_clock(csetmat)
      call set_threej(n, eps, p0, q0, lambdas(1:3)*dt, A, M(1), M(2), M(3))
      time_setmat = time_setmat + time_duration(csetmat, csymbolic)

      if(iter .eq. 1) then
        do i=1,3
          call solver_symbolicf(slv(i), M(i), 1, 0)
        end do
      end if
      time_symbolic = time_symbolic + time_duration(csymbolic, cnumeric)

      do i=1,3
        call solver_numericf(slv(i), M(i))
      end do

      time_numeric = time_numeric + time_duration(cnumeric, cnewton)
      if(use_zero_index) call f2cindex(M(1))

      simple_newton_iter = 0
      ctol = 1d0
      do while (ctol > TolX)
        ! Simplified Newton method.
        simple_newton_iter = simple_newton_iter + 1
        call system_clock(cmb4)
        call mb4func(n, A, eps, dt, theta, p0, q0, PQ, rho, modmb4)
        time_mb4 = time_mb4 + time_duration(cmb4, ckronmv)
        

        ! modmb4 = matmul(kron(Tinf, eye(2*n)), rho)
        call kronTmv(2*n, Tinv, rho, modmb4)
        time_kronmv = time_kronmv + time_duration(ckronmv, csolve)

        !do i=1,3
          call solver_solve(slv(rank+1), M(rank+1), modmb4((n*2)*(rank)+1:), phi)
        !end do
        time_solve = time_solve + time_duration(csolve, cag)

        call MPI_ALLGATHER(phi, n*2, MPI_DOUBLE_PRECISION, rho, n*2, MPI_DOUBLE_PRECISION, MPI_COMM_WORLD, ierr)

        time_ag = time_ag + time_duration(cag, ckronmv)

        call kronTmv(2*n, Tmat, rho, modmb4)
        time_kronmv = time_kronmv + time_duration(ckronmv)
        PQ = PQ - modmb4

        if (simple_newton_iter == 100) then
          print *, '[Warn] simple Newton method max iter ', simple_newton_iter, ' reached'
          exit
        end if
        ctol = maxval(abs(modmb4))
      end do
      time_newton = time_newton + time_duration(cnewton)

      do i=1,3
        call solver_finalize_factor(slv(i))
      end do
      if(use_zero_index) call c2findex(M(1))

      p0 = PQ(4*n+1:5*n)
      q0 = PQ(5*n+1:6*n)

      iter = iter + 1
      t = dt * (iter - 1)
    end do
    time_iteration = time_duration(citer)

    do i=1,3
      call solver_finalize(slv(i))
      call finalize(M(i))
    end do
    time_all = time_duration(cstart)

    if(rank .eq. 0) then
      write (iunit, *) 'time_all', time_all
      write (iunit, *) 'time_iteration', time_iteration
      write (iunit, *) 'time_nrmene', time_nrmene
      write (iunit, *) 'time_io', time_io
      write (iunit, *) 'time_symbolic', time_symbolic
      write (iunit, *) 'time_setmat', time_setmat
      write (iunit, *) 'time_numeric', time_numeric
      write (iunit, *) 'time_newton', time_newton
      write (iunit, *) 'time_mb4', time_mb4
      write (iunit, *) 'time_kronmv', time_kronmv
      write (iunit, *) 'time_solve', time_solve
      write (iunit, *) 'time_ag', time_ag
    end if

  end subroutine mb4

  real(8) function get_normv(nx, ny, P, Q)
    integer, intent(in) :: nx, ny
    real(8), intent(in) :: P(nx*ny), Q(nx*ny)
    get_normv = sum(P**2 + Q**2) / real(nx * ny, 8)
  end function

  real(8) function get_pratio(nx, ny, P, Q)
    integer, intent(in) :: nx, ny
    real(8), intent(in) :: P(nx*ny), Q(nx*ny)
    get_pratio = sum((P**2 + Q**2)**2) / real(nx * ny, 8)**2
  end function

  real(8) function get_energye(nx, ny, eps, FDx, FDy, P, Q, work, work2, V, dif_energy, nl_energy, v_energy)
    use sparse_blas
    integer, intent(in) :: nx, ny
    real(8), intent(in) :: eps
    type(dcsr_t), intent(in) :: FDx, FDy, V
    real(8), intent(in) :: P(nx, ny), Q(nx, ny)
    real(8), intent(out) :: work(nx, ny), work2(ny, nx)
    real(8), intent(out) :: dif_energy, nl_energy, v_energy

    real(8) :: Uabs4, FDxUabs2, UFDyabs2, Venergy

    Uabs4 = eps / 2d0 * sum((P**2+Q**2)**2)

    Venergy = sum((P**2+Q**2)*reshape(V%v, (/nx, ny/)))

    call spmm_test(FDx, P, nx, ny, work, nx)
    FDxUabs2 = sum(work**2)
    call spmm_test(FDx, Q, nx, ny, work, nx)
    FDxUabs2 = FDxUabs2 + sum(work**2)

    work = transpose(P)
    call spmm_T(FDy, work, ny, nx, work2, ny)
    UFDyabs2 = sum(work2**2)
    work = transpose(Q)
    call spmm_T(FDy, work, ny, nx, work2, ny)
    UFDyabs2 = UFDyabs2 + sum(work2**2)

    !source
    !get_energye = (FDxUabs2 + UFDyabs2 + Uabs4) / real(nx * ny, 8)
    !before
    !get_energye = (FDxUabs2 + UFDyabs2 + Uabs4 - Venergy) / real(nx * ny, 8)
    !correct
    !get_energye = (FDxUabs2 + UFDyabs2 + Uabs4 + Venergy) / real(nx * ny, 8)
    dif_energy = (FDxUabs2 + UFDyabs2) / real(nx * ny, 8)
    nl_energy = (Uabs4) / real(nx * ny, 8)
    v_energy = (Venergy) / real(nx * ny, 8)
    get_energye = (FDxUabs2 + UFDyabs2 + Uabs4 + Venergy) / real(nx * ny, 8)
    !print *, 'Venergy = ', Venergy / real(nx*ny,8)
  end function
end module mb4_m

program main
  use mb4_m
  use dcsr_builder
  implicit none
  integer :: nx, ny, nt, n, iunit, rank, ierr
  real(8) :: lx, ly, kPi, dx, dy, dt, t_end, eps, quadtol, TolX, V0
  real(8), allocatable :: energies(:), norms(:), pratios(:)
  real(8), allocatable :: dif_energies(:), nl_energies(:), v_energies(:)
  complex(8), allocatable :: U(:, :)
  type(dcsr_t) :: FDx, FDy, D2x, D2y, A, A0, V
  character(len=32) :: buf

  call MPI_INIT(ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)

  kPi = 3.141592653589793d0
  lx = 2d0 * kPi  ! Spatial region size for x-axis.
  ly = 2d0 * kPi  ! Spatial region size for y-axis.
  nx = 100  ! Mesh size for x-axis.
  ny = 100  ! Mesh size for y-axis.

  if(command_argument_count() .ge. 1) then
    call get_command_argument(1, buf)
    read (buf,*)  nx
    ny = nx
  end if

  dx = lx / nx
  dy = ly / ny

  t_end = 20.0d0  ! Total simulation time. 4.0
  if(command_argument_count() .ge. 2) then
    call get_command_argument(2, buf)
    read (buf,*)  t_end
  end if

  dt = 0.01d0  ! Simulation time step width.
  nt = ceiling(t_end / dt)
  eps = -0.05  ! Factor for the nonlinear term |U|^2 .* U.
  V0 = -50d0
  quadtol = 1d-12  ! Unused in this code.
  TolX = 1d-13  ! Convergence criterion for the simplified Newton method.

  n = nx * ny  ! Total number of mesh points.
  allocate(energies(nt+1), norms(nt+1), pratios(nt+1), U(nx, ny))
  allocate(dif_energies(nt+1), nl_energies(nt+1), v_energies(nt+1))

  call set_forward_difference_matrix(nx, dx, FDx)
  call set_forward_difference_matrix(ny, dy, FDy)
  call set_second_difference_matrix(nx, dx, D2x)
  call set_second_difference_matrix(ny, dy, D2y)
  call set_difference_xy_matrix(nx, ny, D2x, D2y, A0)
  call add_potential_matrix(nx, ny, A0, V, A, V0)
  call finalize(D2y)
  call finalize(D2x)

  call set_initial(nx, ny, dx, dy, U)
  iunit = 7
  if(command_argument_count() .ge. 3) then
    call get_command_argument(3, buf)
    if(rank .eq. 0) then
      open(iunit, file=trim(buf))
    end if
  else
    open(iunit, file='result_mb4_sp_fortran.dat')
  end if
  call mb4(dx, dy, n, nx, ny, FDx, FDy, A, eps, TolX, quadtol, dt, t_end, U, norms, energies, pratios, iunit, V, dif_energies, nl_energies, v_energies)
  close(iunit)

  call finalize(A)
  call finalize(D2y)
  call finalize(D2x)

  call MPI_FINALIZE(ierr)
end program main
