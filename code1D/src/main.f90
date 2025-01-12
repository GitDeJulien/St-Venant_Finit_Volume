!! Berthon & Foucher 1D Finit Volume Scheme for 
!! St-Venant Equation with source terme.
!! ref: 
!! "Efficient well–balanced hydrostatic upwind schemes for shallow–water equations"

!> Numerical resolution for Saint-Venant (Shallow water) equations
!> Berton & Foucher scheme (28 fevirer 2012) (Topology terme rightfully taken into acount)

!> Julien Tenaud 2024

program StVenant

    use init_mod
    use exact_sol_mod
    use time_scheme_mod
    use save_out_mod
    implicit none

    character(len=125)                      :: filepath, ch_Nx, ch_Riemann
    integer                                 :: ios, k
    real(pr), dimension(:), allocatable     :: X
    real(pr), dimension(:,:), allocatable   :: Un, Uexact
    real(pr), dimension(:,:), allocatable   :: Wn
    real(pr), dimension(:), allocatable     :: Topo
    type(DataType)                          :: df

    !time
    integer  :: t_iter
    real(pr) :: tn

    filepath = 'data/data.toml'

    ! Display TOML data file
    call display_toml_file(filepath)

    ! Read and stock all data from TOML data file
    call config_data(df, filepath)

    ! Build mesh
    allocate(X(df%Nx))
    call init_mesh(df, X)

    ! Vectors allocation
    allocate(Un(df%Nx, 2))
    allocate(Wn(df%Nx, 2))
    allocate(Uexact(df%Nx, 2))
    allocate(Topo(df%Nx))

    call init_sol(df, X, Un)
    call exact_sol_fct(df, X, 0.0_pr, Uexact(:,1), Uexact(:,2))
    do k=1,df%Nx
        Topo(k) = topography1D(df, X(k), 0.d0)
    enddo

    Wn = sol_rewrite(Un, Topo)

    ! Download datas
    call save_topography(df, X, 0, Topo)
    call save_exact_sol(df, X, 0, Uexact, Topo)
    call save_approx_sol(df, X, 0, Un, Wn)

    write(ch_Nx, '(I4)') df%Nx
    write(ch_Riemann, '(I10)') df%Riemann_solv
    open(unit=20, file="output/error/err.Nx."//trim(adjustl(ch_Nx))//"&
    &.solv."//trim(adjustl(ch_Riemann))//".dat", status='REPLACE', action='WRITE', iostat=ios)
    if (ios /= 0) then
        print *, 'Error opening file: ', " output/error/err.dat"
        stop
    end if
    !call save_error(df, Uexact, Un, 0, 20)

    t_iter = 0
    df%dt  = time_step(df, Wn)
    tn = df%t0 + df%dt

    ! -- Time loop -- !
    do t_iter=1,df%niter

        ! One more time step
        call advance(df, tn, X, Un, Wn, Topo)

        ! Compute exact sol
        call exact_sol_fct(df, X, tn, Uexact(:,1), Uexact(:,2))

        print*, "tn=", tn

        ! Save datas
        call save_approx_sol(df, X, t_iter, Un, Wn)
        call save_exact_sol(df, X, t_iter, Uexact, Topo)
        call save_topography(df, X, t_iter, Topo)
        call save_error(df, Uexact, Un, t_iter, 20)

        !if (tn > df%tfinal) exit

    enddo
    ! -- End Time loop -- !
    print*, "Final time: ", tn


    ! Deallocate all tensors
    deallocate(X)
    deallocate(Un, Wn, Uexact)
    deallocate(Topo)

    print*, " "
    print*, "Programm terminated correclty"
    print*, "--END--"
end program
