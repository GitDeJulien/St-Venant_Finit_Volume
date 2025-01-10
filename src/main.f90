!> Numerical resolution for Saint-Venant (Shallow water) equations
!> Berton & Foucher scheme (28 fevirer 2012) (Topology terme rightfully taken into acount)

!> Julien Tenaud 2024

program StVenant

    use init_mod
    use exact_sol_mod
    use time_scheme_mod
    use save_out_mod
    implicit none

    character(len=256)                               :: filepath
    real(pr), dimension(:,:), allocatable            :: Un, Unp1, Uexact
    real(pr), dimension(:,:), allocatable            :: Wn
    real(pr), dimension(:), allocatable              :: Topo
    type(DataType)                                   :: df
    type(StructCelleType), dimension(:), allocatable :: celles

    !time
    integer  :: t_iter
    real(pr) :: tn

    filepath = 'input/data.toml'

    ! Display TOML data file
    call display_toml_file(filepath)

    ! Read and stock all data from TOML data file
    call config_data(df, filepath)

    ! Build mesh
    call init_mesh(df, celles)

    

    allocate(Un(df%n_celle, 2))
    allocate(Wn(df%n_celle, 2))
    allocate(Unp1(df%n_celle, 2))
    allocate(Uexact(df%n_celle, 2))
    allocate(Topo(df%n_celle))

    call init_sol(df, celles, Un(:,1), Un(:,2))
    call exact_sol_fct(df, celles, 0.0_pr, Uexact(:,1), Uexact(:,2))
    Topo = topo_fct(df, celles, 0.0_pr)

    Unp1 = Un
    Wn = sol_rewrite(df, Un, Topo)

    ! Download datas
    call save_topography(df, celles, 0, Topo)
    call save_exact_sol(df, celles, 0, Uexact, Topo)
    call save_approx_sol(df, celles, 0, Un, Wn)

    open(unit=20, file="output/error/err.dat", status='REPLACE', action='WRITE')
    call save_error(df, Uexact, Un, 0, 20)

    df%dt = time_step(df, Wn)
    tn = df%t0 + df%dt

    ! -- Time loop -- !
    do t_iter=1,df%niter

        ! One more time step
        Unp1 = advance(df, Un, Wn)

        ! Topography
        Topo = topo_fct(df, celles, tn)

        ! Compute exact sol
        call exact_sol_fct(df, celles, tn, Uexact(:,1), Uexact(:,2))

        ! Update solution and time step
        Un = Unp1
        Wn = sol_rewrite(df, Un, Topo)
        df%dt = time_step(df, Un)
        tn = tn + df%dt

        print*, "invariant: ", SUM(Un(:,2)/Un(:,1) - 2*SQRT(grav*Un(:,1))), SUM(Un(:,2)/Un(:,1) + 2*SQRT(grav*Un(:,1)))

        ! Save datas
        call save_approx_sol(df, celles, t_iter, Un, Wn)
        call save_exact_sol(df, celles, t_iter, Uexact, Topo)
        call save_topography(df, celles, t_iter, Topo)
        call save_error(df, Uexact, Un, t_iter, 20)

    enddo
    ! -- End Time loop -- !


    ! Deallocate all tensors
    deallocate(celles)
    deallocate(Un, Uexact)
    deallocate(Topo)

    print*, "Programm terminated correclty"
    print*, "--END--"
end program