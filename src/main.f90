!> Numerical resolution for Saint-Venant (Shallow water) equations
!> Berton & Foucher scheme (28 fevirer 2012) (Topology terme rightfully taken into acount)

!> Julien Tenaud 2024

program StVenant

    use init_mod
    use exact_sol_mod
    use time_scheme_mod
    implicit none

    character(len=256)                               :: filepath
    real(pr), dimension(:,:), allocatable            :: Un, Unp1, Uexact
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

    ! Download the topology in 'output/topo/*'
    call save_topography(df, celles, 0.0_pr)

    allocate(Un(df%n_celle, 2))
    allocate(Unp1(df%n_celle, 2))
    allocate(Uexact(df%n_celle, 2))

    call init_sol(df, celles, Un(:,1), Un(:,2))

    call exact_sol_fct(df, celles, 0.0_pr, Uexact(:,1), Uexact(:,2))

    Unp1 = Un
    df%dt = time_step(df, Un)
    tn = df%t0 + df%dt

    ! -- Time loop -- !
    do t_iter=1,df%niter


        !Compute exact sol
        call exact_sol_fct(df, celles, tn, Uexact(:,1), Uexact(:,2))

        !Update solution and time step
        !Un = Unp1
        !df%dt = time_step(df, Un)
        tn = tn + df%dt

    enddo
    ! -- End Time loop -- !



    ! Deallocate all tensors
    deallocate(celles)
    deallocate(Un, Uexact)
end program