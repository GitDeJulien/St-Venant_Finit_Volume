!> Numerical resolution for Saint-Venant (Shallow water) equations
!> Berton & Foucher scheme (28 fevirer 2012) (Topology terme rightfully taken into acount)

!> Julien Tenaud 2024

program StVenant

    use init_mod
    implicit none

    character(len=256)                  :: filepath
    real(pr), dimension(:,:), allocatable :: Uapp, Uexact
    type(DataType)                      :: df
    type(StructCelleType), dimension(:), allocatable :: celles


    filepath = 'input/data.toml'

    ! Display TOML data file
    call display_toml_file(filepath)

    ! Read and stock all data from TOML data file
    call config_data(df, filepath)

    ! Build mesh
    call init_mesh(df, celles)

    ! Download the topology in 'output/topo/*'
    call save_topography(df, celles, 0.0_pr)

    allocate(Uapp(df%n_celle, 2))
    allocate(Uexact(df%n_celle, 2))

    call init_sol(df, celles, Uapp(:,1), Uapp(:,2))


    ! Deallocate all tensors
    deallocate(celles)
    deallocate(Uapp, Uexact)
end program