!> Numerical resolution for Saint-Venant (Shallow water) equations
!> Berton & Foucher scheme (28 fevirer 2012) (Topology terme rightfully taken into acount)

!> Julien Tenaud 2024

program StVenant

    use init_mod
    implicit none

    character(len=256)                  :: filepath
    real(pr), dimension(:), allocatable :: Hsol, Qsol
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

    allocate(Hsol(df%n_celle))
    allocate(Qsol(df%n_celle))

    call init_sol(df, celles, Hsol, Qsol)

    ! Deallocate all tensors
    deallocate(celles)
    deallocate(Hsol, Qsol)
end program