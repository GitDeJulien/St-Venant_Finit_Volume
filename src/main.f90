!> Numerical resolution for Saint-Venant (Shallow water) equations
!> Berton & Foucher scheme (28 fevirer 2012) (Topology terme rightfully taken into acount)

!> Julien Tenaud 2024


program StVenant

    use precision
    use data_reader
    use topography_mod
    use structured_mesh_mod
    implicit none

    character(len=256) :: filepath, topo_filepath
    !integer :: i
    type(DataType)     :: df
    type(StructCelleType), dimension(:), allocatable :: celles

    filepath = 'input/data.toml'
    topo_filepath = 'output/topo/topology.dat'

    ! Display TOML data file
    call display_toml_file(filepath)

    ! Read and stock all data from TOML data file
    call config_data(df, filepath)

    ! Build mesh
    call init_mesh(df, celles)

    ! Download the topology in 'output/topo/*'
    call dl_topography(df, celles, 0.0_pr)

    ! Deallocate all tensors
    deallocate(celles)

end program