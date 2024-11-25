!> Numerical resolution for Saint-Venant (Shallow water) equations
!> Berton & Foucher scheme (28 fevirer 2012) (Topology terme rightfully taken into acount)

!> Julien Tenaud 2024


program StVenant

    use precision
    use data_reader
    use topography_mod
    implicit none

    character(len=256) :: filepath
    type(DataType)     :: df

    filepath = 'input/data.toml'

    call display_toml_file(filepath)
    call config_data(df, filepath)

    !>TODO MESH MODULE

    !call dl_topography(df%Topography_key, x_coord, tn, topo_filename)
    

end program