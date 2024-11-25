module data_reader

    use precision
    use toml_parser
    implicit none

    public :: config_data

    type, public :: DataType

        ![VERSION]
        character(len=5) :: version

        ![USEFULL PATH]
        character(len=256) :: input_path
        character(len=256) :: test_path
        character(len=256) :: output_path

        ![DIMENSION] (1 or 2)
        integer :: Dimension

        ![TEST CASE] (0: User define case, 1: case1, 2: case2)
        integer :: Test_case_key

        ![MESH TYPE] (0: User define mesh, 1: Strctured statical mesh, 2: Unstructered statical mesh)
        integer :: Mesh_key

        ![STRUCTURED STATICAL MESHGRID]
        real(pr) :: Lx
        real(pr) :: Ly
        integer :: Nx
        integer :: Ny
        real(pr) :: dx
        real(pr) :: dy

        ![TOPOLOGY FUNCTION] (0: User define function, 1: Forward and Backward Step, 2: Gaussian function)
        integer :: Topography_key

        ![INITIAL WATER SURFACE] (0: User define function, 1:Dame configuration, 2: Unique wave)
        integer :: Initial_key

        ![RIEMANN SCHEME] (1: HLL, 2: HLLC, 3: VFRoe)
        integer :: R_Scheme_key

        ![FINIT VOLUME SCHEME] (1: Berthon-Foucher)
        integer :: FV_Scheme_key

    end type DataType

contains

    subroutine config_data(data, filename)
        character(len=*), intent(in) :: filename
        type(DataType), intent(inout) :: data

        call parse_toml(filename, "version", data%version)
        call parse_toml(filename, "input_path", data%input_path)
        call parse_toml(filename, "test_path", data%test_path)
        call parse_toml(filename, "output_path", data%output_path)
        call parse_toml(filename, "Dimension", data%Dimension)
        call parse_toml(filename, "Test_case_key", data%Test_case_key)
        call parse_toml(filename, "Mesh_key", data%Mesh_key)
        call parse_toml(filename, "Lx", data%Lx)
        call parse_toml(filename, "Ly", data%Ly)
        call parse_toml(filename, "Nx", data%Nx)
        call parse_toml(filename, "Ny", data%Ny)
        call parse_toml(filename, "Topography_key", data%Topography_key)
        call parse_toml(filename, "Initial_key", data%Initial_key)
        call parse_toml(filename, "R_Scheme_key", data%R_Scheme_key)
        call parse_toml(filename, "FV_Scheme_key", data%FV_Scheme_key)

        if (data%mesh_key == 1) then
            data%dx = data%Lx / (data%Nx + 1)
            data%dy = data%Ly / (data%Ny + 1)
        end if

    end subroutine config_data
    
end module data_reader