module data_reader

    use precision
    use toml_parser
    implicit none

    public :: config_data

    type, public :: DataType

        ![VERSION]
        character(len=10) :: version

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
        real(pr) :: x_min
        real(pr) :: y_min
        real(pr) :: Lx
        real(pr) :: Ly
        integer :: Nx
        integer :: Ny
        integer :: n_celle
        real(pr) :: dx
        real(pr) :: dy

        ![RIEMANN SCHEME] (1: HLL, 2: HLLC, 3: VFRoe)
        integer :: R_Scheme_key

        ![FINIT VOLUME SCHEME] (1: Berthon-Foucher)
        integer :: FV_Scheme_key

        ![NUMERICAL TEST]
        !> 1: Dam-break
        !> 2: Subcritical and transcritical flow over a bump
        !> 3: Streaming failure
        !> 4: Discontinuous topography with resonant regime
        !> 5: Thacker’s test
        integer test_case

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
        call parse_toml(filename, "x_min", data%x_min)
        call parse_toml(filename, "y_min", data%y_min)
        call parse_toml(filename, "Lx", data%Lx)
        call parse_toml(filename, "Ly", data%Ly)
        call parse_toml(filename, "Nx", data%Nx)
        call parse_toml(filename, "Ny", data%Ny)
        call parse_toml(filename, "test_case", data%test_case)
        call parse_toml(filename, "R_Scheme_key", data%R_Scheme_key)
        call parse_toml(filename, "FV_Scheme_key", data%FV_Scheme_key)

        if (data%mesh_key == 1) then
            data%dx = data%Lx / (data%Nx-1)
            data%dy = data%Ly / (data%Ny-1)
        end if

        if (data%Dimension == 1) then
            data%n_celle = data%Nx
        else if (data%Dimension == 2) then
            data%n_celle = (data%Nx) * (data%Ny)
        else
            print*, "Error: Dimension can't be greater than 2"
            stop
        end if

    end subroutine config_data
    
end module data_reader