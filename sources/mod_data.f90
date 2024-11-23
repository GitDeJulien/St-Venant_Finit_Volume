module data_reader
    use precision
    use toml_parser
    implicit none

    public :: store_data

    type, public :: DataType

        ![VERSION]
        character(len=5) :: version

        ![USEFULL PATH]
        character(len=256) :: input_path
        character(len=256) :: test_path
        character(len=256) :: output_path

        ![TEST CASE] (0: User define case, 1: case1, 2: case2)
        integer :: test_case_key

        ![MESH TYPE] (0: User define mesh, 1: Strctured statical mesh, 2: Unstructered statical mesh)
        integer :: mesh_key

        ![STRUCTURED STATICAL MESHGRID]
        real(pr) :: Lx
        real(pr) :: Ly
        real(pr) :: hx
        real(pr) :: hy

        ![TOPOLOGY FUNCTION] (0: User define function, 1: Forward and Backward Step, 2: Gaussian function)
        integer :: topology_key

        ![INITIAL WATER SURFACE] (0: User define function, 1:Dame configuration, 2: Unique wave)
        integer :: Water_Surface_key

        ![RIEMANN SCHEME] (1: HLL, 2: HLLC, 3: VFRoe)
        integer :: R_Scheme_key

        ![FINIT VOLUME SCHEME] (1: Berthon-Foucher)
        integer :: FV_Scheme_key

    end type DataType

contains

    subroutine store_data(data, filename)
        character(len=*), intent(in) :: filename
        type(DataType), intent(inout) :: data

        call parse_toml(filename, "version", data%version)
        call parse_toml(filename, "input_path", data%input_path)
        call parse_toml(filename, "test_path", data%test_path)
        call parse_toml(filename, "output_path", data%output_path)
        call parse_toml(filename, "test_case_key", data%test_case_key)
        call parse_toml(filename, "mesh_key", data%mesh_key)
        call parse_toml(filename, "Lx", data%Lx)
        call parse_toml(filename, "Ly", data%Ly)
        call parse_toml(filename, "hx", data%hx)
        call parse_toml(filename, "hy", data%hy)
        call parse_toml(filename, "topology_key", data%topology_key)
        call parse_toml(filename, "Water_Surface_key", data%Water_Surface_key)
        call parse_toml(filename, "R_Scheme_key", data%R_Scheme_key)
        call parse_toml(filename, "FV_Scheme_key", data%FV_Scheme_key)

        

    end subroutine store_data
    
end module data_reader