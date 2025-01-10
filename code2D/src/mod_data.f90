module data_reader

    use precision
    use toml_parser
    implicit none

    public :: config_data

    type, public :: DataType

        ![VERSION]
        character(len=10) :: version

        ![DIMENSION] (1 or 2)
        integer :: dim

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

        ![TIME] (implicite scheme)
        real(pr) :: t0
        integer  :: niter
        real(pr) :: cfl
        real(pr) :: dt

        ![RIEMANN SCHEME] (1: HLL, 2: HLLC, 3: VFRoe)
        integer :: Riemann_solv
        integer :: ordre

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
        call parse_toml(filename, "dim", data%dim)
        call parse_toml(filename, "Mesh_key", data%Mesh_key)
        call parse_toml(filename, "x_min", data%x_min)
        call parse_toml(filename, "y_min", data%y_min)
        call parse_toml(filename, "Lx", data%Lx)
        call parse_toml(filename, "Ly", data%Ly)
        call parse_toml(filename, "Nx", data%Nx)
        call parse_toml(filename, "Ny", data%Ny)

        call parse_toml(filename, "t0", data%t0)
        call parse_toml(filename, "niter", data%niter)
        call parse_toml(filename, "cfl", data%cfl)

        call parse_toml(filename, "test_case", data%test_case)
        call parse_toml(filename, "Riemann_solv", data%Riemann_solv)
        call parse_toml(filename, "ordre", data%ordre)

        call parse_toml(filename, "FV_Scheme_key", data%FV_Scheme_key)

        if (data%mesh_key == 1) then
            data%dx = data%Lx / (data%Nx)
            data%dy = data%Ly / (data%Ny)
        end if

        if (data%dim == 1) then
            data%n_celle = data%Nx
        else if (data%dim == 2) then
            data%n_celle = (data%Nx) * (data%Ny)
        else
            print*, "Error: dim can't be greater than 2"
            stop
        end if

        data%dt = 1.0

    end subroutine config_data
    
end module data_reader