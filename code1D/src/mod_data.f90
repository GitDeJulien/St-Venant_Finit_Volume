module data_reader

    use precision
    use toml_parser
    implicit none

    public :: config_data

    type, public :: DataType

        ![STRUCTURED STATICAL MESHGRID]
        real(pr) :: x_min
        real(pr) :: Lx
        real(pr) :: Ly
        integer :: Nx
        real(pr) :: dx

        ![TIME] (implicite scheme)
        real(pr) :: t0
        integer  :: niter
        real(pr) :: tfinal
        real(pr) :: cfl
        real(pr) :: dt

        ![RIEMANN SCHEME] (1: HLL, 2: HLLC, 3: VFRoe)
        integer :: Riemann_solv
        integer :: ordre

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


        call parse_toml(filename, "x_min", data%x_min)
        call parse_toml(filename, "Lx", data%Lx)
        call parse_toml(filename, "Nx", data%Nx)

        call parse_toml(filename, "t0", data%t0)
        call parse_toml(filename, "niter", data%niter)
        call parse_toml(filename, "tfinal", data%tfinal)
        call parse_toml(filename, "cfl", data%cfl)

        call parse_toml(filename, "test_case", data%test_case)
        call parse_toml(filename, "Riemann_solv", data%Riemann_solv)
        call parse_toml(filename, "ordre", data%ordre)

        data%dx = data%Lx/(data%Nx)

        !data%dt = 1.0

    end subroutine config_data
    
end module data_reader