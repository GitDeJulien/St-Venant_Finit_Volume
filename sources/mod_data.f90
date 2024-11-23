module data
    use precision

    implicit none

    type, public :: DataType

        ![USEFULL PATH]
        character(len=256) :: input_path
        character(len=256) :: test_path
        character(len=256) :: out_path

        ![USEFULL FILENAME]
        character(len=256) :: data_file
        character(len=256) :: data_test_case1
        character(len=256) :: data_test_case2

        ![MESH TYPE] (0: User define mesh, 1: Strctured statical mesh, 2: Unstructered statical mesh)
        integer :: mesh_key

        ![STRUCTURED STATICAL MESHGRID]
        real(pr) :: Lx
        real(pr) :: Ly
        real(pr) :: hx
        real(pr) :: hy

        ![TOPOLOGY FUNCTION] (0: User define function, 1: Forward and Backward Step, 2: Gaussian function)
        integer :: topology_key = 1 ;

        ![INITIAL WATER SURFACE] (0: User define function, 1:Dame configuration, 2: Unique wave)
        integer :: Water_Surface_key = 1 ;

        ![RIEMANN SCHEME] (1: HLL, 2: HLLC, 3: VFRoe)
        integer :: R_Scheme_key = 1 ;

        ![FINIT VOLUME SCHEME] (1: Berthon-Foucher)
        integer :: FV_Scheme_key = 1 ;


    end type DataType

contains
    
end module data