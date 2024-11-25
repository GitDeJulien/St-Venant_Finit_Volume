module structured_mesh_mod
    use precision
    use data_reader
    implicit none

    type, private :: celle 

        real(pr) :: center_uval, center_hval
        real(pr) :: center_x_coord, center_y_coord

    end type

    interface build_mesh
        module procedure mesh1D
        module procedure mesh2D
    end interface
    
contains

    subroutine mesh1D(data, Mesh_key, x_coord)

        !In
        integer, intent(in)        :: Mesh_key
        type(DataType), intent(in) :: data

        !Out
        real(pr), dimension(:), intent(out) :: x_coord

        !Local
        integer :: i

        SELECT CASE(Mesh_key)
        CASE(1)
            do i=1,size(x_coord)
                x_coord(i) = data%dx * i !+data%x_min
            end do


        END SELECT
        
    end subroutine mesh1D

    subroutine mesh2D(data, Mesh_key, x_coord)

        !In
        integer, intent(in)        :: Mesh_key
        type(DataType), intent(in) :: data

        !Out
        real(pr), dimension(:), intent(out) :: x_coord

        SELECT CASE(Mesh_key)
        CASE(1)



        END SELECT
        
    end subroutine mesh2D


    
end module structured_mesh_mod