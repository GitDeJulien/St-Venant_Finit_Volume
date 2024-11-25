!> Module to define and download in data file the
!> the topography function. The latter can be 1D, 2D,
!> stationarry or time dependante.

module topography_mod

    use precision

    implicit none

    private topography1D
    private topography2D
    private dl_topography1D
    private dl_topography2D

    public topography
    public dl_topography

    interface topography
        module procedure topography1D
        module procedure topography2D
    end interface topography

    interface dl_topography
        module procedure dl_topography1D
        module procedure dl_topography2D
    end interface dl_topography
    
contains

    function topography1D(Topography_key, x_coord, tn) result(z0n)

        !In
        integer, intent(in)                :: Topography_key
        real(pr), dimension(:), intent(in) :: x_coord
        real(pr), intent(in), optional     :: tn

        !Out
        real(pr), dimension(size(x_coord)) :: z0n

        !Local
        integer  :: i
        real(pr) :: tn_

        if(present(tn)) tn_= tn

        SELECT CASE(Topography_key)
            case(1) !> constant topography
                z0n(:) = 0.01
            case(2) !> unstationnary but space constant topography 
                z0n(:) = SIN(2*pi*tn_/0.07)
            case(3) !> stationnary but not constant in space topography
                do i=1,size(x_coord)
                    if (x_coord(i) < 12 .and. x_coord(i) > 8) then
                        z0n(i) = 0.3 - 0.05*(x_coord(i)-10)**2
                    else 
                        z0n(i) = 0.1
                    end if
                end do

        END SELECT

    end function topography1D


    function topography2D(Topography_key, x_coord, y_coord, tn) result(z0n)

        !In
        integer, intent(in)                  :: Topography_key
        real(pr), dimension(:,:), intent(in) :: x_coord, y_coord
        real(pr), intent(in), optional       :: tn

        !Out
        real(pr), dimension(size(x_coord,1),size(x_coord,2)) :: z0n

        !Local
        integer :: Nx, Ny, i, j
        real(pr) :: tn_

        if(present(tn)) tn_= tn
        Nx = size(y_coord,1)
        Ny = size(y_coord,2)

        SELECT CASE(Topography_key)
            case(1) !> constant topography
                z0n(:,:) = 0.01
            case(2) !> unstationnary but space constant topography 
                z0n(:,:) = SIN(2*pi*tn_/0.07)
            case(3) !> stationnary but not constant in x direction topography
                do i=1,Nx
                    do j=1,Ny
                        if (x_coord(i,j) < 12 .and. x_coord(i,j) > 8) then
                            z0n(i,j) = 0.3 - 0.05*(x_coord(i,j)-10)**2
                        else 
                            z0n(i,j) = 0.1
                        end if
                    end do
                end do

        END SELECT

    end function topography2D

    subroutine dl_topography1D(Topography_key, x_coord, tn, topo_filename)

        !In
        integer, intent(in)                :: Topography_key
        real(pr), dimension(:), intent(in) :: x_coord
        real(pr), intent(in)               :: tn
        character(len=*), intent(in)       :: topo_filename

        !Local
        integer :: i
        real(pr), dimension(size(x_coord)) :: Z0

        Z0 = topography(Topography_key, x_coord, tn=tn)

        open(unit=10, file=topo_filename, status='REPLACE', action='WRITE')
            do i=1,size(x_coord)
                write(10,*) x_coord(i), Z0(i)
            end do
        close(10)

    end subroutine dl_topography1D


    subroutine dl_topography2D(Topography_key, x_coord, y_coord, tn, topo_filename)

        !In
        integer, intent(in)                  :: Topography_key
        real(pr), dimension(:,:), intent(in) :: x_coord, y_coord 
        real(pr), intent(in)                 :: tn
        character(len=*), intent(in)       :: topo_filename

        !Local
        integer :: i,j
        real(pr), dimension(size(x_coord,1), size(x_coord,2)) :: Z0

        Z0 = topography(Topography_key, x_coord, y_coord, tn=tn)

        open(unit=10, file=topo_filename, status='REPLACE', action='WRITE')
            do i=1,size(x_coord,1)
                do j=1,size(x_coord,2)
                    write(10,*) x_coord(i,j), y_coord(i,j), Z0(i,j)
                end do
            end do
        close(10)

    end subroutine dl_topography2D

    ! function test_topography(Topography_key) result(log)

    !     integer, intent(in) :: Topography_key

    !     logical, dimension(20,10) :: log

    !     !Local
    !     integer :: i,j, t
    !     real(pr) :: tn
    !     real(pr), dimension(20) :: X1D
    !     real(pr), dimension(20,10,2) :: X2D
    !     real(pr), dimension(20) :: z01D_fix, z01D, eps1D
    !     real(pr), dimension(20,10) :: z02D_fix, z02D, eps2D


    !     do i=1,20
    !         X1D(i) = i
    !         do j=1,10
    !             X2D(i,j,1) = i
    !             X2D(i,j,2) = j
    !             log(i,j) = .false.
    !         end do
    !     end do

    !     SELECT CASE(Topography_key)
    !     case(1)

    !         do i=1,20
    !             do t=0,10
    !                 tn = 0.5*t
    !                 eps1D = topography(topography_key, X1D, tn=tn) - 0.01
    !                 if 
    !             end do
    !         end do
    !         do t=0,10
    !             eps1D = topography(topography_key, X1D, tn=tn) - 0.01
                
    !         end do
    !     END SELECT
    !     !>TODO

    ! end function test_topography
    
end module topography_mod