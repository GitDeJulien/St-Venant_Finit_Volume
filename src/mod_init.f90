module init_mod

    use topography_mod
    implicit none

    public init_sol

    ! type, public :: SolType



    ! end type
    
contains

    subroutine init_sol(df, celles, Hx, Qx)

        !In
        type(DataType), intent(in) :: df
        type(StructCelleType), dimension(:), intent(in) :: celles

        !Out
        real(pr), dimension(:), intent(out) :: Hx, Qx

        !Local
        integer :: k
        real(pr) :: const_init

        const_init = 0.66

        SELECT CASE(df%test_case)
            CASE(1)
                do k=1,df%n_celle
                    if (celles(k)%center_x_coord < 0) then
                        Hx(k) = 2.0_pr
                    else
                        Hx(k) = 1./2.0_pr
                    endif
                    Qx(k) = 0.0_pr
                enddo
            CASE(2)
                do k=1,df%n_celle
                    Hx(k) = const_init - topography(df, celles(k)%center_x_coord, 0.0_pr)
                    Qx(k) = 0.0_pr
                enddo
            CASE(3)
                do k=1,df%n_celle
                    if (celles(k)%center_x_coord < 0.5) then
                        Hx(k) = 1.0_pr
                        Qx(k) = 3.0_pr*Hx(k)
                    else
                        Hx(k) = 1.2_pr
                        Qx(k) = 0.1_pr*Hx(k)
                    endif
                enddo
            CASE(4)
                do k=1,df%n_celle
                    if (celles(k)%center_x_coord < -1._pr/2 .and. celles(k)%center_x_coord > -3.0_pr/2) then
                        Hx(k) = 1./2.0_pr*(1 - (celles(k)%center_x_coord+1.0_pr/2)**2)
                    else
                        Hx(k) = 0.0_pr
                    endif
                    Qx(k) = 0.0_pr
                enddo
            CASE DEFAULT
                print*, "Error: No case found for 1 dimension."
                stop
        END SELECT

    end subroutine init_sol

    ! subroutine save_sol(df, celles, path, tn, H, Q)

    !     !In
    !     type(DataType), intent(in)                       :: df
    !     type(StructCelleType), dimension(:), intent(in)  :: celles
    !     character(len=*), intent(in)                     :: path
    !     real(pr), intent(in)                             :: tn
    !     real(pr), dimension(:), intent(in)               :: H, Q

    !     !Local
    !     integer            :: k
    !     character(len=256) :: sol_filename
    !     character(len=10)  :: ch

    !     WRITE(ch, '(1F4.1)') tn
    !     sol_filename = trim(adjustl(path))//'/sol_' // trim(adjustl(ch)) // '.dat'

    !     open(unit=10, file=sol_filename, status='REPLACE', action='WRITE')
    !     SELECT CASE(df%dim)
    !     CASE(1)
    !         write(10,*) "## xk ", " SOL"
    !         do k=1,df%n_celle
    !             write(10,*) celles(k)%center_x_coord, H(k), Q(k)/H(k)
    !         end do
    !     CASE(2)
    !         write(10,*) "## xk ", " yk ", " SOL"
    !         do k=1,df%n_celle
    !             write(10,*) celles(k)%center_x_coord, celles(k)%center_y_coord, H(k), Q(k)/H(k)
    !         end do

    !     CASE DEFAULT
    !         print*, "Error: dim can't be greater than 2"
    !         stop

    !     END SELECT
    !     close(10)

    ! end subroutine save_sol
    
end module init_mod