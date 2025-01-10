module init_mod

    use topography_mod
    implicit none

    public init_sol
    
contains

    subroutine init_sol(df, X, U0)

        !In
        type(DataType), intent(in)         :: df
        real(pr), dimension(:), intent(in) :: X

        !Out
        real(pr), dimension(:,:), intent(out) :: U0

        !Local
        integer :: k
        real(pr) :: const_init

        const_init = 0.66

        SELECT CASE(df%test_case)
            CASE(1)
                do k=1,df%Nx
                    if (X(k) < 0) then
                        U0(k,1) = 2.0_pr
                    else
                        U0(k,1) = 1./2.0_pr
                    endif
                    U0(k,2) = 0.0_pr
                enddo
            CASE(2)
                do k=1,df%Nx
                    U0(k,1) = const_init - topography1D(df, X(k), 0.0_pr)
                    U0(k,2) = 0.0_pr
                enddo
            CASE(3)
                do k=1,df%Nx
                    if (X(k) < 0.5) then
                        U0(k,1) = 1.0_pr
                        U0(k,2) = 3.0_pr*U0(k,1)
                    else
                        U0(k,1) = 1.2_pr
                        U0(k,2) = 0.1_pr*U0(k,1)
                    endif
                enddo
            CASE(4)
                do k=1,df%Nx
                    if (X(k) < -1._pr/2 .and. X(k) > -3.0_pr/2) then
                        U0(k,1) = 1./2.0_pr*(1 - (X(k)+1.0_pr/2)**2)
                    else
                        U0(k,1) = 0.0_pr
                    endif
                    U0(k,2) = 0.0_pr
                enddo
            CASE DEFAULT
                print*, "Error: No case found for 1 dimension."
                stop
        END SELECT

    end subroutine init_sol

    subroutine init_mesh(df, X)

        !In
        type(DataType), intent(in) :: df

        !Out
        real(pr), dimension(:)     :: X

        !Local
        integer :: i
        

        do i=1,df%Nx
            X(i) = df%x_min + 0.5_pr*df%dx + i*df%dx
        end do

    end subroutine init_mesh
    
end module init_mod