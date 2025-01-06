module time_scheme_mod

    use flux_mod
    implicit none

    public time_step

    
contains

    function advance(df, Un, Wn) result(Unp1)

        !In
        type(DataType), intent(in)           :: df
        real(pr), dimension(:,:), intent(in) :: Un, Wn

        !Out
        real(pr), dimension(df%n_celle,2) :: Unp1

        !Local
        integer  :: e, k
        real(pr)               :: Xg, Xd
        real(pr), dimension(2) :: Fg, Fd, Source
        real(pr), dimension(df%n_celle-1) :: bi

        bi = eigen_values(df, Un)

        SELECT CASE(df%ordre)

            CASE(1) ! -- Ordre 1

                ! Internal edge loop
                do e=1,df%n_celle-1

                    ! -- Celle indices
                    k = e + 1 

                    ! -- Flux
                    Fg = flux_num(df, Wn, bi, e)
                    Fd = flux_num(df, Wn, bi, e+1)

                    Xg = Un(e,1)/Wn(e,1)
                    Xd = Un(e+1,1)/Wn(e+1,1)

                    Source(1) = 0.0_pr
                    Source(2) = Wn(e,1)*Wn(e+1,1)*(Xd - Xg)

                    ! -- Volumes finies scheme
                    Unp1(k,:) = Un(k,:) - df%dt/df%dx*(Xd*Fd(:) - Xg*Fg(:)) &
                                + 0.5_pr*grav*df%dt/df%dx*Source(:)

                enddo

                ! -- Boundary conditions: Homogenus Neumann
                Unp1(1,:) = Unp1(2,:)
                Unp1(df%n_celle,:) = Unp1(df%n_celle-1,:)

                ! Source(1) = 0.0_pr
                ! Source(2) = Wn(1,1)*Wn(2,1)*(Un(2,1)/Wn(2,1) - Un(1,1)/Wn(1,1))
                ! Unp1(1,:) = Un(1,:) - df%dt/df%dx*(Xd*Fd(:))&
                !             + 0.5_pr*grav*df%dt/df%dx*Source(:)

                ! Source(1) = 0.0_pr
                ! Source(2) = Wn(df%n_celle-1,1)*Wn(df%n_celle,1)*(Un(df%n_celle,1)/Wn(df%n_celle,1)&
                !             - Un(df%n_celle-1,1)/Wn(df%n_celle-1,1))
                ! Unp1(df%n_celle,:) = Un(df%n_celle,:)  - df%dt/df%dx*(- Xg*Fg(:))&
                !             + 0.5_pr*grav*df%dt/df%dx*Source(:)

            CASE DEFAULT
                print*, "No such order case yet coded"
                stop

        END SELECT


    end function advance


    function time_step(df, Wn) result(dt)
        
        !In
        type(DataType), intent(in)           :: df
        real(pr), dimension(:,:), intent(in) :: Wn
        

        !Out
        real(pr) :: dt

        !Local
        real(pr), dimension(df%n_celle-1) :: vp

        SELECT CASE(df%ordre)

            CASE(1) ! -- Ordre 1

                vp = eigen_values(df, Wn)

                dt = df%cfl*df%dx/(2._pr*MAXVAL(vp))

            CASE DEFAULT
                print*, "No such order case yet coded"
                stop

        END SELECT

    end function time_step
    
end module time_scheme_mod