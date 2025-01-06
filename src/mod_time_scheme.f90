module time_scheme_mod

    use flux_mod
    implicit none

    public time_step

    
contains

    function advance(df, Un, Topo) result(Unp1)

        !In
        type(DataType), intent(in)           :: df
        real(pr), dimension(:,:), intent(in) :: Un
        real(pr), dimension(:), intent(in)   :: Topo

        !Out
        real(pr), dimension(df%n_celle,2) :: Unp1

        !Local
        integer  :: e, k
        real(pr)               :: Xg, Xd
        real(pr), dimension(2) :: Fg, Fd, Source
        real(pr), dimension(df%n_celle-1) :: bi
        real(pr), dimension(df%n_celle,2) :: Wn

        bi = eigen_values(df, Un)
        Wn(:,1) = Un(:,1) + Topo(:)
        Wn(:,2) = Wn(:,1) * Un(:,2)/Un(:,1)

        SELECT CASE(df%ordre)

            CASE(1) ! -- Ordre 1

                ! Internal edge loop
                do e=1,df%n_celle-1

                    ! -- Celle indices
                    k = e + 1 

                    ! ! -- Flux
                    ! Fg = flux_num(df, Un, bi, e)
                    ! Fd = flux_num(df, Un, bi, e+1)

                    ! ! -- Volumes finies scheme
                    ! Unp1(k,:) = Un(k,:) - df%dt/df%dx*(Fd(:) - Fg(:))

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

                ! -- Neumann homogene boundary conditions
                Unp1(1,:) = Unp1(2,:)
                Unp1(df%n_celle,:) = Unp1(df%n_celle-1,:)

            CASE DEFAULT
                print*, "No other order case yet coded"

        END SELECT


    end function advance

    function time_step(df, U) result(dt)
        
        !In
        type(DataType), intent(in)           :: df
        real(pr), dimension(:,:), intent(in) :: U 
        

        !Out
        real(pr) :: dt

        !Local
        real(pr), dimension(df%n_celle-1) :: vp

        vp = eigen_values(df, U)

        dt = df%cfl*df%dx/(2._pr*MAXVAL(vp))

    end function time_step
    
end module time_scheme_mod