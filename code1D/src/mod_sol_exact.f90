module exact_sol_mod

    use data_reader
    implicit none

    real(pr), parameter :: h_init_am = 2.0_pr
    real(pr), parameter :: h_init_av = 1._pr/2._pr

    public exact_sol_fct
    private f_sigma_vp
    private detente_h_u
    
contains


    subroutine exact_sol_fct(df, X, tn, sol_h, sol_u)

        !In
        type(DataType), intent(in)         :: df
        real(pr), dimension(:), intent(in) :: X
        real(pr), intent(in)               :: tn
        
        !Out
        real(pr), dimension(:), intent(out) :: sol_h, sol_u
        
        !Local
        integer  :: i, imax
        real(pr) :: sigma
        real(pr) :: lambda_g, lambda_etoile
        real(pr) :: h, u, h_etoile, u_etoile
        real(pr), dimension(df%Nx) :: x_i

        imax = df%Nx
        call f_sigma_vp(df, sigma, lambda_g, lambda_etoile, h_etoile, u_etoile)

        do i=1,df%Nx
            x_i(i) = X(i)
        enddo

        do i=1, imax

            if (x_i(i) < lambda_g*tn) then
                
                sol_h(i) = h_init_am
                sol_u(i) = 0.
                
            else if (x_i(i) >= lambda_g*tn .AND. x_i(i) < lambda_etoile*tn) then

                call detente_h_u(x_i(i), tn, h, u)
                sol_h(i) = h
                sol_u(i) = u


            else if (x_i(i) >= lambda_etoile*tn .AND. x_i(i) < sigma*tn) then

                sol_h(i) = h_etoile
                sol_u(i) = u_etoile

            else

                sol_h(i) = h_init_av
                sol_u(i) = 0

            end if

        end do
    end subroutine exact_sol_fct

    ! ##################################################################

    subroutine f_sigma_vp(df, sigma, lambda_g, lambda_etoile, h_etoile, u_etoile)

        type(DataType), intent(in)   :: df
        real(pr), intent(out)        :: sigma
        real(pr), intent(out)        :: lambda_g, lambda_etoile
        real(pr), intent (out)       :: h_etoile, u_etoile

        ! -- Variable local
        real(pr) :: fn, fb, hnp1, eps
        integer  :: imax

        !imax = size(flux%hnp1)-2
        imax = df%Nx
        eps = 10.**(-5)


        h_etoile = 0.
        lambda_g = -sqrt(grav*h_init_am)
        hnp1 = h_init_am

        do while (abs(hnp1-h_etoile) > eps)
            
            h_etoile = hnp1

            fn = 2*(sqrt(grav*h_init_am)-sqrt(grav*h_etoile))+(h_init_av-h_etoile)&
                *sqrt(grav*(h_etoile+h_init_av)/(2*h_etoile*h_init_av))
            fb = 2*(sqrt(grav*h_init_am)-sqrt(grav*h_init_av))

            hnp1 = h_etoile - (h_init_av-h_etoile)/(fb-fn)*fn

        end do
        
        h_etoile = hnp1

        u_etoile = 2*(sqrt(grav*h_init_am)-sqrt(grav*h_etoile))
        sigma = h_etoile*u_etoile/(h_etoile-h_init_av)
        lambda_etoile = u_etoile - sqrt(grav*h_etoile)

    end subroutine

    ! ###################################################

    subroutine detente_h_u(x, t, h, u)

        real(pr), intent(in)         :: x, t
        real(pr), intent(out)        :: h, u

        ! -- Variables locales
        real(pr) :: ksi


        ksi = x/t
        h = 1./(9*grav)*(2*sqrt(grav*h_init_am)-ksi)**2
        u = 2*(sqrt(grav*h_init_am)-sqrt(grav*h))

    end subroutine

    ! ####################################################
    
end module exact_sol_mod