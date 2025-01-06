module time_scheme_mod

    use flux_mod
    implicit none
    
contains

    function time_step(df, U) result(dt)
        
        !In
        type(DataType), intent(in)           :: df
        real(pr), dimension(:,:), intent(in) :: U 
        

        !Out
        real(pr) :: dt

        !Local
        real(pr), dimension(df%n_celle-1) :: vp

        vp = eigen_values(df, U)

        dt = df%cfl*df%dx/MAXVAL(vp)

    end function time_step
    
end module time_scheme_mod