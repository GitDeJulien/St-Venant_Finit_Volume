module save_out_mod

    use structured_mesh_mod
    implicit none

    public save_approx_sol, save_exact_sol, save_topography
    
contains

    subroutine save_approx_sol(df, celles, iter, Un, Wn)

        !In
        type(DataType), intent(in)                      :: df
        type(StructCelleType), dimension(:), intent(in) :: celles
        integer, intent(in)                             :: iter
        real(pr), dimension(:,:), intent(in)            :: Un, Wn

        !Local
        integer :: i, io
        character(len=125) :: ch_iter

        write(ch_iter, '(I5)') iter

        open(newunit=io, file="output/sol1D/sol."//trim(adjustl(ch_iter))//".dat",&
                status="replace", action="write")

            write(io, *) "## xk  ", "  h  ", "  u"
            do i=1,df%n_celle
                write(io,*) celles(i)%center_x_coord, Un(i,1), Wn(i,1), Un(i,2)/Un(i,1)
            enddo

        close(io)

    end subroutine save_approx_sol


    subroutine save_exact_sol(df, celles, iter, Uexact, Topo)

        !In
        type(DataType), intent(in)                      :: df
        type(StructCelleType), dimension(:), intent(in) :: celles
        integer, intent(in)                             :: iter
        real(pr), dimension(:,:), intent(in)            :: Uexact
        real(pr), dimension(:), intent(in)              :: Topo

        !Local
        integer :: i, io
        character(len=125) :: ch_iter

        write(ch_iter, '(I5)') iter

        open(newunit=io, file="output/exact1D/sol."//trim(adjustl(ch_iter))//".dat",&
                status="replace", action="write")

            write(io, *) "## xk  ", "  h_exact  ", "  u_exact"
            do i=1,df%n_celle
                write(io,*) celles(i)%center_x_coord, Uexact(i,1), Uexact(i,1) + Topo(i), Uexact(i,2)
            enddo

        close(io)

    end subroutine save_exact_sol

    subroutine save_topography(data, mesh, iter, Topo)
        !In
        type(DataType), intent(in)                      :: data
        type(StructCelleType), dimension(:), intent(in) :: mesh
        integer, intent(in)                             :: iter
        real(pr), dimension(:), intent(in)              :: Topo

        !Local
        integer :: k
        character(len=256) :: topo_filename
        character(len=10)   :: ch

        WRITE(ch, '(I5)') iter
        topo_filename = 'output/topo/topo.' // trim(adjustl(ch)) // '.dat'

        open(unit=10, file=topo_filename, status='REPLACE', action='WRITE')
        SELECT CASE(data%dim)
        CASE(1)
            write(10,*) "## xk ", " topo"
            do k=1,data%n_celle
                write(10,*) mesh(k)%center_x_coord, Topo(k)
            end do
        ! CASE(2)
        !     write(10,*) "## xk ", " yk ", " topo"
        !     do k=1,data%n_celle
        !         write(10,*) mesh(k)%center_x_coord, mesh(k)%center_y_coord,&
        !         topography(data, mesh(k)%center_x_coord, mesh(k)%center_y_coord, tn)
        !     end do

        CASE DEFAULT
            print*, "Error: dim can't be greater than 2"
            stop

        END SELECT
        close(10)

    end subroutine save_topography

    subroutine save_error(df, Uexact, Un, iter, io)

        !In
        type(DataType), intent(in) :: df
        real(pr), dimension(:,:), intent(in) :: Uexact, Un
        integer, intent(in) :: iter, io

        !Local
        real(pr) :: errorL2
        ! real(pr) :: errorL1

        errorL2 = SUM((Uexact(:,1)-Un(:,1))**2, 1) + SUM((Uexact(:,2)-Un(:,2))**2, 1)
        errorL2 = SQRT(errorL2/(SUM(Uexact(:,1)) + SUM(Uexact(:,2)))**2)

        ! errorL1 = SUM(ABS(Uexact(:,1)-Un(:,1))/ABS(Uexact(:,1)), 1) + SUM(ABS(Uexact(:,2)-Un(:,2))/ABS(Uexact(:,2)), 1)
        ! errorL1 = 1._pr/df%n_celle*(errorL1)

        print*, "L2 error: ", errorL2

        if (iter == 0) write(io, *) "## error L2  "

        write(io, *) errorL2

        if (iter == df%niter) close(io)

    end subroutine save_error
    
end module save_out_mod