	program readtimetails
        open(unit=20, file="motor_tails_direct_D001_N5.dat")
        open(unit=10, file="tail_t02_D001_N5.dat")
        open(unit=11, file="tail_t2_D001_N5.dat")
	
	a=0; b=0; c= 0
	do i=1,325
         read(20,*)F_ext,N,vmot,sigconf,time,total_area,sum_x
c	 if(time==0.200008288)then
	 if(time==1.00000882)then
	  a= a+1
	  write(10,*)a,F_ext,N,vmot,sigconf,time,total_area,sum_x
	 end if
         if(time==2.00000501)then
	  b= b+1
          write(11,*)b,F_ext,N,vmot,sigconf,time,total_area,sum_x
         end if
	end do

	close(20)
	close(10)
	close(11)
	stop
	end
