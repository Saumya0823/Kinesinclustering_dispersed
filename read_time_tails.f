	program readtimetails
        open(unit=20, file="motor_tails_ran_D1_N5.dat")
        open(unit=10, file="tail_t02_D1_N5.dat")
        open(unit=11, file="tail_t2_D1_N5.dat")
c        open(unit=12, file="tail_t1_D1_N5.dat")
	
	a=0; b=0; c= 0
	do i=1,1778
         read(20,*)F_ext,N,vmot,sigconf,time,total_area,sum_x
	 if(time==0.200008288)then
	  a= a+1
	  write(10,*)a,F_ext,N,vmot,sigconf,time,total_area,sum_x
	 end if
c         if(time==3.00000119)then
	  if(time==2.00000501)then
	  b= b+1
          write(11,*)b,F_ext,N,vmot,sigconf,time,total_area,sum_x
         end if
c         if(time==1.00000882)then
c	  c= c+1
c          write(12,*)c,F_ext,N,vmot,sigconf,time,total_area,sum_x
c         end if
	end do

	close(20)
	close(10)
	close(11)
c	close(12)
	stop
	end
