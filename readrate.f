	Program ratiorates
	open(unit=21, file= "Rate_dynam_diff_V_N10_D001.dat")
        open(unit=22, file= "Rate_dynam_diff_V_N10_D01.dat")
        open(unit=24, file= "Rate_dynam_diff_V_N10_D1.dat")
	open(unit=23, file= "ratios_rates_V_N10_F0.dat")

c        open(unit=23, file= "RLV_DC_latticeMT_N10_D001.dat")
c        open(unit=22, file= "RLV_DC_latticeMT_N10_D01.dat")
c        open(unit=21, file= "RLV_DC_latticeMT_N10_D1.dat")
c        open(unit=24, file= "att_rates_V_N5_F0.dat"")

	do i=1,33
	 if(i .le. 11)read(21,*)iF,N,imtd,beadRadius,detavg,
     1          det_SD,det_SEM,
     1          attavg,att_SD,att_SEM,stpavg,step_SD,step_SEM
         if(i .gt. 11 .and. i .le. 22)read(22,*)iF,N,imtd,
     1		beadRadius,detavg,det_SD,det_SEM,
     1          attavg,att_SD,att_SEM,stpavg,step_SD,step_SEM
         if(i .gt. 22 .and. i .le. 33)read(24,*)iF,N,imtd,
     1		beadRadius,detavg,det_SD,det_SEM,
     1          attavg,att_SD,att_SEM,stpavg,step_SD,step_SEM
	if(i==1 .or. i==12 .or. i==23)write(23,*)iF,N,imtd,
     1		beadRadius,detavg/attavg,detavg,det_SD,det_SEM,
     1          attavg,att_SD,att_SEM,stpavg,step_SD,step_SEM
	end do

c        do i=1,33
c         if(i .le. 11)read(23,*)-F_ext,Fxavg,N,imtd,beadRadius,
c     1		RLavg,SD,SEM,avgt,tavg_SD,tavg_SEM,avgact,
c     1		active_SD,active_SEM,t_mot_avg
c         if(i .gt. 11 .and. i .le. 22)read(22,*)-F_ext,Fxavg,N,
c     1		imtd,beadRadius,RLavg,SD,SEM,avgt,tavg_SD,tavg_SEM,
c     1		avgact,active_SD,active_SEM,t_mot_avg
c         if(i .gt. 22 .and. i .le. 33)read(21,*)-F_ext,Fxavg,N,imtd,
c     1		beadRadius,RLavg,SD,SEM,avgt,tavg_SD,tavg_SEM,avgact,
c     1		active_SD,active_SEM,t_mot_avg
c        if(i==1 .or. i==12 .or. i==23)write(24,*)-F_ext,Fxavg,N,imtd,
c     1		beadRadius,RLavg,SD,SEM,avgt,tavg_SD,tavg_SEM,avgact,
c     1		active_SD,active_SEM,t_mot_avg
c        end do
	close(21)
	close(22)
	close(23)
	close(24)
	End Program
