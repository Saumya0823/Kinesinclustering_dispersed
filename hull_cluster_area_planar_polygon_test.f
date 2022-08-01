	Program convexhull
	parameter (pi=3.141592, v= 180/pi)
	dimension x(10),y(10),poly_x(10),poly_y(10),
     1		angle(10),ar_sm(1000)
	
	open(unit=10,file='conf_areas_D1_N5.dat')
	open(unit=27,file="avg_area_motran_D1_N5.dat")
	open(unit=20,file="motor_tails_motran_D1_N5.dat")
	open(unit=21,file="time_points_motran_D1_N5.dat")

	l=0; iter=0; ic=0; itim=0
	do i=1,1000
	 ar_sm(i)=0
	end do
6	 read(20,*)for,conf,time,motno,xcoor,ycoor,zcoor
	 iter= iter+1
	 l= l+1
	 x(l)= xcoor
	 y(l)= ycoor
	 if(l==5 .and. l==motno)then
	  N=5
	  l=0
	  go to 7
	 else 
	  go to 6
	 end if
	
        do i=1,N
          poly_x(i)=0
          poly_y(i)=0
        end do

7	continue
	temp_x= x(1); temp_y= y(1)
	do i=2,N						! finding the point with smallest y-coordinate
	 if(y(i) .lt. temp_y)then
	  temp_y= y(i)
	  temp_x= x(i)
	 end if	
	end do

	chosen_x= temp_x					!chosen point acc to smallest y-coordinate
	chosen_y= temp_y
	poly_x(1)=chosen_x					!initialize the point to the array of the polygon vertices
	poly_y(1)=chosen_y
	start_x= chosen_x					!store point in the start point variable
	start_y= chosen_y
	prev_x= 0
	prev_y= chosen_y

	m=1; k=0

1	 k =k +1
	 do i=1,N
	 if(x(i)==chosen_x .and. y(i)==chosen_y)then
	   angle(i)= 1e-5
	 else
	    conss1= (chosen_y-prev_y)/(chosen_x-prev_x)								!slope of line
	    conss6=(y(i)-chosen_y)/(x(i)-chosen_x)
	    conss7= (conss6-conss1)/(1+(conss6*conss1))

	    if(conss7 .ge. 0)then
             angle(i)= atan(conss7)*v
	    else
             angle(i)= 180+(atan(conss7)*v)
	    end if
	 end if

	 do j=2,m
	  if(x(i)==poly_x(j) .and. y(i)==poly_y(j))then
	   angle(i)= 1e-5
	  end if
	 end do
	end do 

         it= 0
         do i=1,N
          if(angle(i) == 1e-5)then
           it= it+1
          end if
         end do

         if(it == (N-1))then
	  do i=1,N
	   if(angle(i) .gt. 1e-5)then
	     m= m+1
             poly_x(m)= x(i)
             poly_y(m)= y(i)
             prev_x= chosen_x
             prev_y= chosen_y
             chosen_x= x(i)
             chosen_y= y(i)
	   end if
	  end do
	  go to 100
         end if
		
	 do i=1,N
	   if(angle(i) .gt. 1e-5)then
	    temp_angle= angle(i)
	    itemp= i
	    exit
	   end if
	 end do

	 do j=1,N
         if (angle(j) .gt. 1e-5 .and. angle(j) .lt. temp_angle)then
	     temp_angle= angle(j)
	     itemp = j
	  end if 
	 end do
	 m= m+1
	 poly_x(m)= x(itemp)
	 poly_y(m)= y(itemp)
	 prev_x= chosen_x
	 prev_y= chosen_y	 
	 chosen_x= x(itemp)
	 chosen_y= y(itemp)

         if(poly_x(m).eq. start_x .and. poly_y(m).eq. start_y)then
          go to 100
         end if

	 it= 0
	 do i=1,N
	  if(angle(i) == 1e-5)then
	   it= it+1
	  end if
	 end do
	 if(it .lt. (N-1))then
		go to 1
	 end if

100 	continue

        sum_area1= poly_x(m-1)*poly_y(1)
        sum_area2= poly_x(1)*poly_y(m-1)
        do i=1,m-2
         sum_area1= sum_area1+ poly_x(i)*poly_y(i+1)
	 sum_area2= sum_area2+ poly_x(i+1)*poly_y(i) 
        end do
	total_area= (abs(sum_area1 - sum_area2))/2
        write(10,*) m-1,for,conf,time,total_area
	print*, "area of polygon=", total_area, iter

c	itim=0
	if(ic==int(conf))then
	 itim=itim+1
	 ar_sm(itim)= ar_sm(itim) +total_area
	else
	 ic = ic +1
         itim= 1
         ar_sm(itim)= ar_sm(itim) +total_area
	end if
	print*, "ic=", ic

c        if(ic==1000)then
c	  do j=1,500
c	     read(21,*) time, isigconf, dist, area
c	     ar_sm(j) = ar_sm(j)/isigconf
c	     write(27,*)m-1, for, isigconf, dist, ar_sm(j)
c	  end do 
c        end do

        if(iter .lt. 22190)then
	   print*, iter, ic
           go to 6
        else
c	 print*, iter, ic
        if(ic==1000)then
          do j=1,32
             read(21,*) time, isigconf, dist								!, area
             ar_sm(j) = ar_sm(j)/isigconf
             write(27,*) m-1, for,time,isigconf, dist, ar_sm(j)						!, area
          end do
	 end if
        end if

	close(27)
	close(20)
	close(21)
	close(10)
	stop
	end
	
