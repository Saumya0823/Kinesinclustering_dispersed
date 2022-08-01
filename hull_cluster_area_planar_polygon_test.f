	Program convexhull
	parameter (pi=3.1415926535, v= 180/pi)
	dimension x(10),y(10),poly_x(10),poly_y(10),slope(10),angle(10)
	dimension phimot(1000),thetamot(1000)
	
	open(unit=10,file='coordinates.dat')
	open(unit=20,file="motor_tails_motran_D001_N5_F0.dat")

c	l=0
c	do i=1,1000
c6	 read(20,*)for,conf,time,motno,xcoor,ycoor,zcoor
c	 l= l+1
c1         phimot(l)= acos(2*rand()-1)
c          thetamot(l)= twopi*rand()
c          caviar1= beadRadius*cos(thetamot(l))*sin(phimot(l))
c          x(l)= beadinitx + caviar1
c          caviar2= beadRadius*sin(thetamot(l))*sin(phimot(l))
c          y(l)= beadinity + caviar2
c	 x(l)= xcoor
c	 y(l)= ycoor
c	 if(l==5 .and. l==motno)then
c	  l=0
c	  go to 5
c	 else 
c	  go to 6
c	 end if
c	end do

	do l=1,1000
1         phimot(l)= acos(2*rand()-1)
          thetamot(l)= twopi*rand()
          caviar1= beadRadius*cos(thetamot(l))*sin(phimot(l))
          x(l)= beadinitx + caviar1
          caviar2= beadRadius*sin(thetamot(l))*sin(phimot(l))
          y(l)= beadinity + caviar2
	  write(10,*)x(l), y(l)  
       end do
	
c	do i=1,N
c	 x(i)=0; y(i)=0
c	end do
	print*, "points="

5	continue
	do i=1,N   						! initialization of points
c2	  x(i)= 50*rand()
c	  y(i)= 100*rand()
c	  if(x(i) .lt. 1e-2 .or. y(i) .lt. 1e-2)then
c	   go to 2
c	  end if
	  print*, i, x(i), y(i)
	  write(10,*) i, x(i), y(i)
	end do

        do i=1,N
          poly_x(i)=0
          poly_y(i)=0
        end do

	temp_x= x(1); temp_y= y(1)
	do i=2,N						! finding the point with smallest y-coordinate
	 if(y(i) .lt. temp_y)then
	  itemp1= i
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
	print*, chosen_y, poly_y(1), start_y
	print*, chosen_x, poly_x(1), start_x
	prev_x= 0
	prev_y= chosen_y

	m=1; k=0

1	 k =k +1
	 do i=1,N
c	 print*, "prev x & y=", prev_x, prev_y
	 if(x(i)==chosen_x .and. y(i)==chosen_y)then
	   slope(i)=0; angle(i)= 1e-5
	 else
	    conss1= (chosen_y-prev_y)/(chosen_x-prev_x)								!slope of line
	    conss6=(y(i)-chosen_y)/(x(i)-chosen_x)
c	    print*, "two slopes=", conss1, conss6
	    conss2= chosen_y- (conss1*chosen_x)							! finding c
	    conss3= y(i)-(conss1*x(i))-conss2						! finding distance of pt. i
	    conss4= sqrt(1+conss1**2)
            distance= conss3/conss4                                     	        ! final distance of pt. i
	    conss5= sqrt((y(i)-chosen_y)**2+(x(i)-chosen_x)**2)         
	    slope(i)= distance/conss5
	    conss7= (conss6-conss1)/(1+(conss6*conss1))
            print*, "two slopes=", i, conss1, conss6, conss7
	    if(conss7 .ge. 0)then
c	     angle(i)= asin(slope(i))*v
             angle(i)= atan(conss7)*v
	    else
c	     angle(i)= 180+(asin(slope(i))*v)
             angle(i)= 180+(atan(conss7)*v)
	    end if
c	    print*, "angle=", slope(i), angle(i)
	 end if

	 do j=2,m
	  if(x(i)==poly_x(j) .and. y(i)==poly_y(j))then
	   slope(i)=0; angle(i)= 1e-5
	  end if
	 end do

            print*, "slope=",conss1, "c=",conss2, "d1=",conss3,
     1          "normal=", distance, "HP=", conss5, "angle=", angle(i)
c	 print*, "all slopes & angles=", i, slope(i), angle(i)
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
	    temp_slope= slope(i)
	    temp_angle= angle(i)
	    itemp= i
	    exit
	   end if
	 end do

	 print*, "start temp_slope & angle=", temp_slope, temp_angle
	 print*, temp_slope, itemp
	 do j=1,N
         if (angle(j) .gt. 1e-5 .and. angle(j) .lt. temp_angle)then
	     temp_angle= angle(j)
	     temp_slope= slope(i)
	     itemp = j
	     print*, j, angle(j), temp_angle, itemp
	  end if 
	 end do
5	 m= m+1
	 print*, m
	 print*, "end temp_slope & angle=",temp_slope, temp_angle
	 poly_x(m)= x(itemp)
	 poly_y(m)= y(itemp)
	 prev_x= chosen_x
	 prev_y= chosen_y	 
	 chosen_x= x(itemp)
	 chosen_y= y(itemp)
	 print*, "chosen pt.=", poly_x(m), poly_y(m)
	 print*, "chosen slope & angle=", slope_prev, angle_prev
	 print*, itemp, chosen_x, chosen_y

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

100	continue	
	print*, "no of sides of polygon=", m 
	do i=1,m
	 print*, "polygon vertices=", poly_x(i),poly_y(i)
	end do
	print*, "original points="
	do i =1,N
	 print*, x(i), y(i)
	end do

	print*, "polygon is=", m-1, "sided"
	do i=1,m-1
	 print*, "final polygon vertices=",poly_x(i),poly_y(i)
	end do

        sum_area1= poly_x(m-1)*poly_y(1)
        sum_area2= poly_x(1)*poly_y(m-1)
        do i=1,m-2
         sum_area1= sum_area1+ poly_x(i)*poly_y(i+1)
	 sum_area2= sum_area2+ poly_x(i+1)*poly_y(i) 
        end do
	total_area= (abs(sum_area1 - sum_area2))/2
	print*, "area of polygon=", total_area
	go to 6

	close(20)
	close(10)
	stop
	end
	
