SUBROUTINE reconstruct_pe()

 USE data 

 IMPLICIT NONE 

 INTEGER :: hh_index(0:24) = -1
 INTEGER :: difdtg,dh,date_last,time_last,i,j,k,l,m
 INTEGER :: pe24_id,pe12_id,pe06_id,pe03_id,pe01_id
 INTEGER :: counter(5)

 !----------------------------------------------------------------------

 pe24_id = -1
 pe12_id = -1
 pe06_id  = -1
 pe03_id  = -1
 pe01_id  = -1
 DO i=1,nparver

  SELECT CASE(varprop(i)%id)
   CASE('PE24')
    pe24_id = i
   CASE('PE12')
    pe12_id = i
   CASE('PE6')
    pe06_id = i
   CASE('PE3')
    pe03_id = i
   CASE('PE1')
    pe01_id = i
  END SELECT

 ENDDO

 counter(:) = 0

 STATION_CYCLE : DO i=1,maxstn

  IF ( .NOT. obs(i)%active ) CYCLE 

  IF ( print_read > 1 ) &
  WRITE(6,*)'Scan station',obs(i)%stnr,obs(i)%ntim

  j = 1
  dh = 0
  hh_index(:) = -1
  date_last = obs(i)%o(j)%date 
  time_last = obs(i)%o(j)%time 
  hh_index(24) = j
  DO j=2,obs(i)%ntim
   dh =  difdtg(date_last,time_last*10000, &
                obs(i)%o(j)%date,obs(i)%o(j)%time*10000)/3600
   IF ( dh <= 25 ) THEN
    hh_index(0:24-dh) = hh_index(dh:24)
    hh_index(24-dh+1:)=-1
   ELSE
    hh_index(:) = -1
   ENDIF
   hh_index(24)=j
   
   date_last = obs(i)%o(j)%date 
   time_last = obs(i)%o(j)%time 
   
   ! Reconstruct 3h from 1h
   k = hh_index(23)
   l = hh_index(22)
   IF ( ALL((/k,l/) /= -1 ) ) THEN

    IF ( obs(i)%o(j)%val(pe03_id) <= err_ind .AND. &
         obs(i)%o(j)%val(pe01_id) >  err_ind .AND. &
         obs(i)%o(k)%val(pe01_id) >  err_ind .AND. &
         obs(i)%o(l)%val(pe01_id) >  err_ind ) THEN
         obs(i)%o(j)%val(pe03_id) = obs(i)%o(j)%val(pe01_id) + &
                                    obs(i)%o(k)%val(pe01_id) + &
                                    obs(i)%o(l)%val(pe01_id)
        IF ( print_read > 1 ) THEN
         WRITE(6,*)'Reconstruct 24h from 06h'
         WRITE(6,*)j,obs(i)%o(j)%date,obs(i)%o(j)%time,obs(i)%o(j)%val(pe01_id),obs(i)%o(j)%val(pe03_id)
         WRITE(6,*)k,obs(i)%o(k)%date,obs(i)%o(k)%time,obs(i)%o(k)%val(pe01_id)
         WRITE(6,*)l,obs(i)%o(l)%date,obs(i)%o(l)%time,obs(i)%o(k)%val(pe01_id)
        ENDIF
        counter(2) = counter(2)+1
    ENDIF

   ENDIF

   k = hh_index(18)
   IF ( k /= -1 ) THEN
     
    ! Reconstruct 6h from 6 and 12h
    IF ( obs(i)%o(j)%val(pe12_id) >  err_ind .AND. &
         obs(i)%o(j)%val(pe06_id) <= err_ind .AND. &
         obs(i)%o(k)%val(pe06_id) >  err_ind ) THEN
         obs(i)%o(j)%val(pe06_id) = obs(i)%o(j)%val(pe12_id) - &
                                    obs(i)%o(k)%val(pe06_id)
        IF ( print_read > 1 ) THEN
         WRITE(6,*)'Reconstruct 6h from 6 and 12h'
         WRITE(6,*)j,obs(i)%o(j)%date,obs(i)%o(j)%time,obs(i)%o(j)%val(pe12_id),obs(i)%o(j)%val(pe06_id)
         WRITE(6,*)k,obs(i)%o(k)%date,obs(i)%o(k)%time,obs(i)%o(k)%val(pe06_id)
        ENDIF
        counter(3) = counter(3)+1
    ENDIF

    ! Reconstruct 12h from 6h
    IF ( obs(i)%o(j)%val(pe12_id) <= err_ind .AND. &
         obs(i)%o(j)%val(pe06_id) >  err_ind .AND. &
         obs(i)%o(k)%val(pe06_id) >  err_ind ) THEN
         obs(i)%o(j)%val(pe12_id) = obs(i)%o(j)%val(pe06_id) + &
                                    obs(i)%o(k)%val(pe06_id)
        IF ( print_read > 1 ) THEN
         WRITE(6,*)'Reconstruct 12h from 6h'
         WRITE(6,*)j,obs(i)%o(j)%date,obs(i)%o(j)%time,obs(i)%o(j)%val(pe06_id),obs(i)%o(j)%val(pe12_id)
         WRITE(6,*)k,obs(i)%o(k)%date,obs(i)%o(k)%time,obs(i)%o(k)%val(pe06_id)
        ENDIF
        counter(4) = counter(4)+1
    ENDIF

   ENDIF

   k = hh_index(21)
   IF ( k /= -1 ) THEN
    ! Reconstruct 3h from 3 and 6h
    IF ( obs(i)%o(j)%val(pe06_id) >  err_ind .AND. &
         obs(i)%o(j)%val(pe03_id) <= err_ind .AND. &
         obs(i)%o(k)%val(pe03_id) >  err_ind ) THEN
         obs(i)%o(j)%val(pe03_id) = obs(i)%o(j)%val(pe06_id) - &
                                    obs(i)%o(k)%val(pe03_id)
        IF ( print_read > 1 ) THEN
         WRITE(6,*)'Reconstruct 3h from 3 and 6h'
         WRITE(6,*)j,obs(i)%o(j)%date,obs(i)%o(j)%time,obs(i)%o(j)%val(pe06_id),obs(i)%o(j)%val(pe03_id)
         WRITE(6,*)k,obs(i)%o(k)%date,obs(i)%o(k)%time,obs(i)%o(k)%val(pe03_id)
        ENDIF
        counter(2) = counter(2)+1
    ENDIF
    ! Reconstruct 6h from 3h
    IF ( obs(i)%o(j)%val(pe06_id) <= err_ind .AND. &
         obs(i)%o(j)%val(pe03_id) >  err_ind .AND. &
         obs(i)%o(k)%val(pe03_id) >  err_ind ) THEN
         obs(i)%o(j)%val(pe06_id) = obs(i)%o(j)%val(pe03_id) + &
                                    obs(i)%o(k)%val(pe03_id)
        IF ( print_read > 1 ) THEN
         WRITE(6,*)'Reconstruct 6h from 3h'
         WRITE(6,*)j,obs(i)%o(j)%date,obs(i)%o(j)%time,obs(i)%o(j)%val(pe03_id),obs(i)%o(j)%val(pe06_id)
         WRITE(6,*)k,obs(i)%o(k)%date,obs(i)%o(k)%time,obs(i)%o(k)%val(pe03_id)
        ENDIF
        counter(3) = counter(3)+1
    ENDIF

   ENDIF

   ! Reconstruction 
   k = hh_index(12)
   IF ( k /= -1 .AND. MOD(obs(i)%o(j)%time,3) == 0 ) THEN
     
    ! Reconstruct 24h from 12h 
    IF ( obs(i)%o(j)%val(pe24_id) <= err_ind .AND. &
         obs(i)%o(j)%val(pe12_id) >  err_ind .AND. &
         obs(i)%o(k)%val(pe12_id) >  err_ind ) THEN
         obs(i)%o(j)%val(pe24_id) = obs(i)%o(j)%val(pe12_id) + &
                                    obs(i)%o(k)%val(pe12_id)
        IF ( print_read > 1 ) THEN
         WRITE(6,*)'Reconstruct 24h from 12h'
         WRITE(6,*)j,obs(i)%o(j)%date,obs(i)%o(j)%time,obs(i)%o(j)%val(pe12_id),obs(i)%o(j)%val(pe24_id)
         WRITE(6,*)k,obs(i)%o(k)%date,obs(i)%o(k)%time,obs(i)%o(k)%val(pe12_id)
        ENDIF
        counter(5) = counter(5)+1
    ENDIF

   ENDIF

   ! Reconstruct 24h from 6 
   k = hh_index(18)
   l = hh_index(12)
   m = hh_index(06)
   IF ( ALL((/k,l,m/) /= -1 ) .AND. MOD(obs(i)%o(j)%time,3) == 0 ) THEN

    IF ( obs(i)%o(j)%val(pe24_id) <= err_ind .AND. &
         obs(i)%o(j)%val(pe06_id) >  err_ind .AND. &
         obs(i)%o(k)%val(pe06_id) >  err_ind .AND. &
         obs(i)%o(l)%val(pe06_id) >  err_ind .AND. &
         obs(i)%o(m)%val(pe06_id) >  err_ind ) THEN
         obs(i)%o(j)%val(pe24_id) = obs(i)%o(j)%val(pe06_id) + &
                                    obs(i)%o(k)%val(pe06_id) + &
                                    obs(i)%o(l)%val(pe06_id) + &
                                    obs(i)%o(m)%val(pe06_id)
        IF ( print_read > 1 ) THEN
         WRITE(6,*)'Reconstruct 24h from 06h'
         WRITE(6,*)j,obs(i)%o(j)%date,obs(i)%o(j)%time,obs(i)%o(j)%val(pe06_id),obs(i)%o(j)%val(pe24_id)
         WRITE(6,*)k,obs(i)%o(k)%date,obs(i)%o(k)%time,obs(i)%o(k)%val(pe06_id)
         WRITE(6,*)l,obs(i)%o(l)%date,obs(i)%o(l)%time,obs(i)%o(k)%val(pe06_id)
         WRITE(6,*)m,obs(i)%o(m)%date,obs(i)%o(m)%time,obs(i)%o(k)%val(pe06_id)
        ENDIF
        counter(5) = counter(5)+1
    ENDIF

   ENDIF

   ! Reconstruct 12h from 3 
   k = hh_index(21)
   l = hh_index(18)
   m = hh_index(15)
   IF ( ALL((/k,l,m/) /= -1 ) .AND. MOD(obs(i)%o(j)%time,3) == 0 ) THEN

    IF ( obs(i)%o(j)%val(pe12_id) <= err_ind .AND. &
         obs(i)%o(j)%val(pe03_id) >  err_ind .AND. &
         obs(i)%o(k)%val(pe03_id) >  err_ind .AND. &
         obs(i)%o(l)%val(pe03_id) >  err_ind .AND. &
         obs(i)%o(m)%val(pe03_id) >  err_ind ) THEN
         obs(i)%o(j)%val(pe12_id) = obs(i)%o(j)%val(pe03_id) + &
                                    obs(i)%o(k)%val(pe03_id) + &
                                    obs(i)%o(l)%val(pe03_id) + &
                                    obs(i)%o(m)%val(pe03_id)
        IF ( print_read > 1 ) THEN
         WRITE(6,*)'Reconstruct 12h from 03h'
         WRITE(6,*)j,obs(i)%o(j)%date,obs(i)%o(j)%time,obs(i)%o(j)%val(pe03_id),obs(i)%o(j)%val(pe12_id)
         WRITE(6,*)k,obs(i)%o(k)%date,obs(i)%o(k)%time,obs(i)%o(k)%val(pe03_id)
         WRITE(6,*)l,obs(i)%o(l)%date,obs(i)%o(l)%time,obs(i)%o(k)%val(pe03_id)
         WRITE(6,*)m,obs(i)%o(m)%date,obs(i)%o(m)%time,obs(i)%o(k)%val(pe03_id)
        ENDIF
        counter(4) = counter(4)+1
    ENDIF

   ENDIF

   ! Reconstruct 1h from 1h and 3h
   k = hh_index(23)
   l = hh_index(22)
   IF ( ALL((/k,l/) /= -1 ) ) THEN

    IF ( obs(i)%o(j)%val(pe01_id) <= err_ind .AND. &
         obs(i)%o(j)%val(pe03_id) >  err_ind .AND. &
         obs(i)%o(k)%val(pe01_id) >  err_ind .AND. &
         obs(i)%o(l)%val(pe01_id) >  err_ind ) THEN
         obs(i)%o(j)%val(pe01_id) = obs(i)%o(j)%val(pe03_id) - &
                                    obs(i)%o(k)%val(pe01_id) - &
                                    obs(i)%o(l)%val(pe01_id)
        IF ( print_read > 1 ) THEN
         WRITE(6,*)'Reconstruct 1h from 1h and 3h'
         WRITE(6,*)j,obs(i)%o(j)%date,obs(i)%o(j)%time,obs(i)%o(j)%val(pe03_id),obs(i)%o(j)%val(pe01_id)
         WRITE(6,*)k,obs(i)%o(k)%date,obs(i)%o(k)%time,obs(i)%o(k)%val(pe01_id)
         WRITE(6,*)l,obs(i)%o(l)%date,obs(i)%o(l)%time,obs(i)%o(k)%val(pe01_id)
        ENDIF
        counter(1) = counter(1)+1
    ENDIF

   ENDIF

  ENDDO

 ENDDO STATION_CYCLE

 ! Reset nparver
 nparver = nparver_org

 WRITE(6,*)'Precipication reconstructed ',SUM(counter),' observations'
 WRITE(6,*)' 1/3/6/12/24h:',counter
 WRITE(6,*)


END SUBROUTINE reconstruct_pe
