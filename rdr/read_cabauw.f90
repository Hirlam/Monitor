SUBROUTINE read_cabauw

!
! Read data from cabauw, provided by Anton Beljaars,
! and organize for evalutaion
!
! Ulf Andrae, ECMWF, 2003
!
 
 USE constants
 USE data
 USE functions

 IMPLICIT NONE
 
 INTEGER ::                     & 
            yy,mm,dd,			&
            i,j,k,l,ierr,		&
            wdate,wtime,		&
            odate,otime,		&
            cyear,				&
            ind,stat_i

 INTEGER ::								&
         YYMMDD1,HHMN1,					&
         IF201,ID201,IT201,IQ201,		&
         ISRD1,ITRD1,IRAIN1,			&
         IUST1,IH01,ILE1,ITRU1,			&
         ISQN1,IG01,					&
         YYMMDD2,HHMN2,					&
         IF202,ID202,IT202,IQ202,		&
         ISRD2,ITRD2,IRAIN2,			&
         IUST2,IH02,ILE2,ITRU2,			&
         ISQN2,IG02
         
 REAL ::									&
         DAYNR1,							&
         SF201,SD201,ST201,SQ201,			&
         SSRD1,STRD1,STRAINL1,SRAINC1,		&
         SUST1,SH01,SLE1,STRU1,				&
         SQN1,SG01,G51,G101,				&
         TS01,TS21,SRDD1,					&
         DAYNR2,							&
         SF202,SD202,ST202,SQ202,			&
         SSRD2,STRD2,STRAINL2,SRAINC2,		&
         SUST2,SH02,SLE2,STRU2,				&
         SQN2,SG02,G52,G202,TS02,TS22,SRDD2
         
 LOGICAL :: qcc

 CHARACTER(LEN=70) :: fname1='../dat/cabauw/cabauwYY.R31'

 !---------------------------------------------------------------------

 IF(lprint_read) WRITE(6,*)'READ_CABAUW'

 ! Check if we should do this station

 stat_i = 0
 DO i=1,maxstn
  IF (stnlist(i).EQ.05) stat_i = i
 ENDDO
 IF (stat_i.EQ.0) RETURN

 WRITE(6,*)'TRY TO READ CABAUW DATA',stnlist(stat_i)

 !
 ! Allocate obs arrays if it is not already done
 !

 IF (.NOT.ANY(obs%obs_is_allocated)) CALL allocate_obs

 ind   = INDEX(fname1,'YY')
 
 !
 ! Loop over all stations
 !

 STATION_LOOP : DO k=1,1

 IF(use_database) THEN 
    CALL read_database(stat_i,0,ierr)
    IF(ierr.EQ.0) CYCLE
 ENDIF

   IF(lprint_read) WRITE(6,*)'STATION',obs(stat_i)%stnr

   odate   = sdate
   i       = 0

   ! 
   ! Loop over all times
   ! 

   TIME_LOOP : DO

      cyear = odate /10000

      ! 
      ! Create filenames, open and exit/cycle or fail if first or second
      ! file not found. Read first dummy lines
      !    

      WRITE(fname1(ind:ind+1),'(I2.2)')MOD(cyear,100)
      OPEN(lunin,file=fname1,status='old',iostat=ierr)

      IF(ierr.NE.0) THEN
         WRITE(6,*)'Could not open',trim(fname1)
         cyear = cyear + 1
         odate = cyear *10000 + 100 + 1
         IF(odate.GT.edate) EXIT TIME_LOOP
         CYCLE TIME_LOOP
      ENDIF

      WRITE(6,*)'Could open ',trim(fname1)
   
      READ(lunin,*)
      READ(lunin,*)

      !
      ! Read file
      !

      READ_LOOP : DO
    
         ! Read 00 and 30 of each hour and take mean

         READ(lunin,*,iostat=ierr)		&
         YYMMDD1,HHMN1,DAYNR1,			&
         SF201,IF201,SD201,ID201,		&
         ST201,IT201,SQ201,IQ201,		&
         SSRD1,ISRD1,STRD1,ITRD1,		&
         STRAINL1,SRAINC1,IRAIN1,		&
         SUST1,IUST1,SH01,IH01,			&
         SLE1,ILE1,STRU1,ITRU1,			&
         SQN1,ISQN1,SG01,IG01,			&
         G51,G101,TS01,TS21,SRDD1
         
         
         IF(ierr.GT.0) THEN
         WRITE(6,*)		&
         YYMMDD1,HHMN1,DAYNR1,			&
         SF201,IF201,SD201,ID201,		&
         ST201,IT201,SQ201,IQ201,		&
         SSRD1,ISRD1,STRD1,ITRD1,		&
         STRAINL1,SRAINC1,IRAIN1,		&
         SUST1,IUST1,SH01,IH01,			&
         SLE1,ILE1,STRU1,ITRU1,			&
         SQN1,ISQN1,SG01,IG01,			&
         G51,G101,TS01,TS21,SRDD1

           WRITE(6,*)'IERR',IERR
           CALL ABORT
         ELSEIF(IERR.LT.0) THEN
           EXIT READ_LOOP
         ENDIF

         READ(lunin,*,iostat=ierr)		&
         YYMMDD2,HHMN2,DAYNR2,			&
         SF202,IF202,SD202,ID202,		&
         ST202,IT202,SQ202,IQ202,		&
         SSRD2,ISRD2,STRD2,ITRD2,		&
         STRAINL2,SRAINC1,IRAIN2,		&
         SUST2,IUST2,SH02,IH02,			&
         SLE2,ILE2,STRU2,ITRU2,			&
         SQN2,ISQN2,SG02,IG02,			&
         G52,G202,TS02,TS22,SRDD2
         
         IF(ierr.GT.0) THEN
         WRITE(6,*)		&
         YYMMDD2,HHMN2,DAYNR2,			&
         SF202,IF202,SD202,ID202,		&
         ST202,IT202,SQ202,IQ202,		&
         SSRD2,ISRD2,STRD2,ITRD2,		&
         STRAINL2,SRAINC1,IRAIN2,		&
         SUST2,IUST2,SH02,IH02,			&
         SLE2,ILE2,STRU2,ITRU2,			&
         SQN2,ISQN2,SG02,IG02,			&
         G52,G202,TS02,TS22,SRDD2
           CALL ABORT
           WRITE(6,*)'IERR',IERR
         ELSEIF(IERR.LT.0) THEN
           EXIT READ_LOOP
         ENDIF

         IF(lprint_read) WRITE(6,*)'READ DAY',	&
         yymmdd1,hhmn1,yymmdd2,hhmn2

         !
         ! Calculate date and correct time
         !

         wdate = 19000000 + yymmdd1
         wtime = (hhmn1 / 100) * 10000
         CALL adddtg(wdate,wtime,3600,odate,otime) 
         IF(odate.LT.sdate) CYCLE
         IF(odate.GT.edate) EXIT TIME_LOOP

         i = i + 1
   
         !
         ! Store data
         ! Fluxes are taken as mean over hour, inst. values as the latest instant
         ! NB !. Ground heat flux are calculated as residual
         !

         ALLOCATE(obs(stat_i)%o(i)%date)
         ALLOCATE(obs(stat_i)%o(i)%time)
         ALLOCATE(obs(stat_i)%o(i)%val(nparver))

         obs(stat_i)%o(i)%date  = odate
         obs(stat_i)%o(i)%time  = otime/10000
         obs(stat_i)%o(i)%val   = err_ind

         IF (ff_ind.NE.0 .AND. qcc(if202,if202,sf202,sf202))   obs(stat_i)%o(i)%val(ff_ind)  = sf202
         IF (dd_ind.NE.0 .AND. qcc(id201,id202,sd201,sd202))   obs(stat_i)%o(i)%val(dd_ind)  = sd202
         IF (tt_ind.NE.0 .AND. qcc(it201,it202,st201,st202))   obs(stat_i)%o(i)%val(tt_ind)  = st202
         IF (gr_ind.NE.0 .AND. qcc(isrd1,isrd2,ssrd1,ssrd2))   obs(stat_i)%o(i)%val(gr_ind)  = ( ssrd1 + ssrd2 ) * .5
         IF (sd_ind.NE.0 .AND. qcc(isrd1,isrd2,ssrd1,ssrd2))   obs(stat_i)%o(i)%val(sd_ind)  = ( ssrd1 + ssrd2 ) * .5
         IF (ld_ind.NE.0 .AND. qcc(itrd1,itrd2,strd1,strd2))   obs(stat_i)%o(i)%val(ld_ind)  = ( strd1 + strd2 ) * .5
         IF (lu_ind.NE.0 .AND. qcc(itru1,itru2,stru1,stru2))   obs(stat_i)%o(i)%val(lu_ind)  = ( stru1 + stru2 ) * .5
         IF (nr_ind.NE.0 .AND. qcc(isqn1,isqn2,sqn1,sqn2))     obs(stat_i)%o(i)%val(nr_ind)  = ( sqn1  + sqn2  ) * .5
         IF (wq_ind.NE.0 .AND. qcc(ile1,ile2,sle1,sle2))       obs(stat_i)%o(i)%val(wq_ind)  =-( sle2  + sle1  ) * .5
         IF (wt_ind.NE.0 .AND. qcc(ih01,ih02,sh01,sh02))       obs(stat_i)%o(i)%val(wt_ind)  =-(  sh02 +  sh01 ) * .5
         IF (uw_ind.NE.0 .AND. qcc(iust1,iust2,sust1,sust2))   obs(stat_i)%o(i)%val(uw_ind)  = -SQRT(( sust1 + sust2 ) * .5)
         IF (gs_ind.NE.0 .AND. qcc(ig01,ig02,sg01,sg02))       obs(stat_i)%o(i)%val(gs_ind)  = (sg01 + sg02 ) * .5

         IF (hb_ind.NE.0 .AND. qcc(ih01,ih02,sh01,sh02)		&
                         .AND. qcc(isqn1,isqn2,sqn1,sqn2)	&
                         .AND. qcc(ig01,ig02,sg01,sg02)		&
                         .AND. qcc(ile1,ile2,sle1,sle2)) 	&
                          obs(stat_i)%o(i)%val(hb_ind)  =  ( sqn1 + sqn2 - sle2 - sle1 - sh01 - sh02 -sg01 - sg02) * .5
      
         IF (gc_ind.NE.0 .AND. qcc(ih01,ih02,sh01,sh02)	&
                         .AND. qcc(isqn1,isqn2,sqn1,sqn2)	&
                         .AND. qcc(ile1,ile2,sle1,sle2)) &
                          obs(stat_i)%o(i)%val(gc_ind)  =  (( sqn1 + sqn2 ) - ( sle2 + sle1 ) - (  sh01 +  sh02 )) * .5
      
         IF(lprint_read) WRITE(6,*)'DATA IN',k,i,obs(stat_i)%o(i)%date,obs(stat_i)%o(i)%time
         IF(lprint_read) WRITE(6,*)'DATA', obs(stat_i)%o(i)%val

      ENDDO READ_LOOP

      CLOSE(lunin)

      !
      ! Step time
      !

      cyear = cyear + 1
      odate = cyear *10000 + 100 + 1
      IF(odate.GT.edate) EXIT TIME_LOOP

    ENDDO TIME_LOOP
    CLOSE(lunin)

    obs(stat_i)%ntim = i

    ! Try to use database if requested
    IF(use_database) CALL write_database(stat_i,0)

    IF ( obs(stat_i)%ntim.GT.0) THEN
    
       !
       ! If we compare with other than hourly model data we must average fluxes 
       ! model output period
       ! This is a bit messy because I try to do everything.....
       !
   
       write(6,*) 'STATION ',obs(stat_i)%stnr
       write(6,*) 'FOUND TIMES OBS',obs(stat_i)%ntim,maxtim
       write(6,*) 'OBS PERIOD',obs(stat_i)%o(1)%date,obs(stat_i)%o(obs(stat_i)%ntim)%date
       obs(stat_i)%active = .TRUE.

    ELSE
   
       obs(stat_i)%active = .FALSE.
       write(6,*) 'NO DATA FROM STATION ',obs(stat_i)%stnr,obs(stat_i)%active
   
    ENDIF
   
 ENDDO STATION_LOOP

 IF (lprint_read) WRITE(6,*)obs%ntim

END SUBROUTINE read_cabauw
!-------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------
LOGICAL FUNCTION qcc(a,b,c,d)

USE DATA, only : err_ind,gap_filled_data

IMPLICIT NONE

INTEGER :: a,b
REAL    :: c,d

  IF (gap_filled_data) THEN
     qcc = ((ABS(c+9999.).GT.1.e-6 .AND.	&
             ABS(d+9999.).GT.1.e-6      )    )
  ELSE
     qcc = ( a.EQ.0 .AND. b.EQ.0   .AND.    &
            (ABS(c+9999.).GT.1.e-6 .AND.	&
             ABS(d+9999.).GT.1.e-6      ))
  ENDIF
 

END FUNCTION qcc
