SUBROUTINE do_stat(per_ind,p1,p2)

 USE constants
 USE data
 USE timing

 IMPLICIT NONE

 ! Input
 INTEGER, INTENT (IN) :: per_ind,p1(maxstn),p2(maxstn)

 ! Local
 INTEGER :: i,j,k,o,current_month,      &
            wrk(mparver),nlev
 INTEGER :: vertime(ntimver),timing_id, &
             par_active(nparver),       &
             rar_active(nparver,ntimver), &
             pro_active(nparver,0:ntimver)

 TYPE (statistics) :: onestat(nexp)
 TYPE (statistics), ALLOCATABLE  :: statall(:,:,:)

 CHARACTER(LEN=4 ) :: ttype = 'TIME'
 CHARACTER(LEN=38) :: text  = '    BIAS    MAE     RMSE    STDV     N'
 CHARACTER(LEN=5 ) :: clev
 CHARACTER(LEN=len_lab ) :: cid
!---------------------------------

 timing_id = 0
 IF (ltiming) CALL acc_timing(timing_id,'do_stat')

 IF(lprint_do_stat) WRITE(6,*)'--DO_STAT--',p1,p2

102 format(2A5,5(A31))
103 format(5(A31))

 ! Determine number of levels

 wrk = 0
 WHERE ( lev_lst > 0 ) wrk = 1 
 nlev = SUM(wrk)

 ! Recheck active stations
 par_active = 0
 rar_active = 0
 pro_active = 0
 DO i=1,maxstn
   DO j=1,ntimver
    DO k=1,nparver
       rar_active(k,j) = rar_active(k,j) + stat(i)%par_active(k,j)
    ENDDO
    IF ( ltemp ) THEN
     DO k=1,nparver,nlev
       pro_active(k,j) = pro_active(k,j) + MAXVAL(stat(i)%par_active(k:k+nlev-1,j))
     ENDDO
    ENDIF
   ENDDO
   IF ( ltemp ) THEN
     DO k=1,nparver,nlev
       pro_active(k,0) = pro_active(k,0) + MAXVAL(stat(i)%par_active(k:k+nlev-1,:))
     ENDDO
   ENDIF
 ENDDO

 !
 ! Allocate and init
 !

 IF (lallstat) THEN
    ALLOCATE(statall(nexp,nparver,ntimver))
    statall   = statistics(0.,0.,0.,0,0,0.,0.,0.,0.,0.)
 ENDIF

 IF ( lfcver ) THEN
   IF (lplot_seasonal) THEN
     DO i=1,ntimver 
       vertime(i)=i
     ENDDO
   ELSE
     vertime = use_fclen(1:nuse_fclen)
   ENDIF
 ELSE
    DO i=1,ntimver 
      vertime(i)=(i-1)*timdiff + time_shift
    ENDDO
 ENDIF

 IF (lfcver) ttype='  LL'

 !
 ! Loop over all stations
 !

 DO i = 1,maxstn

    ! Check if we should plot this station

    IF (MAXVAL(stnlist_plot) == -1 ) THEN
       leach_station = .FALSE.
    ELSEIF (MAXVAL(stnlist_plot) == 0 ) THEN
       leach_station = .TRUE.
    ELSE
       leach_station = .FALSE.
       DO j=1,maxstn
          IF (hir(i)%stnr == stnlist_plot(j)) THEN
             leach_station = .TRUE.
             EXIT
          ENDIF
       ENDDO
    ENDIF
    
    csi = i

    IF(lprint_do_stat) WRITE(6,*)'DO station', i,maxstn,stat(i)%active

    IF (.NOT.stat(i)%active) CYCLE

    IF(leach_station) THEN
       WRITE(lunstat,*)
       IF ( MINVAL(p1) < 13 ) THEN
          SELECT CASE(period_freq) 
          CASE(1)
             WRITE(lunstat,*)'Station',stat(i)%stnr,'Period ',seasonal_name1(MINVAL(p1))
          CASE(3)
             WRITE(lunstat,*)'Station',stat(i)%stnr,'Period ',seasonal_name2(MINVAL(p1))
          END SELECT
       ELSE
          WRITE(lunstat,*)'Station',stat(i)%stnr,p1(i),p2(i)
       ENDIF
       IF (nexp.GT.1) WRITE(lunstat,103)expname(1:nexp)
       WRITE(lunstat,102)'TYPE ',ttype,(text,o=1,nexp)
    ENDIF

    DO j=1,nparver

       cid = TRIM(varprop(j)%id)//TRIM(clev(varprop(j)%lev))
       onestat = statistics(0.,0.,0.,0,0,0.,0.,0.,0.,0.)

       DO k=1,ntimver

          IF (leach_station) &
          CALL write_stat(cid,vertime(k),stat(i)%s(:,j,k),nexp)
          DO o=1,nexp
             CALL acc_stat(onestat(o),stat(i)%s(o,j,k),1,1,1)
          ENDDO

       ENDDO

       IF(leach_station) CALL write_stat(cid,999,onestat,nexp)

    ENDDO

    !
    ! Print statistics against hour or forecast time
    !
    DO k=1,nparver
       par_active(k) = MAXVAL(rar_active(k,:))
    ENDDO

    IF (leach_station.AND. lprint_stat )                  &
    CALL print_stat2(lunout,nexp,nparver,ntimver,         &
    stat(i)%s,stat(i)%stnr,p1(i),p2(i),par_active,        &
    used_hours(:,per_ind,:),used_fclen(:,per_ind,:))

    !
    ! Plot statistics against level for specific
    ! hour or forecast time
    !

    IF ( ltemp .AND. leach_station  .AND.     &
         ( lplot_vert .OR. lprint_vert ) )  THEN

      IF ( lprint_vert )                                     &
      CALL print_vert(lunout,nexp,nlev,nparver,ntimver,      &
      stat(i)%s,stat(i)%stnr,p1(i),p2(i),pro_active,         &
      used_hours(:,per_ind,:),used_fclen(:,per_ind,:))

    ENDIF

    IF (lallstat) CALL acc_stat(statall,stat(i)%s,nexp,nparver,ntimver)

 ENDDO

 IF ( print_bias_map ) CALL print_map(0,minval(p1),maxval(p2),0,per_ind,rar_active)
 IF ( print_rmse_map ) CALL print_map(0,minval(p1),maxval(p2),1,per_ind,rar_active)
 IF ( print_stdv_map ) CALL print_map(0,minval(p1),maxval(p2),3,per_ind,rar_active)
 IF ( print_mabe_map ) CALL print_map(0,minval(p1),maxval(p2),4,per_ind,rar_active)

 IF ( print_obs_map  ) CALL print_map(0,minval(p1),maxval(p2),2,per_ind,rar_active)

 csi = 1

 IF (doing_monthvise) THEN
    current_month = minval(p1)
 ELSE
    current_month = 0
 ENDIF

 ALLSTAT : IF (lallstat.AND.(count(stat%active).GT.0)) THEN

    WRITE(lunstat,*)
    IF ( MINVAL(p1)< 13 ) THEN
       SELECT CASE(period_freq) 
       CASE(1)
          WRITE(lunstat,'(A4,I4,A19,A4)')'All ',    &
          count(stat%active),' stations  Period :',seasonal_name1(MINVAL(p1))
       CASE(3)
          WRITE(lunstat,'(A4,I4,A19,A4)')'All ',    &
          count(stat%active),' stations  Period :',seasonal_name2(MINVAL(p1))
       END SELECT
    ELSE
       WRITE(lunstat,'(A4,I4,A10,2I12)')'All ',    &
       count(stat%active),' stations',minval(p1),maxval(p2)
    ENDIF

    IF (nexp > 1) WRITE(lunstat,103)expname(1:nexp)
    WRITE(lunstat,102)'TYPE ',ttype,(text,o=1,nexp)

    LOOP_NPARVER : DO j=1,nparver

       cid = TRIM(varprop(j)%id)//TRIM(clev(varprop(j)%lev))
       onestat = statistics(0.,0.,0.,0,0,0.,0.,0.,0.,0.)

       DO k=1,ntimver

          CALL write_stat(cid,vertime(k),statall(:,j,k),nexp)

          DO o=1,nexp
             CALL acc_stat(onestat(o),statall(o,j,k),1,1,1)
          ENDDO

       ENDDO

       CALL write_stat(cid,999,onestat,nexp)

    ENDDO LOOP_NPARVER

    !
    ! Print statistics against hour or forecast time
    !

    IF( lprint_stat ) THEN
      IF (lprint_do_stat) WRITE(6,*)'Call print_stat'
      CALL print_stat2(lunout,nexp,nparver,ntimver,    &
      statall,0,minval(p1),maxval(p2),par_active,      &
      used_hours(:,per_ind,:),used_fclen(:,per_ind,:))
    ENDIF

    IF ( lsign_test .AND. lallstat ) THEN

       !
       ! Print significance test
       !

       CALL print_sign_test(lunout,nexp,nparver,          &
         0,minval(p1),maxval(p2),par_active,              &
         used_hours(:,per_ind,:),used_fclen(:,per_ind,:))

    ENDIF

    !
    ! Plot statistics against level for specific
    ! hour or forecast time
    !
    IF ( ltemp .AND. ( lplot_vert .OR. lprint_vert ) )  THEN

       IF ( lprint_vert )                               &
       CALL print_vert(lunout,nexp,nlev,nparver,ntimver,&
                      statall,0,MINVAL(p1),MAXVAL(p2),  &
                      pro_active,                       &
                      used_hours(:,per_ind,:),          &
                      used_fclen(:,per_ind,:))

    ENDIF

    DEALLOCATE(statall)

 ENDIF ALLSTAT

 IF (ltiming) CALL acc_timing(timing_id,'do_stat')

 RETURN
END SUBROUTINE do_stat
!---------------------------
!---------------------------
!---------------------------
SUBROUTINE acc_stat(s,p,i,j,k)

 USE types

 IMPLICIT NONE

 INTEGER :: i,j,k
 TYPE (statistics) :: s(i,j,k),p(i,j,k)

   s%r    = s%r    + p%r
   s%n    = s%n    + p%n
   s%bias = s%bias + p%bias
   s%rmse = s%rmse + p%rmse
   s%obs  = s%obs  + p%obs
   s%obs2 = s%obs2 + p%obs2
   s%obs3 = s%obs3 + p%obs3
   s%s2   = s%s2   + p%s2
   s%s3   = s%s3   + p%s3
   s%mabe = s%mabe + p%mabe

 RETURN
END SUBROUTINE acc_stat
!---------------------------
!---------------------------
!---------------------------
SUBROUTINE write_stat(p,t,s,n)

 USE types
 USE data, ONLY : len_lab,lunstat,lfcver

 IMPLICIT NONE

 INTEGER :: t,n
 CHARACTER(LEN=len_lab) :: p
 CHARACTER(LEN=3      ) :: cc='Day'
 TYPE (statistics) :: s(n)

! Local

 INTEGER :: o
 REAL    :: rn(n)
 CHARACTER(LEN=30) :: form1='(2A6,xx(4f8.3,I7))'
 CHARACTER(LEN=30) :: form2='(A6,I5,xx(4f8.3,I7))'
 
!------------------------------------------

 WRITE(form1(6:7),'(I2.2)')n
 WRITE(form2(8:9),'(I2.2)')n

 rn = 1.
 WHERE (s%n > 0 ) rn = FLOAT(s%n)
 !
 ! STD^2 = RMSE^2 - BIAS^2
 !

 IF (t.eq.999) THEN

       IF (lfcver) THEN
         cc='All'
       ELSE
         cc='Day'
       ENDIF

       WRITE(lunstat,form1)p,cc,              &
           (      s(o)%bias/rn(o),            &
                  s(o)%mabe/rn(o),            &
             sqrt(s(o)%rmse/rn(o)),           &
             sqrt(ABS(s(o)%rmse/rn(o)         &
                    -(s(o)%bias/rn(o))**2)),  &
                  s(o)%n,o=1,n)
       WRITE(lunstat,*)
 ELSE
       WRITE(lunstat,form2)p,t,               &
           (      s(o)%bias/rn(o),            &
                  s(o)%mabe/rn(o),            &
             sqrt(s(o)%rmse/rn(o)),           &
             sqrt(ABS(s(o)%rmse/rn(o)         &
                    -(s(o)%bias/rn(o))**2)),  &
                  s(o)%n,o=1,n)
 ENDIF

 RETURN
END SUBROUTINE write_stat
!--------------------------------------------------
CHARACTER(LEN=5) FUNCTION clev(lev)
 IMPLICIT NONE

 INTEGER :: lev,magn
 CHARACTER(LEN=4) :: cform = '(IX)'

 clev = ''

 IF (lev > 0 ) THEN
   magn=FLOOR(LOG10(FLOAT(lev)))+1
   WRITE(cform(3:3),'(I1)')magn
   WRITE(clev,cform)lev
 ENDIF

END FUNCTION clev
