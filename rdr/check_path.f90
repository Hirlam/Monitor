SUBROUTINE check_path(isobs,date,time,fclen,exp,path,use_analysis,fname)

 !
 ! Ulf Andrae, SMHI 2021
 !
 ! Dynamic path and file name mangament
 !
 ! DATE = YYYYMMDD
 ! TIME = HHmmss
 ! LEAD TIME = [LHH|LHHH]:lm:ls
 !
 ! A complete expansion could look like
 ! SOME_DIR/@YYYY@/@MM@/@DD@/@HH@/@mm@/vfld@EXP@@YYYY@@MM@DD@HH@:@mm@:@SS@+@LHH@:@LM@:@LS@
 !

 IMPLICIT NONE

 CHARACTER(LEN=6), PARAMETER :: ctest(8) = (/'+     ','@LL@  ','@LH@  ','@LLL@ ', &
                                           & '@LHH@ ','@LHHH@','@LM@  ','@LS@  '/)

 LOGICAL,          INTENT(IN   ) :: isobs
 INTEGER,          INTENT(IN   ) :: date,time,fclen
 CHARACTER(LEN=*), INTENT(IN   ) :: exp,path
 LOGICAL,          INTENT(IN   ) :: use_analysis
 CHARACTER(LEN=*), INTENT(INOUT) :: fname

 INTEGER :: yy,mm,dd,hh,min,ss,lhh,lmin,lss,ind,i

 !------------------------------------------------

 fname = path

 yy  = date /10000
 mm  = MOD (date/100,100)
 dd  = MOD (date    ,100)
 hh  = time /10000
 min = MOD(time/100,100)
 ss  = MOD(time,100)
 lhh = INT(fclen/60)
 lmin= MOD(fclen,60)
 lss = 0

 ! Old style, add template
 IF ( isobs .AND. INDEX(fname,'vobs@YYYY@') == 0 ) THEN
  fname = TRIM(path)//'/vobs@YYYY@@MM@@DD@@HH@'
 ELSEIF ( .NOT. isobs               .AND. &
          INDEX(fname,'@EXP@') == 0 .AND. &
          INDEX(fname,'@L') == 0 ) THEN
  fname = TRIM(path)//'/vfld@EXP@@YYYY@@MM@@DD@@HH@@LL@'
 ENDIF

 CALL find_and_replace('@YYYY@',yy  ,fname)
 CALL find_and_replace('@MM@'  ,mm  ,fname)
 CALL find_and_replace('@DD@'  ,dd  ,fname)
 CALL find_and_replace('@HH@'  ,hh  ,fname)
 CALL find_and_replace('@mm@'  ,min ,fname)
 CALL find_and_replace('@SS@'  ,0   ,fname)

 IF ( .NOT. isobs ) THEN
  IF ( ALL((/lhh,lmin,lss/) == 0) .AND. use_analysis ) THEN
   DO i=1,LEN(ctest)
     ind = INDEX(fname,TRIM(ctest(i)))
     IF ( ind > 0 ) THEN
      fname = TRIM(fname(1:ind-1))//'                    '
      EXIT 
     ENDIF
   ENDDO
  ELSE
   CALL find_and_replace('@LL@'  ,lhh ,fname)
   CALL find_and_replace('@LH@'  ,lhh ,fname)
   CALL find_and_replace('@LLL@' ,lhh ,fname)
   CALL find_and_replace('@LHH@' ,lhh ,fname)
   CALL find_and_replace('@LHHH@',lhh ,fname)
   CALL find_and_replace('@LM@'  ,lmin,fname)
   CALL find_and_replace('@LS@'  ,lss ,fname)
  ENDIF
 ENDIF

 CALL find_and_replace_str('@EXP@',exp,fname)

 RETURN

 CONTAINS

 SUBROUTINE find_and_replace(str,val,fname)

  IMPLICIT NONE

   ! Input/output
   CHARACTER(LEN=*), INTENT(IN)    :: str
   INTEGER,          INTENT(IN)    :: val
   CHARACTER(LEN=*), INTENT(INOUT) :: fname

   ! Local
   CHARACTER (LEN=10) :: cc,cf
   INTEGER :: ind,i,ii

   cc = ''
   i = len(str)
   ii = i-2
   WRITE(cf,'(a2,i1,a1,i1,a1)')'(I',ii,'.',ii,')'
   DO
    ind = INDEX(fname,str)
    IF ( ind == 0 ) EXIT
    WRITE(cc,cf) val
    fname = TRIM(fname(1:ind-1))//TRIM(cc)//TRIM(fname(ind+i:))
   ENDDO

 END SUBROUTINE find_and_replace

 !-----------------------------------------------------------------------

 SUBROUTINE find_and_replace_str(str,val,fname)

  IMPLICIT NONE

   ! Input/output
   CHARACTER(LEN=*), INTENT(IN)    :: str
   CHARACTER(LEN=*), INTENT(IN)    :: val
   CHARACTER(LEN=*), INTENT(INOUT) :: fname

   ! Local
   INTEGER :: ind,i

   i = len(str)
   DO
    ind = INDEX(fname,str)
    IF ( ind == 0 ) EXIT
    fname = TRIM(fname(1:ind-1))//TRIM(val)//TRIM(fname(ind+i:))
   ENDDO

 END SUBROUTINE find_and_replace_str

END SUBROUTINE check_path

