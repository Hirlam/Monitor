MODULE contingency

 !
 ! Module for contingency table accumulation
 !
 ! Ulf Andrae, SMHI, Aug 2007
 !

 USE types

 IMPLICIT NONE

 SAVE

 ! Parameters
 INTEGER,           PARAMETER :: luncont  = 25

 ! Variables
 INTEGER :: ncont_param = 0

 TYPE (contingency_type), ALLOCATABLE ::  cont_table(:)

 CHARACTER(LEN=50)            :: contfile = 'contingency'

 LOGICAL :: do_ini_cont = .TRUE.


 !------------------------------------------------------
 !------------------------------------------------------
 !------------------------------------------------------

 CONTAINS

 !------------------------------------------------------
 !------------------------------------------------------
 !------------------------------------------------------

 SUBROUTINE clear_cont

  IMPLICIT NONE

  INTEGER :: i
  LOGICAL :: is_open

  DO i=1,ncont_param
     IF ( ASSOCIATED(cont_table(i)%table) ) DEALLOCATE(cont_table(i)%table)
     IF ( ASSOCIATED(cont_table(i)%limit) ) DEALLOCATE(cont_table(i)%limit)
     cont_table(i)%nclass = 0
     cont_table(i)%ind    = 0
     cont_table(i)%nval   = 0
  ENDDO

  IF ( ALLOCATED(cont_table) ) DEALLOCATE(cont_table)

  ncont_param = 0
  do_ini_cont = .TRUE.

  INQUIRE(FILE=contfile,OPENED=is_open)
  IF ( is_open ) CLOSE(luncont)


 END SUBROUTINE clear_cont

 !------------------------------------------------------
 !------------------------------------------------------
 !------------------------------------------------------

 SUBROUTINE ini_cont(nexp,                      &
                     xcont_mclass,xcont_mparam, &
                     xcont_class,xcont_param,   &
                     xcont_ind,xcont_lim)

  IMPLICIT NONE

  ! Input

  INTEGER, INTENT (IN) :: nexp
  INTEGER, INTENT (IN) :: xcont_mclass
  INTEGER, INTENT (IN) :: xcont_mparam,xcont_param
  INTEGER, INTENT (IN) :: xcont_class(xcont_mparam)
  INTEGER, INTENT (IN) :: xcont_ind(xcont_mparam)
  REAL,    INTENT (IN) :: xcont_lim(xcont_mclass,xcont_mparam)

  ! Local

  INTEGER :: i

  !------------------------------------------------------------------

  !
  ! Clear data
  !

  CALL clear_cont

  !
  ! Allocate and copy external information
  !

  ncont_param = xcont_param

  ALLOCATE(cont_table(ncont_param))

  DO i=1,ncont_param

     ALLOCATE(cont_table(i)%limit(xcont_class(i)))
     ALLOCATE(cont_table(i)%table(nexp,0:xcont_class(i),0:xcont_class(i)))

     cont_table(i)%limit  = xcont_lim(1:xcont_class(i),i)
     cont_table(i)%table  = 0
     cont_table(i)%ind    = xcont_ind(i)
     cont_table(i)%nclass = xcont_class(i)

  ENDDO

  do_ini_cont = .FALSE.
  
 END SUBROUTINE ini_cont

 !------------------------------------------------------
 !------------------------------------------------------
 !------------------------------------------------------

 SUBROUTINE acc_cont(nexp,nparver,dat)

  IMPLICIT NONE

  ! Input

  INTEGER,            INTENT(IN) :: nexp,nparver
  TYPE(scatter_type), INTENT(IN) :: dat(nparver)

  ! Local

  INTEGER :: i,j,k,l,m,oc,mc
  
  !-----------------------------------------------

  DO i=1,ncont_param

     cont_table(i)%table(:,:,:) = 0

     DO j=1,nparver

        IF ( j /= cont_table(i)%ind ) CYCLE

        cont_table(i)%nval = dat(j)%n

        DO k=1,dat(j)%n

           oc = 0
           DO m=cont_table(i)%nclass,1,-1
              IF ( dat(j)%dat(1,k) > cont_table(i)%limit(m)) THEN
                 oc = m
                 EXIT
              ENDIF
           ENDDO

           DO l=1,nexp
              mc = 0
              DO m=cont_table(i)%nclass,1,-1
                 IF ( dat(j)%dat(l+1,k) +    &
                      dat(j)%dat(  1,k) > cont_table(i)%limit(m)) THEN
                    mc = m
                    EXIT
                 ENDIF
              ENDDO
              cont_table(i)%table(l,oc,mc) = cont_table(i)%table(l,oc,mc) + 1
           ENDDO

        ENDDO

     ENDDO

  ENDDO

  RETURN

 END SUBROUTINE acc_cont

 !------------------------------------------------------
 !------------------------------------------------------
 !------------------------------------------------------

END MODULE contingency
