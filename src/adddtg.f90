SUBROUTINE adddtg(kdi,kti,kfp,kd,kt)

  !     returns new date yyyymmdd and time hhmmss by adding kfp (seconds,
  !     may be negative) to old values



  INTEGER, INTENT(IN OUT)                  :: kdi
  INTEGER, INTENT(IN)                      :: kti
  INTEGER, INTENT(IN)                      :: kfp
  INTEGER, INTENT(OUT)                     :: kd
  INTEGER, INTENT(OUT)                     :: kt
  PARAMETER(ispd=86400)

  !     convert to century date
  ic=idat2c(kdi)

  !     split kfp into days and seconds
  id=kfp/ispd
  IF(kfp < 0)id=id-1
  is=kfp-id*ispd

  !     decode kti into seconds, and add to seconds from kti
  ih=kti/10000
  ir=kti-ih*10000
  im=ir/100
  is=is + ir-im*100 + im*60 + ih*3600

  !     split seconds into days and seconds
  ir=is/ispd
  id=id+ir
  is=is-ir*ispd

  !     add days, convert to yyyymmdd format
  ic=ic+id
  kd=ic2dat(ic)

  !     convert seconds to hhmmss format
  ih=is/3600
  ir=is-ih*3600
  im=ir/60
  is=ir-im*60
  kt=ih*10000 + im*100 + is
END SUBROUTINE adddtg
