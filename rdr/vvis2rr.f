c     $Id: vvis2rr.f,v 1.1.1.1 2001/10/19 10:31:41 lhaggmar Exp $
      SUBROUTINE VVIS2RR(TT,VVIS_RR,VVIS_SS,SFAC,RR)

      VVISRAIN(X) = -2.0 + 3.7*SQRT(X)

c     Based on Gray [1973], Hydrology, Rafael L. Bras page 249
      CM2MM(Tiw) = 8.*(0.02+0.1*((Tiw-252.0)/20)**2)

      SNOW=VVIS_SS
      RAIN=VVIS_RR

      if (RAIN.lt.0.0 .and. SNOW.lt.0.0) then
         RR=-99.0
         RETURN
      ELSE IF (RAIN.LT.0.0) THEN
         RAIN = 0.0
      ELSE IF (SNOW.LT.0.0) THEN
         SNOW = 0.0
      ENDIF

c     correct for systematic errors in rain-measuring 
      RAIN = VVISRAIN(RAIN)
      RAIN = MAX(RAIN,0.0)

c     convert mm snow to mm rain
      IF (TT.LT.-50.0 .OR. TT.GT.30) THEN
c        no observed temperature
         RR = RAIN + 0.1*SNOW*SFAC
      ELSE
         Tiw = TT + 273. - 1.5
         Tiw = MAX(Tiw,260.)
         FAC = CM2MM(Tiw)
c        VViS underestimates snowfall with a factor 1.6
         RR  = RAIN + 0.1*FAC*SNOW*SFAC
      ENDIF

c     problems with rain - remove obs if more than 5 mm rain
      if (rain.gt.5) RR = -99.0 

      RETURN
      END
