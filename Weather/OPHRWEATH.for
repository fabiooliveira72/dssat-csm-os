C=======================================================================
C  OpHrWeath, Subroutine, F. Oliveira
C  Generates output for Hourly weather data.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  03/08/2022  FO Written
C-----------------------------------------------------------------------
C  Called from:   WEATHR
C  Calls:         None
C=======================================================================
      SUBROUTINE OPHRWEATH(CONTROL, ISWITCH, WEATHER,
     &    AMTRH, AZZON, BETA, FRDIFP, FRDIFR, PARHR,      !Hourly values
     &    RADHR, RHUMHR, TAIRHR, TAVG, TDAY, TGRO,        !Hourly values
     &    TGROAV, TGRODY, WINDHR)                         !Hourly values

!-----------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE
      SAVE

      CHARACTER*1  IDETG, IDETL, RNMODE, FMOPT
      CHARACTER*6,  PARAMETER :: ERRKEY = 'OPHRWE'
      CHARACTER*13, PARAMETER :: OUTWTH = 'HRWeather.OUT'

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, LUN
      INTEGER RUN, YEAR, YRDOY, REPNO, FYRDOY, WDATE
      INTEGER H,NDAY

      REAL
     &  CLOUDS, CO2, DAYL, OZON7, PAR, RAIN, SRAD, 
     &  TAVG, TDAY, TDEW, TGROAV, TGRODY,
     &  TMAX, TMIN, TWILEN, WINDSP, VPDF, vpd_transp,
     &  PARHRMJ, RADHRMJ
     
!     TS defined in ModuleDefs.for     
      REAL, DIMENSION(TS) :: AMTRH, AZZON, BETA, FRDIFP, FRDIFR, PARHR
      REAL, DIMENSION(TS) :: RADHR, RHUMHR, TAIRHR, TGRO, WINDHR

      LOGICAL FEXIST
      TYPE (WeatherType) WEATHER

C-----------------------------------------------------------------------
C     Define constructed variable types based on definitions in
C     ModuleDefs.for.
C-----------------------------------------------------------------------
      TYPE (ControlType) CONTROL
C-----------------------------------------------------------------------
C     The variable "ISWITCH" is of type "SwitchType".
C-----------------------------------------------------------------------
      TYPE (SwitchType) ISWITCH

      IDETG   = ISWITCH % IDETG
      IDETL   = ISWITCH % IDETL
      IF (IDETG .NE. 'Y' .OR. IDETL == '0') RETURN

      DAS     = CONTROL % DAS 
      DYNAMIC = CONTROL % DYNAMIC 
      FROP    = CONTROL % FROP  
      RNMODE  = CONTROL % RNMODE
      REPNO   = CONTROL % REPNO
      RUN     = CONTROL % RUN    
      YRDOY   = CONTROL % YRDOY   

      FMOPT   = ISWITCH % FMOPT   ! VSH
!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
          CALL GETLUN('OUTHRW', LUN)
          INQUIRE (FILE = OUTWTH, EXIST = FEXIST)
          IF (FEXIST) THEN
            OPEN (UNIT = LUN, FILE = OUTWTH, STATUS = 'OLD',
     &        IOSTAT = ERRNUM, POSITION = 'APPEND')
          ELSE
            OPEN (UNIT = LUN, FILE = OUTWTH, STATUS = 'NEW',
     &        IOSTAT = ERRNUM)
            WRITE(LUN,'("*WEATHER MODULE HOURLY OUTPUT FILE")')
          ENDIF
        END IF   ! VSH
        
        IF (RNMODE .NE. 'Q' .OR. RUN .EQ. 1) THEN
          IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
            !For first run of a sequenced run, use replicate
            ! number instead of run number in header.
            IF (RNMODE .EQ. 'Q') THEN
              CALL HEADER(SEASINIT, LUN, REPNO)
            ELSE
              CALL HEADER(SEASINIT, LUN, RUN)
            ENDIF

C-----------------------------------------------------------------------
C           Variable heading for Weather.OUT
C-----------------------------------------------------------------------
            WRITE (LUN,120)
  120       FORMAT('@YEAR DOY   DAS  HOUR',
     &'  AMTRH  AZZON   BETA FRDIFP FRDIFR  PARHR  RADHR RHUMHR',
     &' TAIRHR   TGRO WINDHR')
          END IF   ! VSH
        ENDIF

!***********************************************************************
!***********************************************************************
!     Daily Output - OUTPUT and SEASEND calls
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
C       Generate output for file Weather.OUT
        IF ((DYNAMIC .EQ. OUTPUT .AND. MOD(DAS,FROP) .EQ. 0) .OR.
     &      (DYNAMIC .EQ. SEASEND  .AND. MOD(DAS,FROP) .NE. 0) .OR. 
     &       DAS == 1) THEN 

      
          CALL YR_DOY(YRDOY, YEAR, DOY)
          IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! FMOPT 'A'
            !Horly printout

            IF (FYRDOY > 0) THEN
              WDATE = FYRDOY  !Date for weather forecasting ensembles
            ELSE
              WDATE = YRDOY
            ENDIF          
           
            DO H = 1,TS ! Loop through hourly weather data.
              
              
!             Convert units
!             VAR = J/m2/sec -> MJ/m2/day  
!              PARHRMJ = (24*60*60 * PARHR(H)) / 1000000  
!              RADHRMJ = (24*60*60 * RADHR(H)) / 1000000  
!             VAR = J/m2/sec -> MJ/m2/hr  
              PARHRMJ = (3600 * PARHR(H)) / 1000000  
              RADHRMJ = (3600 * RADHR(H)) / 1000000                
                                          
              WRITE (LUN,300) YEAR, DOY, DAS, H,
     &              AMTRH(H), AZZON(H), BETA(H), FRDIFP(H), 
     &              FRDIFR(H), PARHRMJ, RADHRMJ, RHUMHR(H), 
     &              TAIRHR(H), TGRO(H), WINDHR(H)
            ENDDO

  300       FORMAT(1X,I4,1X,I3,1X,I5,1X,I5,
     &        11(1X,F6.1))
     
          END IF   ! FMOPT 'A'
          
          IF (FMOPT == 'C') THEN ! Csv output

          END IF ! Csv output
        ENDIF

        IF ((DYNAMIC .EQ. SEASEND)
     &    .AND. (FMOPT == 'A'.OR. FMOPT == ' '))THEN
          !Close hourly output files.
          CLOSE (LUN)
        ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OPHRWEATH
!***********************************************************************
