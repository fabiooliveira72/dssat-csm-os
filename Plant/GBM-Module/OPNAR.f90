!=======================================================================
!  OPNAR, Subroutine
!  Genetic output file for Gene-based module.
!-----------------------------------------------------------------------
!  REVISION   HISTORY
!-----------------------------------------------------------------------
!======================================================================= 
      SUBROUTINE OPNAR(CONTROL, ISWITCH, &
          GENID,DAYL,SRAD,TMAX,TMIN,MSNOD,RMSNOD,DAP)

!-------------------------------------------------------------------
      USE ModuleDefs
      
      IMPLICIT NONE
      SAVE

      CHARACTER* 6   GENID
      CHARACTER* 6, PARAMETER :: ERRKEY = 'OPGENE'
      CHARACTER*12, PARAMETER :: OUTSENS = 'RFM-Sens.OUT'

      LOGICAL FEXIST, FIRST
      
      INTEGER ERRNUM, GUNIT
      INTEGER RUN, DYNAMIC, DAS, YRDOY, YR, DOY, YRPLT
      INTEGER DAP, FDOY
      
      REAL DAYL, SRAD, TMAX, TMIN
      REAL MSNOD, RMSNOD
      
!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType) ISWITCH

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      RUN     = CONTROL % RUN
      DAS     = CONTROL % DAS
      YRDOY   = CONTROL % YRDOY

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
        CALL GETLUN('OUTRFS', GUNIT)
        INQUIRE (FILE = OUTSENS, EXIST = FEXIST)
        
        IF (FEXIST) THEN
          OPEN (UNIT = GUNIT, FILE = OUTSENS, STATUS = 'OLD', IOSTAT = ERRNUM, POSITION = 'APPEND')
          FIRST = .FALSE.
        ELSE
          OPEN (UNIT = GUNIT, FILE = OUTSENS, STATUS = 'NEW', IOSTAT = ERRNUM)
          WRITE(GUNIT,'("*RF MODULE SENSITIVITY OUTPUT",/)')
          WRITE(GUNIT,'(/,a)') " Run  Cultivar      DayLi     Sradi      Tmax     Tmin      MSNODE        ADAP      Fdoy"  
          
          FIRST = .TRUE.
        ENDIF
        
!***********************************************************************
!***********************************************************************
!     Daily output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------------------------------    
        WRITE(GUNIT,'(i4,2x,A6,3x,3f10.1,F10.2,2X,F10.5,I10,I10)') RUN,GENID,DAYL,SRAD,TMAX,TMIN,MSNOD,DAP,FDOY
!***********************************************************************
!***********************************************************************
!     SEASEND
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------        
        CLOSE (GUNIT)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
    END !SUBROUTINE OPGENE
!-----------------------------------------------------------------------
!     OPGENE Variable Definitions
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!     End Subroutine OPGENE
!-----------------------------------------------------------------------
