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
      CHARACTER*12, PARAMETER :: OUTSENS = 'GBM-NAR.OUT'

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
          WRITE(GUNIT,'(/,a)') " Run     RIL  DAYL  SRAD  TMAX  TMIN   DAP  RMSN MSNOD"  
          
          FIRST = .TRUE.
        ENDIF
        
!***********************************************************************
!***********************************************************************
!     Daily output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------    
        WRITE(GUNIT,'(I4,2X,A6,4F6.0,I6,2F6.2)')  &
             RUN,GENID,DAYL,SRAD,TMAX,TMIN,DAP,RMSNOD,MSNOD
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
