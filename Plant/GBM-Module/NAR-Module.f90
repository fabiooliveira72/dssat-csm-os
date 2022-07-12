!=======================================================================
!  NAR-Module, Subroutine, Fabio Oliveira
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!======================================================================= 
!------------------------------------------------------------------------      

      SUBROUTINE RFMODULE(CONTROL, ISWITCH,                   &  !Control
          WEATHER, YRPLT,                                     &  !Input
          MSNOD, RMSNOD)                                         !Output
         
!-----------------------------------------------------------------------
      USE ModuleDefs
      
      IMPLICIT none
      SAVE 
!-----------------------------------------------------------------------
      CHARACTER*6   GENID
      CHARACTER*30 FILEIO
      
      INTEGER RUN, DYNAMIC, DAS, YRDOY, YR, DOY, YRPLT
      INTEGER DAP, TIMDIF, FDOY
      
      REAL DAYL, SRAD, TMAX, TMIN
      REAL MSNOD, RMSNOD, NMAX
      REAL, DIMENSION(70) :: QTL
      
      TYPE (ControlType) CONTROL
      TYPE (SwitchType) ISWITCH
      TYPE (WeatherType) WEATHER
      
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      RUN     = CONTROL % RUN
      DAS     = CONTROL % DAS
      YRDOY   = CONTROL % YRDOY
      
      
!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
!       Read Genetic input data
!-----------------------------------------------------------------------      
        CALL IPGENE(FILEIO, QTL, GENID)    
        
!***********************************************************************        
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN  
!------------------------------------------------------------------------
!     Set sowing/start day of year for flowering model to start
!     Initialize progress toward flowering, SumDRi, & Day of First Flower
!------------------------------------------------------------------------
        MSNOD   = 0.0
        RMSNOD  = 0.0
        DAYL    = 0.0
        SRAD    = 0.0
        TMAX    = 0.0
        TMIN    = 0.0
        FDOY    = 0
        GENID   = ''
        
!------------------------------------------------------------------------                      
!     Limit maximum rate for a genotype based on QTLs, (RFMAXi)
!------------------------------------------------------------------------          
      !  NMAX = 0.02798             &
      ! + 0.001064   * QTL(1)       &
      ! + 0.001249   * QTL(2)       &
      ! - 0.0003596  * QTL(3)       &
      ! + 0.0003961  * QTL(4)       &
      ! + 0.00006515 * QTL(5)       &
      ! + 0.0005581  * QTL(6)       &
      ! - 0.0004415  * QTL(7)       &
      ! - 0.0002479  * QTL(8)       &
      ! - 0.0006232  * QTL(9)       &
      ! - 0.0003084  * QTL(10)      &
      ! + 0.0006278  * QTL(11)      &
      ! - 0.0002375  * QTL(12)      
!-----------------------------------------------------------------------
!        Initialize output files 
!-----------------------------------------------------------------------
        CALL OPNAR(CONTROL, ISWITCH, &
            GENID,DAYL,SRAD,TMAX,TMIN,MSNOD,RMSNOD,DAP)        
!***********************************************************************        
!***********************************************************************
!     Daily Rate calculations
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. RATE) THEN           
!-----------------------------------------------------------------------
!       Read Weather data 
!----------------------------------------------------------------------- 
        CALL YR_DOY(YRPLT,YR,DOY)
        DAP = MAX (0, TIMDIF(YRPLT, YRDOY))
        
        SRAD = WEATHER%SRAD
        TMAX = WEATHER%TMAX
        TMIN = WEATHER%TMIN
        DAYL = WEATHER%DAYL
        
!------------------------------------------------------------------------                                  
!       The dynamic NAR model
!------------------------------------------------------------------------                                  
        RMSNOD =  -0.4101894                        &
          + 0.0029931  * SRAD                       &
          + 0.0148248  * DAYL                       &
          + 0.0026493  * TMAX                       &
          + 0.0154878  * TMIN                       &
          - 0.0058622  * QTL(1)                     &
          + 0.0046893  * QTL(2)                     &
          + 0.0038363  * QTL(3)                     &
          - 0.0047681  * QTL(4)                     &
          + 0.0019547  * QTL(5)                     &
          + 0.0026788  * QTL(6)                     &
          + 0.0014677  * QTL(7)                     &
          + 0.0021931  * QTL(8)                     &
          - 0.0026936  * QTL(9)                     &
          - 0.0006309  * QTL(10)                    &
          + 0.0002642  * QTL(11)                    
          
!***********************************************************************
!***********************************************************************
!     Daily integration
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN      
!------------------------------------------------------------------------
        
        IF (RMSNOD < 0.0) THEN
          RMSNOD = 0.0
        ENDIF
        
        !IF (RMSNOD > NMAX) THEN
        !  RMSNOD = NMAX
        !ENDIF
        
        MSNOD = MSNOD + RMSNOD
                        
!***********************************************************************
!***********************************************************************
!     OUTPUT/SEASEND
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
          CALL OPNAR(CONTROL, ISWITCH, &
              GENID,DAYL,SRAD,TMAX,TMIN,MSNOD,RMSNOD,DAP)
        
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF                

      RETURN
    END ! NAR-Module
    
!-----------------------------------------------------------------------
!     NAR-MODULE VARIABLES LIST
!-----------------------------------------------------------------------
! DAP        Number of days after planting (d)
! DAS        Days after start of simulation (d)
! DAYL       Day length on day of simulation (from sunrise to sunset) (hr)
! DOY        Current day of simulation (d)
! DYNAMIC    Module control variable
! GENID      Identifier for reading in the input file '.gen'
!             genotype (j) and environmental factors on the current environment & day (i) 
! SRAD       Solar radiation (MJ/m2-d)
! QTL(n)      Alleles at QTL(1) : QTL(n) in jth genotype 
! TIMDIF     Integer function which calculates the number of days between two Julian dates (da)
! TMAX       Maximum daily temperature (Celsius)
! TMIN       Minimum daily temperature (Celsius)
! YR         Year portion of date 
! YRDOY      Current day of simulation (YYYYDDD)
! YRPLT      Planting date (YYYYDDD)                                                             
