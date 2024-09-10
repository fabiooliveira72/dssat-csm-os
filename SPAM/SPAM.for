C=======================================================================
C  COPYRIGHT 1998-2023
C                      DSSAT Foundation
C                      University of Florida, Gainesville, Florida
C                      International Fertilizer Development Center
C
C  ALL RIGHTS RESERVED
C=======================================================================
C=======================================================================
C  SPAM, Subroutine
C  Calculates soil-plant-atmosphere interface energy balance components.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  11/09/2001 WMB/CHP Split WATBAL into WATBAL and SPAM.
C  02/06/2003 KJB/CHP Added KEP, EORATIO inputs from plant routines.
C  06/19/2003 CHP Added KTRANS - used instead of KEP in TRANS.
C  04/01/2004 CHP/US Added Penman - Meyer routine for potential ET
!  10/24/2005 CHP Put weather variables in constructed variable.
!  07/24/2006 CHP Use MSALB instead of SALB (includes mulch and soil
!                 water effects on albedo)
!  08/25/2006 CHP Add SALUS soil evaporation routine, triggered by new
!                 FILEX parameter MESEV
!  12/09/2008 CHP Remove METMP
!  10/16/2020 CHP Cumulative "soil" evaporation includes mulch and flood evap
!  01/26/2023 CHP Reduce compile warnings: add EXTERNAL stmts, remove 
!                 unused variables, shorten lines. 
C-----------------------------------------------------------------------
C  Called by: Main
C  Calls:     XTRACT, OPSPAM    (File SPSUBS.for)
C             PET     (File PET.for)
C             PSE     (File PET.for)
C             ROOTWU  (File ROOTWU.for)
C             SOILEV  (File SOILEV.for)
C             TRANS   (File TRANS.for)
C=======================================================================

      SUBROUTINE SPAM(CONTROL, ISWITCH,
     &    CANHT, EORATIO, KSEVAP, KTRANS, MULCH,          !Input
     &    PSTRES1, PORMIN, RLV, RWUMX, SOILPROP, SW,      !Input
     &    SWDELTS, UH2O, WEATHER, WINF, XHLAI, XLAI,      !Input
     &    FLOODWAT, SWDELTU,                              !I/O
     &    EO, EOP, EOS, EP, ES, RWU, SRFTEMP, ST,         !Output
     &    SWDELTX, TRWU, TRWUP, UPFLOW)                   !Output

!-----------------------------------------------------------------------
      USE ModuleDefs
      USE ModuleData
      USE FloodModule
!*********************************************************************** 
!     CSM_Reverse_ST_Modeling by FO
!     CROP2ML - DEFINE MODULES
!***********************************************************************      
!     BIOMA-Parton
      USE Soiltemperatureswatmod
      USE Surfacepartonsoilswatcmod
!-----------------------------------------------------------------------
!     BIOMA-SWAT
      USE Soiltemperatureswatmod_SW
      USE Surfaceswatsoilswatcmod_SW
!-----------------------------------------------------------------------
!     C2ML DSSAT-EPIC
!-----------------------------------------------------------------------
!     C2ML DSSAT
!-----------------------------------------------------------------------
!     MONICA
      USE Soiltemperaturecompmod
!-----------------------------------------------------------------------
!     Simplace
      USE Snowcovercalculatormod
      USE Stmpsimcalculatormod
!-----------------------------------------------------------------------
!     SIRIUS-Quality
      USE Soiltemperaturemod_SIRIUS
!-----------------------------------------------------------------------
!     STICS      
      USE Soil_tempmod
!-----------------------------------------------------------------------
!     CSM_Reverse_ST_Modeling by FO
!     END
!***********************************************************************

      IMPLICIT NONE
      EXTERNAL ETPHOT, STEMP_EPIC, STEMP, ROOTWU, SOILEV, TRANS
      EXTERNAL MULCH_EVAP, OPSPAM, PET, PSE, FLOOD_EVAP, ESR_SOILEVAP
      EXTERNAL XTRACT
      SAVE

      CHARACTER*1  IDETW, ISWWAT
      CHARACTER*1  MEEVP, MEINF, MEPHO, MESEV, METMP
      CHARACTER*2  CROP
      CHARACTER*6, PARAMETER :: ERRKEY = "SPAM  "
!      CHARACTER*78 MSG(2)

      INTEGER DYNAMIC, L, NLAYR, YRDOY, YEAR, DOY

      REAL CANHT, CO2, SRAD, TAVG,
     &    TMAX, TMIN, WINDSP, XHLAI, XLAI
      REAL CEF, CEM, CEO, CEP, CES, CET, CEVAP
      REAL EF, EM, EO, EP, ES, ET, EVAP
      REAL TRWU, TRWUP, U
      REAL EOS, EOP, WINF, MSALB, ET_ALB
      REAL XLAT, TAV, TAMP, SRFTEMP
      REAL EORATIO, KSEVAP, KTRANS

      REAL DLAYR(NL), DUL(NL), LL(NL), RLV(NL), RWU(NL),
     &    SAT(NL), ST(NL), SW(NL), SW_AVAIL(NL), !SWAD(NL),
     &    SWDELTS(NL), SWDELTU(NL), SWDELTX(NL), UPFLOW(NL)
      REAL ES_LYR(NL)

!     Root water uptake computed by some plant routines (optional)
      REAL UH2O(NL)

!     Species-dependant variables imported from PLANT module:
      REAL PORMIN, RWUMX

!     Flood management variables:
      REAL FLOOD, EOS_SOIL

!     P Stress on photosynthesis
      REAL PSTRES1
!     Hourly transpiration for MEEVP=H
      REAL, DIMENSION(TS)    :: ET0

!*********************************************************************** 
!     CSM_Reverse_ST_Modeling by FO
!     CROP2ML - CONTROL VARIABLES
!***********************************************************************
!     BIOMA-Parton
      REAL LagCoefficient, SoilProfileDepth
      REAL SurfaceTemperatureMinimum
      REAL SurfaceTemperatureMaximum
      !REAL,DIMENSION(0:INPITF % NLAYR):: THICKNESS
      REAL,DIMENSION(0:NL):: THICKNESS
      INTEGER I
      REAL BIOMAS
!-----------------------------------------------------------------------
!     BIOMA-SWAT
      REAL HeatCapacity
      REAL WaterEquivalentOfSnowPack
!-----------------------------------------------------------------------      
!     C2ML DSSAT-EPIC
      INTEGER NDays
      INTEGER, DIMENSION(30) :: WetDay
      REAL CUMDPT, X2_PREV, TDL
      REAL, DIMENSION(NL) :: DSMID
      REAL, DIMENSION(5) :: TMA
      REAL SNOW, MULCHMASS, DEPIR
!-----------------------------------------------------------------------
!     C2ML DSSAT
      REAL HDAY, ATOT
!-----------------------------------------------------------------------
!     MONICA
      REAL timeStep, baseTemp
      REAL initialSurfaceTemp, densityAir, densityHumus
      REAL specificHeatCapacityHumus, densityWater
      REAL specificHeatCapacityAir
      REAL specificHeatCapacityWater, quartzRawDensity
      REAL specificHeatCapacityQuartz, nTau
      INTEGER noOfTempLayers, noOfSoilLayers, noOfTempLayersPlus1
      INTEGER LTK, TMPLY
      REAL, DIMENSION(:), ALLOCATABLE :: layerThickness
      REAL, DIMENSION(:), ALLOCATABLE :: soilBulkDensity
      REAL, DIMENSION(:), ALLOCATABLE :: saturation
      REAL, DIMENSION(:), ALLOCATABLE :: soilOrganicMatter
      REAL, DIMENSION(:), ALLOCATABLE :: soilMoistureConst
      REAL, DIMENSION(:), ALLOCATABLE :: V
      REAL, DIMENSION(:), ALLOCATABLE :: B_monica
      REAL, DIMENSION(:), ALLOCATABLE :: volumeMatrix
      REAL, DIMENSION(:), ALLOCATABLE :: volumeMatrixOld
      REAL, DIMENSION(:), ALLOCATABLE :: matrixPrimaryDiagonal
      REAL, DIMENSION(:), ALLOCATABLE :: matrixSecondaryDiagonal
      REAL, DIMENSION(:), ALLOCATABLE :: heatConductivity
      REAL, DIMENSION(:), ALLOCATABLE :: heatConductivityMean
      REAL, DIMENSION(:), ALLOCATABLE :: heatCapacity_monica
      REAL, DIMENSION(:), ALLOCATABLE :: solution 
      REAL, DIMENSION(:), ALLOCATABLE :: matrixDiagonal
      REAL, DIMENSION(:), ALLOCATABLE :: matrixLowerTriangle
      REAL, DIMENSION(:), ALLOCATABLE :: heatFlow
      REAL, DIMENSION(:), ALLOCATABLE :: soilTemperature
      REAL dampingFactor, soilCoverage, auxstemp
      REAL soilSurfaceTemperatureBelowSnow
      LOGICAL hasSnowCover
!-----------------------------------------------------------------------
!     Simplace
      INTEGER AgeOfSnow, rAgeOfSnowRate, cInitialAgeOfSnow
      REAL cFirstDayMeanTemp
      REAL cAverageBulkDensity, iSoilWaterContent
      REAL rSnowWaterContentRate, cInitialSnowWaterContent
      REAL rSoilSurfaceTemperatureRate
      REAL SnowIsolationIndex
      REAL cSnowIsolationFactorA, cSnowIsolationFactorB
      REAL pInternalAlbedo, iSoilSurfaceTemperature
      REAL cCarbonContent, cDampingDepth
      REAL TSW, SABDM, CUMDEP
      REAL , ALLOCATABLE, DIMENSION(: ):: SoilTempArray
      REAL , ALLOCATABLE, DIMENSION(: ):: rSoilTempArrayRate
      REAL , ALLOCATABLE, DIMENSION(: ):: pSoilLayerDepth
      REAL , ALLOCATABLE, DIMENSION(: ) :: cSoilLayerDepth
!-----------------------------------------------------------------------
!     SIRIUS-Quality
      REAL deepLayerT, deepLayerT_t1, lambda_, a,b,c
      REAL maxTSoil, minTSoil
      REAL heatFlux
      REAL DAYLD
      REAL, DIMENSION(24) :: hourlySoilT
!-----------------------------------------------------------------------
!     STICS
      REAL air_temp_day1, max_canopy_temp, min_canopy_temp
      REAL prev_canopy_temp, temp_amp
      REAL , ALLOCATABLE, DIMENSION(: ):: prev_temp_profile
      REAL , ALLOCATABLE, DIMENSION(: ):: temp_profile
      REAL , ALLOCATABLE, DIMENSION(: ):: layer_temp
      INTEGER, DIMENSION(0:NL):: LTHICK
!-----------------------------------------------------------------------
!     CSM_Reverse_ST_Modeling by FO
!     END
!***********************************************************************

!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP
      TYPE (SwitchType)  ISWITCH
      TYPE (FloodWatType)FLOODWAT
      TYPE (MulchType)   MULCH
      TYPE (WeatherType) WEATHER

!     Transfer values from constructed data types into local variables.
      CROP    = CONTROL % CROP
      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY

      DLAYR  = SOILPROP % DLAYR
      DUL    = SOILPROP % DUL
      LL     = SOILPROP % LL
      MSALB  = SOILPROP % MSALB
      NLAYR  = SOILPROP % NLAYR
      SAT    = SOILPROP % SAT
      U      = SOILPROP % U

      ISWWAT = ISWITCH % ISWWAT
      IDETW  = ISWITCH % IDETW
      MEEVP  = ISWITCH % MEEVP
      MEINF  = ISWITCH % MEINF
      MEPHO  = ISWITCH % MEPHO
      METMP  = ISWITCH % METMP
      MESEV  = ISWITCH % MESEV

      FLOOD  = FLOODWAT % FLOOD

      CO2    = WEATHER % CO2
      SRAD   = WEATHER % SRAD
      TAMP   = WEATHER % TAMP
      TAV    = WEATHER % TAV
      TAVG   = WEATHER % TAVG
      TMAX   = WEATHER % TMAX
      TMIN   = WEATHER % TMIN
      WINDSP = WEATHER % WINDSP
      XLAT   = WEATHER % XLAT

      CALL YR_DOY(YRDOY, YEAR, DOY)
!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
      IF (MEPHO .EQ. 'L' .OR. MEEVP .EQ. 'Z') THEN
        CALL ETPHOT(CONTROL, ISWITCH,
     &    PORMIN, PSTRES1, RLV, RWUMX, SOILPROP, ST, SW,  !Input
     &    WEATHER, XLAI,                                  !Input
     &    EOP, EP, ES, RWU, TRWUP)                        !Output
      ENDIF

      !Initialize ASCE dual KC ET variables (KRT)
      CALL PUT('SPAM', 'REFET', -99.0)
      CALL PUT('SPAM', 'KCB', -99.0)
      CALL PUT('SPAM', 'KE', -99.0)
      CALL PUT('SPAM', 'KC', -99.0)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!*********************************************************************** 
!     CSM_Reverse_ST_Modeling by FO
!     CROP2ML - INTERFACE INDIVIDUAL ATTRIBUTIONS
!***********************************************************************
!     BIOMA-Parton
      !SoilProfileDepth = INPITF % SLDP / 100
      SoilProfileDepth = SOILPROP % DS(NLAYR) / 100
      !DO I = 0, INPITF % NLAYR
      THICKNESS = 0.0
      DO I = 1, NLAYR
            THICKNESS(I) = SOILPROP % DLAYR(I) / 100
      ENDDO
      LagCoefficient = 0.8
!-----------------------------------------------------------------------
!     BIOMA-SWAT
      !SoilProfileDepth = INPITF % SLDP / 100
      SoilProfileDepth = SOILPROP % DS(NLAYR) / 100
      !DO I = 0, INPITF % NLAYR
      THICKNESS = 0.0
      DO I = 1, NLAYR
            THICKNESS(I) = SOILPROP % DLAYR(I) / 100
      ENDDO
      LagCoefficient = 0.8
!-----------------------------------------------------------------------
!     C2ML DSSAT-EPIC
!-----------------------------------------------------------------------
!     C2ML DSSAT
!-----------------------------------------------------------------------
!     MONICA
      timeStep = 1.0
      soilCoverage = 1 - EXP(-0.5 * XHLAI);
      baseTemp = 9.5
      dampingFactor = 0.8
      initialSurfaceTemp = 10.0
      densityAir = 1.25
      specificHeatCapacityAir = 1005
      densityHumus = 1300
      specificHeatCapacityHumus = 1920
      densityWater = 1000
      specificHeatCapacityWater = 4192
      quartzRawDensity = 2650
      specificHeatCapacityQuartz = 750
      nTau = 0.65
      noOfSoilLayers = (SOILPROP % DS(NLAYR)/5)
      noOfTempLayers = noOfSoilLayers + 2
      noOfTempLayersPlus1 = 45
      hasSnowCover = .FALSE.
      
      IF(.NOT. allocated(layerThickness)) THEN
          allocate(layerThickness(noOfTempLayers))
          allocate(soilBulkDensity(noOfSoilLayers))
          allocate(saturation(noOfSoilLayers))
          allocate(soilOrganicMatter(noOfSoilLayers))
          allocate(soilMoistureConst(noOfSoilLayers))
          allocate(V(noOfTempLayers))
          allocate(volumeMatrix(noOfTempLayers))
          allocate(volumeMatrixOld(noOfTempLayers))
          allocate(B_monica(noOfTempLayers))
          allocate(matrixPrimaryDiagonal(noOfTempLayers))
          allocate(matrixSecondaryDiagonal(noOfTempLayersPlus1))
          allocate(heatConductivity(noOfTempLayers))
          allocate(heatConductivityMean(noOfTempLayers))
          allocate(heatCapacity_monica(noOfTempLayers))
          allocate(solution(noOfTempLayers))
          allocate(matrixDiagonal(noOfTempLayers))
          allocate(matrixLowerTriangle(noOfTempLayers))
          allocate(heatFlow(noOfTempLayers))
          allocate(soilTemperature(noOfTempLayers))
        ENDIF

        ! Thickness is m. However every layer is 5cm
        layerThickness = 0.05
        
        TMPLY= 1
        DO I = 1, SOILPROP % NLAYR
          LTK = SOILPROP % DLAYR(I) / 5
          DO WHILE (LTK > 0 .AND. TMPLY <= noOfSoilLayers)
            saturation(TMPLY) = SOILPROP % SAT(I)
            !Convert from g/cm3 to kg/m3
            soilBulkDensity(TMPLY) = SOILPROP % BD(I) * 1000
            soilOrganicMatter(TMPLY) =(SOILPROP % OC(I)/0.57)/100
            ! DSSAT Calculates soilMoistureConst as SW.
!            soilMoistureConst(TMPLY)= SOILPROP % LL(I) + 
!     &      INPITF % AWC * (SOILPROP % DUL (I) - SOILPROP % LL(I))
            soilMoistureConst(TMPLY)= SW(I)
            
            LTK = LTK - 1
            TMPLY = TMPLY + 1
          ENDDO
        ENDDO
!-----------------------------------------------------------------------
!     Simplace
      DO I = 1, NLAYR
        TSW = TSW + 
     &       SW(I) * SOIlPROP % DLAYR(I)
      ENDDO
      IF(.NOT. allocated(cSoilLayerDepth)) THEN
        allocate(cSoilLayerDepth(NLAYR))
      ENDIF
      iSoilWatercontent =  TSW * 10 ! cm to mm
      cCarbonContent = SOILPROP % OC(1)
      cSoilLayerDepth = SOILPROP % DS * 0.01 ! cm to m
      cDampingDepth = 6.0
      AgeOfSnow = 0
      cFirstDayMeanTemp = TAV
      cInitialAgeOfSnow = 0
      cInitialSnowWaterContent = 0.0
      cSnowIsolationFactorA = 0.47
      cSnowIsolationFactorB = 0.62
      pInternalAlbedo = SOILPROP % MSALB
      ! Calculate average bulk density
      SABDM = 0.0
      CUMDEP = 0.0
      DO I = 1, NLAYR
          SABDM = SABDM + SOILPROP % BD(I) * SOILPROP % DLAYR(I)
          CUMDEP = CUMDEP + SOILPROP % DLAYR(I)
      ENDDO
      SABDM = SABDM / CUMDEP
!-----------------------------------------------------------------------
!     SIRIUS-Quality
      lambda_ = 2.454
      hourlySoilT = 0.0
      a = 0.0
      b = 0.0
      c = 0.0
      heatFlux = 0.0
      deepLayerT = TAV
!-----------------------------------------------------------------------
!     STICS
!-----------------------------------------------------------------------
!     CSM_Reverse_ST_Modeling by FO
!     END
!***********************************************************************

      EF   = 0.0; CEF  = 0.0
      EM   = 0.0; CEM  = 0.0
      EO   = 0.0; CEO  = 0.0
      EP   = 0.0; EOP  = 0.0; CEP  = 0.0
      ES   = 0.0; EOS  = 0.0; CES  = 0.0
      ET   = 0.0; CET  = 0.0
      EVAP = 0.0; CEVAP =0.0
      ES_LYR = 0.0
      SWDELTX = 0.0
      TRWU = 0.0
      XHLAI = 0.0
      ET0 = 0.0

!     ---------------------------------------------------------
      IF (MEEVP .NE.'Z') THEN   !LPM 02dec14 use values from ETPHOT
        SELECT CASE (METMP)
!*********************************************************************** 
!     CSM_Reverse_ST_Modeling by FO
!     CROP2ML - CONTROL VARIABLES
!     Select case METMP:
!         F - C2ML-BIOMA-Parton
!         G - C2ML-Bioma-SWAT
!         H - C2ML-DSSAT Epic
!         I - C2Ml-DSSAT
!         J - C2ML-MONICA
!         K - C2ML-Simplace
!         L - C2ML-Sirius-Quality
!         M - C2ML-Stics
!***********************************************************************
        CASE('F') ! BIOMA-Parton
            WRITE(*,*) 'BIOMA-Parton running...'
            CALL init_soiltemperatureswat(
!     &            INPITF % SWLD, ! VolumetricWaterContent
     &            SW, ! VolumetricWaterContent
!     &            THICKNESS(1:INPITF % NLAYR), ! LayerThickness
     &            THICKNESS(1:NLAYR), ! LayerThickness
     &            LagCoefficient, 
!     &            INPITF % TAV, ! AirTemperatureAnnualAverage
     &            WEATHER % TAV, ! AirTemperatureAnnualAverage
!     &            INPITF % SLBDM, ! BulkDensity 
     &            SOILPROP % BD, ! BulkDensity 
     &            SoilProfileDepth, 
!     &            OUTITF % TSLD(1:INPITF % NLAYR))! SoilTemperatureByLayers
     &            ST(1:NLAYR))! SoilTemperatureByLayers

             CALL OPSTEMP(CONTROL, ISWITCH, DOY, SRFTEMP, ST, TAV, TAMP)
             CALL OPSTEMP_AMEI_ST(CONTROL, ISWITCH,DOY,SRFTEMP,ST,SW)
             CALL OPSTEMP_AMEI_CL(CONTROL, ISWITCH,EOS,ES,EO,ET)
!-----------------------------------------------------------------------
        CASE('G') ! BIOMA-SWAT
            WRITE(*,*) 'BIOMA-SWAT running...'
            CALL init_soiltemperatureswat_SW(
!     &            INPITF % SWLD, ! VolumetricWaterContent
     &            SW, ! VolumetricWaterContent
!     &            THICKNESS(1:INPITF % NLAYR), ! LayerThickness
     &            THICKNESS(1:NLAYR), ! LayerThickness
     &            LagCoefficient, 
!     &            INPITF % TAV, ! AirTemperatureAnnualAverage
     &            WEATHER % TAV, ! AirTemperatureAnnualAverage
!     &            INPITF % SLBDM, ! BulkDensity 
     &            SOILPROP % BD, ! BulkDensity 
     &            SoilProfileDepth, 
!     &            OUTITF % TSLD(1:INPITF % NLAYR))! SoilTemperatureByLayers
     &            ST(1:NLAYR))! SoilTemperatureByLayers

             CALL OPSTEMP(CONTROL, ISWITCH, DOY, SRFTEMP, ST, TAV, TAMP)
             CALL OPSTEMP_AMEI_ST(CONTROL, ISWITCH,DOY,SRFTEMP,ST,SW)
             CALL OPSTEMP_AMEI_CL(CONTROL, ISWITCH,EOS,ES,EO,ET)
!-----------------------------------------------------------------------
        CASE('H') ! C2ML DSSAT-EPIC
            WRITE(*,*) 'C2ML DSSAT-EPIC running...'
            CALL GET('PLANT','BIOMAS',BIOMAS)      !kg/ha
            CALL GET('ORGC' ,'MULCHMASS',MULCHMASS)   !kg/ha
            CALL GET('WATER','SNOW',SNOW)       !mm
            CALL GET('MGMT','DEPIR',DEPIR)      !mm

            CALL init_stemp_epic(NL, ISWWAT, 
!     &            INPITF % SLBDM, INPITF % THICK(1:INPITF % NLAYR), 
     &            SOILPROP % BD, SOILPROP % DLAYR,
!     &            INPITF % SLLB(1:INPITF % NLAYR), INPITF % SLDUL, 
     &            SOILPROP % DS, SOILPROP % DUL,
!     &            INPITF % SLLL, INPITF % NLAYR, 
     &            SOILPROP % LL, NLAYR, 
!     &            INPITF % TAMP, 
     &            WEATHER % TAMP, 
!     &            INPITF % RAIN, INPITF % SWLD, INPITF % T2M, 
     &            WEATHER % RAIN, SW, WEATHER % TAVG, 
!     &            INPITF % TMAX, INPITF % TMIN, INPITF % TAV, 
     &            WEATHER % TMAX, WEATHER % TMIN, WEATHER % TAV, 
!     &            INPITF % IRVAL, 
     &            DEPIR, 
!     &            INPITF % CWAD, 
     &            BIOMAS, 
!     &            INPITF % MLTHK, 
     &            MULCHMASS, 
!     &            INPITF % SNOW, 
     &            SNOW, 
     &            CUMDPT, 
     &            DSMID, 
     &            TDL,
     &            TMA, 
     &            NDays, 
     &            WetDay, 
     &            X2_PREV, 
!     &            OUTITF % TSLD(0), OUTITF % TSLD(1:INPITF % NLAYR))
     &            SRFTEMP, ST)
            CALL OPSTEMP(CONTROL, ISWITCH, DOY, SRFTEMP, ST, TAV, TAMP)
            CALL OPSTEMP_AMEI_ST(CONTROL, ISWITCH,DOY,SRFTEMP,ST,SW)
            CALL OPSTEMP_AMEI_CL(CONTROL, ISWITCH,EOS,ES,EO,ET)
!-----------------------------------------------------------------------
        CASE ('I') ! C2ML DSSAT
            WRITE(*,*) 'C2ML DSSAT running...'
            CALL init_stemp(NL, ISWWAT,                                    
!     &            INPITF % SLBDM,                                      
     &            SOILPROP % BD,
!     &            INPITF % THICK(1:INPITF % NLAYR),                    
     &            SOILPROP % DLAYR,
!     &            INPITF % SLLB(1:INPITF % NLAYR),                       
     &            SOILPROP % DS,     
!     &            INPITF % SLDUL, INPITF % SLLL,  
     &            SOILPROP % DUL, SOILPROP % LL,      
!     &            INPITF % NLAYR, INPITF % SALB,                       
     &            SOILPROP % NLAYR, SOILPROP % MSALB, 
!     &            INPITF % SRAD, INPITF % SWLD, INPITF % T2M, 
     &            SRAD, SW, TAVG,          
!     &            INPITF % TMAX, INPITF % XLAT, INPITF % TAV,    
     &            TMAX, XLAT, TAV,             
!     &            INPITF % TAMP, 
     &            TAMP,     
     &            DOY,                                                 
     &            CUMDPT,                                              
     &            DSMID,                                               
     &            TDL,                                                 
     &            TMA,                                                 
     &            ATOT,                                                
!     &            OUTITF % TSLD(0), OUTITF % TSLD(1:INPITF % NLAYR),   
     &            SRFTEMP, ST,
     &            HDAY)

             CALL OPSTEMP(CONTROL, ISWITCH, DOY, SRFTEMP, ST, TAV, TAMP)
             CALL OPSTEMP_AMEI_ST(CONTROL, ISWITCH,DOY,SRFTEMP,ST,SW)
             CALL OPSTEMP_AMEI_CL(CONTROL, ISWITCH,EOS,ES,EO,ET)
!-----------------------------------------------------------------------
        CASE ('J') ! MONICA
            WRITE(*,*) 'MONICA running...'
            CALL init_soiltemperature(
     &        noOfSoilLayers, 
     &        noOfTempLayers, 
     &        noOfTempLayersPlus1, 
     &        timeStep, 
     &        soilMoistureConst, 
     &        baseTemp, 
     &        initialSurfaceTemp, 
     &        densityAir, 
     &        specificHeatCapacityAir, 
     &        densityHumus, 
     &        specificHeatCapacityHumus, 
     &        densityWater, 
     &        specificHeatCapacityWater, 
     &        quartzRawDensity, 
     &        specificHeatCapacityQuartz, 
     &        nTau, 
     &        layerThickness, 
     &        soilBulkDensity, 
     &        saturation, 
     &        soilOrganicMatter, 
     &        SRFTEMP, !OUTITF % TSLD(0), ! soilSurfaceTemperature, 
     &        soilTemperature, 
     &        V, 
     &        B_monica, 
     &        volumeMatrix, 
     &        volumeMatrixOld, 
     &        matrixPrimaryDiagonal, 
     &        matrixSecondaryDiagonal, 
     &        heatConductivity, 
     &        heatConductivityMean, 
     &        heatCapacity_monica, 
     &        solution, 
     &        matrixDiagonal, 
     &        matrixLowerTriangle, 
     &        heatFlow)
            CALL OPSTEMP(CONTROL, ISWITCH, DOY, SRFTEMP, ST, TAV, TAMP)
            CALL OPSTEMP_AMEI_ST(CONTROL, ISWITCH,DOY,SRFTEMP,ST,SW)
            CALL OPSTEMP_AMEI_CL(CONTROL, ISWITCH,EOS,ES,EO,ET)
!-----------------------------------------------------------------------
        CASE ('K') ! Simplace
            WRITE(*,*) 'Simplace running...'
            CALL GET('ORGC' ,'MULCHMASS',MULCHMASS)   !kg/ha
            CALL GET('WATER','SNOW',SNOW)       !mm  
            CALL init_snowcovercalculator(
     &            cCarbonContent,
     &            cInitialAgeOfSnow, 
     &            cInitialSnowWaterContent, 
     &            SOILPROP % MSALB, ! Albedo, 
     &            cSnowIsolationFactorA, 
     &            cSnowIsolationFactorB, 
     &            WEATHER % TMAX, ! iTempMax, 
     &            WEATHER % TMIN, ! iTempMin, 
     &            WEATHER % SRAD, ! iRadiation, 
     &            WEATHER % RAIN, ! iRAIN, 
     &            MULCHMASS,! iCropResidues,
     &            EOS, ! iPotentialSoilEvaporation
     &            XHLAI, ! iLeafAreaIndex
     &            SoilTempArray,
     &            pInternalAlbedo, 
     &            SNOW, ! SnowWaterContent
     &            SRFTEMP, ! SoilSurfaceTemperature
     &            AgeOfSnow)
     
            CALL init_stmpsimcalculator(
     &            cSoilLayerDepth, 
     &            cFirstDayMeanTemp, 
     &            WEATHER % TAV, ! cAVT: cAverageGroundTemperature
     &            SABDM, ! cABD: cAverageBulkDensity
     &            cDampingDepth, 
     &            iSoilWaterContent, 
     &            SRFTEMP, ! SoilSurfaceTemperature
     &            SoilTempArray,
     &            rSoilTempArrayRate,
     &            pSoilLayerDepth)
            CALL OPSTEMP(CONTROL, ISWITCH, DOY, SRFTEMP, ST, TAV, TAMP)
            CALL OPSTEMP_AMEI_ST(CONTROL, ISWITCH,DOY,SRFTEMP,ST,SW)
            CALL OPSTEMP_AMEI_CL(CONTROL, ISWITCH,EOS,ES,EO,ET)
!-----------------------------------------------------------------------
        CASE ('L') ! SIRIUS-Quality
            WRITE(*,*) 'SIRIUS-Quality running...'
            CALL init_calculatesoiltemperature(
!     &            INPITF % T2M, ! meanTAir 
     &            WEATHER % TAVG, ! meanTAir 
!     &            INPITF % TMIN, ! minTAir
     &            WEATHER % TMIN, ! minTAir
     &            lambda_, 
!     &            INPITF % TAV, ! meanAnnualAirTemp
     &            WEATHER % TAV, ! meanAnnualAirTemp
!     &            INPITF % TMAX, ! maxTAir
     &            WEATHER % TMAX, ! maxTAir
     &            deepLayerT)

            CALL OPSTEMP(CONTROL, ISWITCH, DOY, SRFTEMP, ST, TAV, TAMP)
            CALL OPSTEMP_AMEI_ST(CONTROL, ISWITCH,DOY,SRFTEMP,ST,SW)
            CALL OPSTEMP_AMEI_CL(CONTROL, ISWITCH,EOS,ES,EO,ET)
!-----------------------------------------------------------------------
        CASE ('M') ! STICS
            WRITE(*,*) 'STICS running...'
!            LTHICK = INT(INPITF % THICK)
            LTHICK(1:NL) = INT(SOILPROP % DLAYR)

            CALL init_temp_profile(
!     &            INPITF % TMIN, !min_air_temp
     &            WEATHER % TMIN, !min_air_temp
!     &            INPITF % T2M, !air_temp_day1
     &            WEATHER % TAVG, !air_temp_day1
!     &            LTHICK(1:INPITF % NLAYR), !layer_thick 
     &            LTHICK(1:NLAYR), !layer_thick 
     &            temp_amp, 
     &            prev_temp_profile, 
     &            prev_canopy_temp)

            CALL OPSTEMP(CONTROL, ISWITCH, DOY, SRFTEMP, ST, TAV, TAMP)
            CALL OPSTEMP_AMEI_ST(CONTROL, ISWITCH,DOY,SRFTEMP,ST,SW)
            CALL OPSTEMP_AMEI_CL(CONTROL, ISWITCH,EOS,ES,EO,ET)
!-----------------------------------------------------------------------
!     CSM_Reverse_ST_Modeling by FO
!     END
!***********************************************************************
        CASE ('E')    !EPIC soil temperature routine
          CALL STEMP_EPIC(CONTROL, ISWITCH,
     &      SOILPROP, SW, TAVG, TMAX, TMIN, TAV, WEATHER,    !Input
     &      SRFTEMP, ST)                                     !Output
        CASE DEFAULT  !DSSAT soil temperature
          CALL STEMP(CONTROL, ISWITCH,
     &      SOILPROP, SRAD, SW, TAVG, TMAX, XLAT, TAV, TAMP, !Input
     &      SRFTEMP, ST)                                     !Output
        END SELECT
      ENDIF
!     ---------------------------------------------------------
      IF (MEEVP .NE. 'Z') THEN
        CALL ROOTWU(SEASINIT,
     &      DLAYR, LL, NLAYR, PORMIN, RLV, RWUMX, SAT, SW,!Input
     &      RWU, TRWUP)                           !Output

!       Initialize soil evaporation variables
        SELECT CASE (MESEV)
!     ----------------------------
        CASE ('R')  !Original soil evaporation routine
          CALL SOILEV(SEASINIT,
     &      DLAYR, DUL, EOS, LL, SW, SW_AVAIL(1),         !Input
     &      U, WINF,                                      !Input
     &      ES)                                           !Output
!     ----------------------------
        END SELECT

!       Initialize plant transpiration variables
        CALL TRANS(DYNAMIC, MEEVP,
     &    CO2, CROP, EO, ET0, EVAP, KTRANS,               !Input
     &    WINDSP, XHLAI, WEATHER,                         !Input
     &    EOP)                                            !Output
      ENDIF

      CALL MULCH_EVAP(DYNAMIC, MULCH, EOS, EM)

!     ---------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
        IF (MEPHO .EQ. 'L' .OR. MEEVP .EQ. 'Z') THEN
          CALL ETPHOT(CONTROL, ISWITCH,
     &    PORMIN, PSTRES1, RLV, RWUMX, SOILPROP, ST, SW,  !Input
     &    WEATHER, XLAI,                                  !Input
     &    EOP, EP, ES, RWU, TRWUP)                        !Output
        ENDIF
      ENDIF

!     Call OPSPAM to open and write headers to output file
      IF (IDETW .EQ. 'Y') THEN
        CALL OPSPAM(CONTROL, ISWITCH, FLOODWAT, TRWU,
     &    CEF, CEM, CEO, CEP, CES, CET, CEVAP, EF, EM,
     &    EO, EOP, EOS, EP, ES, ET, TMAX, TMIN, TRWUP, SRAD,
     &    ES_LYR, SOILPROP)
      ENDIF

!     Transfer data to storage routine
      CALL PUT('SPAM', 'CEF', CEF)
      CALL PUT('SPAM', 'CEM', CEM)
      CALL PUT('SPAM', 'CEO', CEO)
      CALL PUT('SPAM', 'CEP', CEP)
      CALL PUT('SPAM', 'CES', CES)
      CALL PUT('SPAM', 'CET', CET)
      CALL PUT('SPAM', 'CEVAP',CEVAP)
      CALL PUT('SPAM', 'EF',  EF)
      CALL PUT('SPAM', 'EM',  EM)
      CALL PUT('SPAM', 'EO',  EO)
      CALL PUT('SPAM', 'EP',  EP)
      CALL PUT('SPAM', 'ES',  ES)
      CALL PUT('SPAM', 'ET',  ET)

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
      SWDELTX = 0.0
!     ---------------------------------------------------------
      IF (MEEVP .NE.'Z') THEN  !LPM 02dec14 use values from ETPHOT
        SELECT CASE (METMP)
!*********************************************************************** 
!     CSM_Reverse_ST_Modeling by FO
!     CROP2ML - RATE CALCULATIONS
!***********************************************************************
        CASE('F') ! BIOMA-Parton
            CALL GET('PLANT','BIOMAS',BIOMAS)      !kg/ha

            CALL model_surfacepartonsoilswatc(
!     &            INPITF % DAYLD, ! DayLength
     &            WEATHER % DAYL, ! DayLength
!     &            INPITF % SRAD, ! GlobalSolarRadiation
     &            WEATHER % SRAD, ! GlobalSolarRadiation
!     &            INPITF % CWAD, ! AboveGroundBiomass
     &            BIOMAS, ! AboveGroundBiomass
!     &            INPITF % TMIN, ! AirTemperatureMinimum
     &            WEATHER % TMIN, ! AirTemperatureMinimum
!     &            INPITF % TMAX, ! AirTemperatureMaximum
     &            WEATHER % TMAX, ! AirTemperatureMaximum
!     &            THICKNESS(1:INPITF % NLAYR), ! LayerThickness
     &            THICKNESS(1:NLAYR), ! LayerThickness
!     &            INPITF % SLBDM, ! BulkDensity
     &            SOILPROP % BD, ! BulkDensity 
     &            SoilProfileDepth, 
!     &            INPITF % TAV, ! AirTemperatureAnnualAverage
     &            WEATHER % TAV, ! AirTemperatureAnnualAverage
!     &            INPITF % SWLD, ! VolumetricWaterContent, 
     &            SW, ! VolumetricWaterContent, 
     &            LagCoefficient, 
!     &            OUTITF % TSLN(0), ! SurfaceTemperatureMinimum
     &            SurfaceTemperatureMinimum,
!     &            OUTITF % TSLX(0), ! SurfaceTemperatureMaximum
     &            SurfaceTemperatureMaximum,
!     &            OUTITF % TSLD(0), ! SurfaceSoilTemperature
     &            SRFTEMP, ! SurfaceSoilTemperature
!     &            OUTITF % TSLD(1:INPITF % NLAYR))! SoilTemperatureByLayers
     &            ST(1:NLAYR))! SoilTemperatureByLayers
!-----------------------------------------------------------------------     
        CASE('G') ! BIOMA-SWAT
            CALL GET('PLANT','BIOMAS',BIOMAS)      !kg/ha
            CALL GET('WATER','SNOW', WaterEquivalentOfSnowPack)       !mm

            CALL model_surfaceswatsoilswatc_SW(
!     &            INPITF % TMAX, ! AirTemperatureMaximum
     &            WEATHER % TMAX, ! AirTemperatureMaximum
!     &            INPITF % TMIN, ! AirTemperatureMinimum
     &            WEATHER % TMIN, ! AirTemperatureMinimum
!     &            INPITF % SRAD, ! GlobalSolarRadiation
     &            WEATHER % SRAD, ! GlobalSolarRadiation
!     &            INPITF % CWAD, ! AboveGroundBiomass
     &            BIOMAS, ! AboveGroundBiomass
!     &            INPITF % SNOW, ! WaterEquivalentOfSnowPack 
     &            WaterEquivalentOfSnowPack,
!     &            INPITF % SALB, ! Albedo
     &            SOILPROP % MSALB, ! Albedo
!     &            INPITF % SLBDM, ! BulkDensity
     &            SOILPROP % BD, ! BulkDensity 
!     &            INPITF % TAV, ! AirTemperatureAnnualAverage
     &            WEATHER % TAV, ! AirTemperatureAnnualAverage
!     &            INPITF % SWLD, ! VolumetricWaterContent, 
     &            SW, ! VolumetricWaterContent, 
     &            SoilProfileDepth, 
     &            LagCoefficient, 
!     &            THICKNESS(1:INPITF % NLAYR), ! LayerThickness
     &            THICKNESS(1:NLAYR), ! LayerThickness
!     &            OUTITF % TSLD(0), ! SurfaceSoilTemperature
     &            SRFTEMP, ! SurfaceSoilTemperature
!     &            OUTITF % TSLD(1:INPITF % NLAYR))! SoilTemperatureByLayers
     &            ST(1:NLAYR))! SoilTemperatureByLayers
!-----------------------------------------------------------------------
        CASE('H') ! C2ML DSSAT-EPIC
            CALL GET('PLANT','BIOMAS',BIOMAS)      !kg/ha
            CALL GET('ORGC' ,'MULCHMASS',MULCHMASS)   !kg/ha
            CALL GET('WATER','SNOW',SNOW)       !mm
            CALL GET('MGMT','DEPIR',DEPIR)      !mm

            CALL model_stemp_epic(NL, ISWWAT, 
!     &            INPITF % SLBDM, INPITF % THICK(1:INPITF % NLAYR), 
     &            SOILPROP % BD, SOILPROP % DLAYR,
!     &            INPITF % SLLB(1:INPITF % NLAYR), INPITF % SLDUL, 
     &            SOILPROP % DS, SOILPROP % DUL,
!     &            INPITF % SLLL, INPITF % NLAYR, 
     &            SOILPROP % LL, NLAYR, 
!     &            INPITF % TAMP, 
     &            WEATHER % TAMP, 
!     &            INPITF % RAIN, INPITF % SWLD, INPITF % T2M, 
     &            WEATHER % RAIN, SW, WEATHER % TAVG, 
!     &            INPITF % TMAX, INPITF % TMIN, INPITF % TAV, 
     &            WEATHER % TMAX, WEATHER % TMIN, WEATHER % TAV, 
     &            CUMDPT, 
     &            DSMID,
     &            TDL, 
     &            TMA, 
     &            NDays, 
     &            WetDay, 
     &            X2_PREV, 
!     &            OUTITF % TSLD(0), OUTITF % TSLD(1:INPITF % NLAYR), 
     &            SRFTEMP, ST,
!     &            INPITF % IRVAL, 
     &            DEPIR, 
!     &            INPITF % CWAD, 
     &            BIOMAS, 
!     &            INPITF % MLTHK, 
     &            MULCHMASS, 
!     &            INPITF % SNOW, 
     &            SNOW)
!-----------------------------------------------------------------------
        CASE ('I') ! C2ML DSSAT
            CALL model_stemp(NL, ISWWAT,
!     &            INPITF % SLBDM,  
     &            SOILPROP % BD,    
!     &            INPITF % THICK(1:INPITF % NLAYR),                    
     &            SOILPROP % DLAYR,                    
!     &            INPITF % SLLB(1:INPITF % NLAYR),                       
     &            SOILPROP % DS,                       
!     &            INPITF % SLDUL, INPITF % SLLL,                       
     &            SOILPROP % DUL, SOILPROP % LL,                       
!     &            INPITF % NLAYR, INPITF % SALB,                       
     &            SOILPROP % NLAYR, SOILPROP % MSALB,                       
!     &            INPITF % SRAD, INPITF % SWLD, INPITF % T2M,           
     &            SRAD, SW, TAVG,           
!     &            INPITF % TMAX, INPITF % XLAT, INPITF % TAV,          
     &            TMAX, XLAT, TAV,                                    
!     &            INPITF % TAMP,                                                                        
     &            TAMP,                                       
     &            CUMDPT,                                              
     &            DSMID,                                               
     &            TDL,                                                 
     &            TMA,                                                 
     &            ATOT,      
!     &            OUTITF % TSLD(0), OUTITF % TSLD(1:INPITF % NLAYR),                                             
     &            SRFTEMP, ST,   
     &            DOY, HDAY)
!-----------------------------------------------------------------------
        CASE ('J') ! MONICA
          CALL model_soiltemperaturecomp(
     &            WEATHER % TMIN, ! tmin
     &            WEATHER % TMAX, ! tmax
     &            WEATHER % SRAD, ! globrad
     &            dampingFactor, 
     &            soilCoverage, 
     &            soilSurfaceTemperatureBelowSnow, 
     &            hasSnowCover, 
     &            timeStep, 
     &            soilMoistureConst, 
     &            baseTemp, 
     &            initialSurfaceTemp, 
     &            densityAir, 
     &            specificHeatCapacityAir, 
     &            densityHumus, 
     &            specificHeatCapacityHumus, 
     &            densityWater, 
     &            specificHeatCapacityWater, 
     &            quartzRawDensity, 
     &            specificHeatCapacityQuartz, 
     &            nTau, 
     &            noOfTempLayers, 
     &            noOfTempLayersPlus1, 
     &            noOfSoilLayers, 
     &            layerThickness,
     &            soilBulkDensity, 
     &            saturation, 
     &            soilOrganicMatter, 
     &            V, 
     &            B_monica, 
     &            volumeMatrix, 
     &            volumeMatrixOld, 
     &            matrixPrimaryDiagonal, 
     &            matrixSecondaryDiagonal, 
     &            heatConductivity, 
     &            heatConductivityMean, 
     &            heatCapacity_monica, 
     &            solution, 
     &            matrixDiagonal, 
     &            matrixLowerTriangle, 
     &            heatFlow, 
     &            SRFTEMP,!OUTITF % TSLD(0), ! soilSurfaceTemperature
     &            soilTemperature)

            ! Matching SQ outputs for ST.
            auxstemp = 0
            DO I = 1, noOfSoilLayers
              auxstemp = auxstemp + soilTemperature(I) 
              SELECT CASE(I)
                CASE(1)
                  ST(1) = auxstemp
                  auxstemp = 0
                CASE(3)
                  ST(2) = auxstemp / 2
                  auxstemp = 0
                CASE(6)
                    ST(3) = auxstemp / 3
                  auxstemp = 0
                CASE(9)
                  ST(4) = auxstemp / 3
                  auxstemp = 0
                CASE(12)
                  ST(5) = auxstemp / 3
                  auxstemp = 0
                CASE(18)
                  ST(6) = auxstemp / 6
                  auxstemp = 0
                CASE(24)
                  ST(7) = auxstemp / 6
                  auxstemp = 0
                CASE(30)
                  ST(8) = auxstemp / 6
                  auxstemp = 0
                CASE(36)
                  ST(9) = auxstemp / 6
                  auxstemp = 0
                CASE(42)
                  ST(10) = auxstemp / 6
                  auxstemp = 0
                CASE DEFAULT
              END SELECT
            ENDDO          
!-----------------------------------------------------------------------
        CASE ('K') ! Simplace
            CALL GET('ORGC' ,'MULCHMASS',MULCHMASS)   !kg/ha
            CALL GET('WATER','SNOW',SNOW)       !mm
            
            CALL model_snowcovercalculator(
     &            cCarbonContent, 
     &            cInitialAgeOfSnow, 
     &            cInitialSnowWaterContent, 
     &            SOILPROP % MSALB, ! Albedo,
     &            pInternalAlbedo, 
     &            cSnowIsolationFactorA, 
     &            cSnowIsolationFactorB, 
     &            WEATHER % TMAX, ! iTempMax, 
     &            WEATHER % TMIN, ! iTempMin, 
     &            WEATHER % SRAD, ! iRadiation, 
     &            WEATHER % RAIN, ! iRAIN, 
     &            MULCHMASS,! iCropResidues,
     &            EOS, ! iPotentialSoilEvaporation
     &            XHLAI, ! iLeafAreaIndex
     &            SoilTempArray,
     &            SNOW, ! SnowWaterContent
     &            SRFTEMP, ! SoilSurfaceTemperature
     &            AgeOfSnow, 
     &            rSnowWaterContentRate, 
     &            rSoilSurfaceTemperatureRate, 
     &            rAgeOfSnowRate, 
     &            SnowIsolationIndex)

            CALL model_stmpsimcalculator(
     &            cSoilLayerDepth, 
     &            cFirstDayMeanTemp, 
     &            WEATHER % TAV, ! cAVT: cAverageGroundTemperature
     &            SABDM, ! cABD: cAverageBulkDensity
     &            cDampingDepth, 
     &            iSoilWaterContent, 
     &            SRFTEMP, ! SoilSurfaceTemperature
     &            SoilTempArray,
     &            rSoilTempArrayRate, 
     &            pSoilLayerDepth)

             ST(1:NLAYR) = SoilTempArray(1:NLAYR)
!-----------------------------------------------------------------------
        CASE ('L') ! SIRIUS-Quality
            CALL model_soiltemperature(
!     &            INPITF % T2M,  ! meanTAir
     &            WEATHER % TAVG,  ! meanTAir
!     &            INPITF % TMIN, ! minTAir
     &            WEATHER % TMIN, ! minTAir
     &            lambda_, 
!     &            INPITF % TAV, ! meanAnnualAirTemp
     &            WEATHER % TAV, ! meanAnnualAirTemp
!     &            INPITF % G, ! heatFlux (g/m2/d)
     &            heatFlux,
!     &            INPITF % TMAX, ! maxTAir
     &            WEATHER % TMAX, ! maxTAir
     &            b, 
     &            c, 
     &            a, 
!     &            INPITF % DAYLD, ! dayLength
     &            WEATHER % DAYL, ! dayLength
     &            minTSoil, 
     &            deepLayerT, 
     &            maxTSoil, 
     &            hourlySoilT)

!                  OUTITF % TSLD(0) = -99
                  SRFTEMP = -99.0
!                  OUTITF % TSLD(1) = (maxTsoil + minTSoil)/2
                  ST(1) = (maxTsoil + minTSoil)/2
!                  OUTITF % TSLD(2:INPITF % NLAYR) = deepLayerT
                  ST(2:NLAYR) = deepLayerT
!-----------------------------------------------------------------------
        CASE ('M') ! STICS
            CALL model_soil_temp(
!     &            INPITF % TMIN, !min_temp
     &            WEATHER % TMIN, !min_temp
!     &            INPITF % TMAX, !max_temp
     &            WEATHER % TMAX, !max_temp
     &            prev_temp_profile,
     &            prev_canopy_temp,
!     &            INPITF % TMIN, !min_air_temp
     &            WEATHER % TMIN, !min_air_temp
!     &            INPITF % T2M, !air_temp_day1
     &            WEATHER % TAVG, !air_temp_day1
!     &            LTHICK(1:INPITF % NLAYR), !layer_thick
     &            LTHICK(1:NLAYR), !layer_thick
!     &            INPITF % TMIN, !min_canopy_temp
     &            WEATHER % TMIN, !min_canopy_temp
!     &            INPITF % TMAX,!max_canopy_temp
     &            WEATHER % TMAX,!max_canopy_temp
     &            temp_amp, !temp_amp 
     &            temp_profile, 
     &            layer_temp,
!     &            OUTITF % TSLD(0)) !canopy_temp_avg
     &            SRFTEMP) !canopy_temp_avg

            prev_temp_profile = temp_profile
!            prev_canopy_temp  = OUTITF % TSLD(0)
            prev_canopy_temp  = SRFTEMP
!            OUTITF % TSLD(1:INPITF%NLAYR) = layer_temp(1:INPITF%NLAYR)
            ST(1:NLAYR) = layer_temp(1:NLAYR)
!-----------------------------------------------------------------------
!     CSM_Reverse_ST_Modeling by FO
!     END
!***********************************************************************
        CASE ('E')    !EPIC soil temperature routine
          CALL STEMP_EPIC(CONTROL, ISWITCH,
     &      SOILPROP, SW, TAVG, TMAX, TMIN, TAV, WEATHER,   !Input
     &      SRFTEMP, ST)                                    !Output
        CASE DEFAULT
!       7/21/2016 - DSSAT method is default, per GH
!        CASE ('D')  !DSSAT soil temperature
          CALL STEMP(CONTROL, ISWITCH,
     &      SOILPROP, SRAD, SW, TAVG, TMAX, XLAT, TAV, TAMP,!Input
     &      SRFTEMP, ST)                                    !Output
        END SELECT
      ENDIF
!-----------------------------------------------------------------------
!     POTENTIAL ROOT WATER UPTAKE
!-----------------------------------------------------------------------
      IF (ISWWAT .EQ. 'Y') THEN
!       Calculate the availability of soil water for use in SOILEV.
        DO L = 1, NLAYR
          SW_AVAIL(L) = MAX(0.0, SW(L) + SWDELTS(L) + SWDELTU(L))
        ENDDO

!       These processes are done by ETPHOT for hourly (Zonal) energy
!       balance method.
        IF (MEEVP .NE. 'Z') THEN
!         Calculate potential root water uptake rate for each soil layer
!         and total potential water uptake rate.
          IF (XHLAI .GT. 0.0) THEN
            CALL ROOTWU(RATE,
     &          DLAYR, LL, NLAYR, PORMIN, RLV, RWUMX, SAT, SW,!Input
     &          RWU, TRWUP)                                   !Output
          ELSE
            RWU   = 0.0
            TRWUP = 0.0
          ENDIF

!-----------------------------------------------------------------------
!         POTENTIAL EVAPOTRANSPIRATION
!-----------------------------------------------------------------------
          IF (FLOOD .GT. 0.0) THEN
            ! Set albedo to 0.08 under flooded conditions
            ! US - change to 0.05 Feb2004
            ET_ALB = 0.05
          ELSE
            ET_ALB = MSALB
          ENDIF

          CALL PET(CONTROL,
     &       ET_ALB, XHLAI, MEEVP, WEATHER,  !Input for all
     &       EORATIO, !Needed by Penman-Monteith
     &       CANHT,   !Needed by dynamic Penman-Monteith
     &       EO,      !Output
     &       ET0)     !Output hourly Priestly-Taylor with VPD effect

!-----------------------------------------------------------------------
!         POTENTIAL SOIL EVAPORATION
!-----------------------------------------------------------------------
!         05/26/2007 CHP/MJ Use XLAI instead of XHLAI
!         This was important for Canegro and affects CROPGRO crops
!             only very slightly (max 0.5% yield diff for one peanut
!             experiment).  No difference to other crop models.
          CALL PSE(EO, KSEVAP, XLAI, EOS)

!-----------------------------------------------------------------------
!         ACTUAL SOIL, MULCH AND FLOOD EVAPORATION
!-----------------------------------------------------------------------
!         Initialize soil, mulch and flood evaporation
          ES = 0.; EM = 0.; EF = 0.; EVAP = 0.0
          UPFLOW = 0.0; ES_LYR = 0.0

!         First meet evaporative demand from floodwater
          IF (FLOOD .GT. 1.E-4) THEN
            CALL FLOOD_EVAP(XLAI, EO, EF)
            IF (EF > FLOOD) THEN
!             Floodwater not enough to supply EOS demand
              EOS_SOIL = MIN(EF - FLOOD, EOS)
              EF = FLOOD
            ELSE
              EOS_SOIL = 0.0
            ENDIF
          ELSE
            EOS_SOIL = EOS
          ENDIF

!         Next meet evaporative demand from mulch
          IF (EOS_SOIL > 1.E-6 .AND. INDEX('RSM',MEINF) > 0) THEN
            CALL MULCH_EVAP(DYNAMIC, MULCH, EOS_SOIL, EM)
            IF (EOS_SOIL > EM) THEN
!             Some evaporative demand leftover for soil
              EOS_SOIL = EOS_SOIL - EM
            ELSE
              EOS_SOIL = 0.0
            ENDIF
          ENDIF

!         Soil evaporation after flood and mulch evaporation
          IF (EOS_SOIL > 1.E-6) THEN
            SELECT CASE(MESEV)
!           ------------------------
            CASE ('S')  ! Sulieman-Ritchie soil evaporation routine
!             Note that this routine calculates UPFLOW, unlike the SOILEV.
              CALL ESR_SoilEvap(
     &          EOS_SOIL, SOILPROP, SW, SWDELTS,          !Input
     &          ES, ES_LYR, SWDELTU, UPFLOW)              !Output
!           ------------------------
            CASE DEFAULT
!           CASE ('R')  !Ritchie soil evaporation routine
!             Calculate the availability of soil water for use in SOILEV.
              DO L = 1, NLAYR
                SW_AVAIL(L) = MAX(0.0, SW(L) + SWDELTS(L) + SWDELTU(L))
              ENDDO
              CALL SOILEV(RATE,
     &          DLAYR, DUL, EOS_SOIL, LL, SW,             !Input
     &          SW_AVAIL(1), U, WINF,                     !Input
     &          ES)                                       !Output
            END SELECT
!           ------------------------
          ENDIF

!         Total evaporation from soil, mulch, flood
          EVAP = ES + EM + EF

!-----------------------------------------------------------------------
!         Potential transpiration - model dependent
!-----------------------------------------------------------------------
          IF (XHLAI > 1.E-6) THEN
            CALL TRANS(RATE, MEEVP,
     &      CO2, CROP, EO, ET0, EVAP, KTRANS,           !Input
     &      WINDSP, XHLAI,                              !Input
     &      WEATHER,                                    !Input
     &      EOP)                                        !Output

          ELSE
            EOP = 0.0
          ENDIF

!-----------------------------------------------------------------------
!         ACTUAL TRANSPIRATION
!-----------------------------------------------------------------------
          IF (XHLAI .GT. 1.E-4 .AND. EOP .GT. 1.E-4) THEN
            !These calcs replace the old SWFACS subroutine
            !Stress factors now calculated as needed in PLANT routines.
            EP = MIN(EOP, TRWUP*10.)
          ELSE
            EP = 0.0
          ENDIF
        ENDIF
      ENDIF

!-----------------------------------------------------------------------
!     ALTERNATE CALL TO ENERGY BALANCE ROUTINES
!-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
        IF (MEEVP .EQ. 'Z' .OR.
     &        (MEPHO .EQ. 'L' .AND. XHLAI .GT. 0.0)) THEN
          !ETPHOT called for photosynthesis only
          !    (MEPHO = 'L' and MEEVP <> 'Z')
          !or for both photosynthesis and evapotranspiration
          !   (MEPHO = 'L' and MEEVP = 'Z').
          CALL ETPHOT(CONTROL, ISWITCH,
     &    PORMIN, PSTRES1, RLV, RWUMX, SOILPROP, ST, SW,  !Input
     &    WEATHER, XLAI,                                 !Input
     &    EOP, EP, ES, RWU, TRWUP)                        !Output
          EVAP = ES  !CHP / BK 7/13/2017
        ENDIF

!-----------------------------------------------------------------------
!       ACTUAL ROOT WATER EXTRACTION
!-----------------------------------------------------------------------
        IF (ISWWAT .EQ. 'Y') THEN
!         Adjust available soil water for evaporation
          SELECT CASE(MESEV)
          CASE ('R') !
            SW_AVAIL(1) = MAX(0.0, SW_AVAIL(1) - 0.1 * ES / DLAYR(1))

          CASE DEFAULT
            DO L = 1, NLAYR
              SW_AVAIL(L) = MAX(0.0,SW_AVAIL(L) -0.1*ES_LYR(L)/DLAYR(L))
            ENDDO
          END SELECT

!         Calculate actual soil water uptake and transpiration rates
          CALL XTRACT(
     &      NLAYR, DLAYR, LL, SW, SW_AVAIL, TRWUP, UH2O,  !Input
     &      EP, RWU,                                      !Input/Output
     &      SWDELTX, TRWU)                                !Output
        ENDIF   !ISWWAT = 'Y'
      ENDIF

!     Transfer computed value of potential floodwater evaporation to
!     flood variable.
      FLOODWAT % EF = EF

!     Transfer data to storage routine
      CALL PUT('SPAM', 'EF',  EF)
      CALL PUT('SPAM', 'EM',  EM)
      CALL PUT('SPAM', 'EO',  EO)
      CALL PUT('SPAM', 'EP',  EP)
      CALL PUT('SPAM', 'ES',  ES)
      CALL PUT('SPAM', 'EOP', EOP)
      CALL PUT('SPAM', 'EVAP',EVAP)
      CALL PUT('SPAM', 'UH2O',RWU)

!***********************************************************************
!***********************************************************************
!     DAILY INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
      IF (ISWWAT .EQ. 'Y') THEN
!       Perform daily summation of water balance variables.
        ET  = EVAP + EP
        CEF = CEF + EF
        CEM = CEM + EM
        CEO = CEO + EO
        CEP = CEP + EP
        CES = CES + ES
        CEVAP=CEVAP + EVAP
        CET = CET + ET
      ENDIF

      IF (IDETW .EQ. 'Y') THEN
        CALL OPSPAM(CONTROL, ISWITCH, FLOODWAT, TRWU,
     &    CEF, CEM, CEO, CEP, CES, CET, CEVAP, EF, EM,
     &    EO, EOP, EOS, EP, ES, ET, TMAX, TMIN, TRWUP, SRAD,
     &    ES_LYR, SOILPROP)
      ENDIF

!     Transfer data to storage routine
      CALL PUT('SPAM', 'CEF', CEF)
      CALL PUT('SPAM', 'CEM', CEM)
      CALL PUT('SPAM', 'CEO', CEO)
      CALL PUT('SPAM', 'CEP', CEP)
      CALL PUT('SPAM', 'CES', CES)
      CALL PUT('SPAM', 'CET', CET)
      CALL PUT('SPAM', 'ET',  ET)
      CALL PUT('SPAM', 'CEVAP', CEVAP)

!***********************************************************************
!***********************************************************************
!     OUTPUT - daily output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
!     Flood water evaporation can be modified by Paddy_Mgmt routine.
      EF = FLOODWAT % EF

!     ---------------------------------------------------------
      IF (MEEVP .NE.'Z') THEN  !LPM 02dec14 use values from ETPHOT
          SELECT CASE (METMP)
!*********************************************************************** 
!     CSM_Reverse_ST_Modeling by FO
!     CROP2ML - DAILY OUTPUT
!***********************************************************************
          CASE('F') ! BIOMA-Parton
            CALL OPSTEMP(CONTROL, ISWITCH, DOY, SRFTEMP, ST, TAV, TAMP)
            CALL OPSTEMP_AMEI_ST(CONTROL, ISWITCH,DOY,SRFTEMP,ST,SW)
            CALL OPSTEMP_AMEI_CL(CONTROL, ISWITCH,EOS,ES,EO,ET)
!-----------------------------------------------------------------------
          CASE('G') ! BIOMA-SWAT
            CALL OPSTEMP(CONTROL, ISWITCH, DOY, SRFTEMP, ST, TAV, TAMP)
            CALL OPSTEMP_AMEI_ST(CONTROL, ISWITCH,DOY,SRFTEMP,ST,SW)
            CALL OPSTEMP_AMEI_CL(CONTROL, ISWITCH,EOS,ES,EO,ET)
!-----------------------------------------------------------------------
          CASE('H') ! C2ML DSSAT-EPIC
            CALL OPSTEMP(CONTROL, ISWITCH, DOY, SRFTEMP, ST, TAV, TAMP)
            CALL OPSTEMP_AMEI_ST(CONTROL, ISWITCH,DOY,SRFTEMP,ST,SW)
            CALL OPSTEMP_AMEI_CL(CONTROL, ISWITCH,EOS,ES,EO,ET)
!-----------------------------------------------------------------------
          CASE('I') ! C2ML DSSAT
            CALL OPSTEMP(CONTROL, ISWITCH, DOY, SRFTEMP, ST, TAV, TAMP)
            CALL OPSTEMP_AMEI_ST(CONTROL, ISWITCH,DOY,SRFTEMP,ST,SW)
            CALL OPSTEMP_AMEI_CL(CONTROL, ISWITCH,EOS,ES,EO,ET)
!-----------------------------------------------------------------------
          CASE('J') ! MONICA
            CALL OPSTEMP(CONTROL, ISWITCH, DOY, SRFTEMP, ST, TAV, TAMP)
            CALL OPSTEMP_AMEI_ST(CONTROL, ISWITCH,DOY,SRFTEMP,ST,SW)
            CALL OPSTEMP_AMEI_CL(CONTROL, ISWITCH,EOS,ES,EO,ET)
!-----------------------------------------------------------------------
          CASE('K') ! Simplace
            CALL OPSTEMP(CONTROL, ISWITCH, DOY, SRFTEMP, ST, TAV, TAMP)
            CALL OPSTEMP_AMEI_ST(CONTROL, ISWITCH,DOY,SRFTEMP,ST,SW)
            CALL OPSTEMP_AMEI_CL(CONTROL, ISWITCH,EOS,ES,EO,ET)
!-----------------------------------------------------------------------
          CASE('L') ! SIRIUS-Quality
            CALL OPSTEMP(CONTROL, ISWITCH, DOY, SRFTEMP, ST, TAV, TAMP)
            CALL OPSTEMP_AMEI_ST(CONTROL, ISWITCH,DOY,SRFTEMP,ST,SW)
            CALL OPSTEMP_AMEI_CL(CONTROL, ISWITCH,EOS,ES,EO,ET)
!-----------------------------------------------------------------------
          CASE('M') ! STICS
            CALL OPSTEMP(CONTROL, ISWITCH, DOY, SRFTEMP, ST, TAV, TAMP)
            CALL OPSTEMP_AMEI_ST(CONTROL, ISWITCH,DOY,SRFTEMP,ST,SW)
            CALL OPSTEMP_AMEI_CL(CONTROL, ISWITCH,EOS,ES,EO,ET)
!-----------------------------------------------------------------------
!     CSM_Reverse_ST_Modeling by FO
!     END
!***********************************************************************
          CASE ('E')    !EPIC soil temperature routine
            CALL STEMP_EPIC(CONTROL, ISWITCH,
     &        SOILPROP, SW, TAVG, TMAX, TMIN, TAV, WEATHER,   !Input
     &        SRFTEMP, ST)                                    !Output
          CASE DEFAULT  !DSSAT soilt temperature
            CALL STEMP(CONTROL, ISWITCH,
     &        SOILPROP, SRAD, SW, TAVG, TMAX, XLAT, TAV, TAMP,!Input
     &        SRFTEMP, ST)                                    !Output

          END SELECT
      ENDIF

      CALL OPSPAM(CONTROL, ISWITCH, FLOODWAT, TRWU,
     &    CEF, CEM, CEO, CEP, CES, CET, CEVAP, EF, EM,
     &    EO, EOP, EOS, EP, ES, ET, TMAX, TMIN, TRWUP, SRAD,
     &    ES_LYR, SOILPROP)

      IF (CROP .NE. 'FA' .AND. MEPHO .EQ. 'L') THEN
        CALL ETPHOT(CONTROL, ISWITCH,
     &    PORMIN, PSTRES1, RLV, RWUMX, SOILPROP, ST, SW,  !Input
     &    WEATHER, XLAI,                                  !Input
     &    EOP, EP, ES, RWU, TRWUP)                        !Output
      ENDIF

!      CALL OPSTRESS(CONTROL, ET=ET, EP=EP)

!***********************************************************************
!***********************************************************************
!     SEASEND - seasonal output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
      CALL OPSPAM(CONTROL, ISWITCH, FLOODWAT, TRWU,
     &    CEF, CEM, CEO, CEP, CES, CET, CEVAP, EF, EM,
     &    EO, EOP, EOS, EP, ES, ET, TMAX, TMIN, TRWUP, SRAD,
     &    ES_LYR, SOILPROP)

!     ---------------------------------------------------------
      IF (MEEVP .NE.'Z') THEN  !LPM 02dec14 use values from ETPHOT
          SELECT CASE (METMP)
          CASE ('E')    !EPIC soil temperature routine
            CALL STEMP_EPIC(CONTROL, ISWITCH,
     &        SOILPROP, SW, TAVG, TMAX, TMIN, TAV, WEATHER,   !Input
     &        SRFTEMP, ST)                                    !Output
          CASE DEFAULT  !DSSAT soilt temperature
            CALL STEMP(CONTROL, ISWITCH,
     &        SOILPROP, SRAD, SW, TAVG, TMAX, XLAT, TAV, TAMP,!Input
     &        SRFTEMP, ST)                                    !Output
          END SELECT
      ENDIF

      IF (MEPHO .EQ. 'L') THEN
        CALL ETPHOT(CONTROL, ISWITCH,
     &    PORMIN, PSTRES1, RLV, RWUMX, SOILPROP, ST, SW,  !Input
     &    WEATHER, XLAI,                                  !Input
     &    EOP, EP, ES, RWU, TRWUP)                        !Output
      ENDIF

!     Transfer data to storage routine
      CALL PUT('SPAM', 'CEF', CEF)
      CALL PUT('SPAM', 'CEM', CEM)
      CALL PUT('SPAM', 'CEO', CEO)
      CALL PUT('SPAM', 'CEP', CEP)
      CALL PUT('SPAM', 'CES', CES)
      CALL PUT('SPAM', 'CET', CET)
      CALL PUT('SPAM', 'ET',  ET)
      CALL PUT('SPAM', 'CEVAP', CEVAP)

!      CALL OPSTRESS(CONTROL, ET=ET, EP=EP)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE SPAM

!-----------------------------------------------------------------------
!     VARIABLE DEFINITIONS: (updated 12 Feb 2004)
!-----------------------------------------------------------------------
! CANHT       Canopy height (m)
! CEF         Cumulative seasonal evaporation from floodwater surface (mm)
! CEM         Cumulative evaporation from surface mulch layer (mm)
! CEO         Cumulative potential evapotranspiration (mm)
! CEP         Cumulative transpiration (mm)
! CES         Cumulative evaporation (mm)
! CET         Cumulative evapotranspiration (mm)
! CLOUDS      Relative cloudiness factor (0-1)
! CO2         Atmospheric carbon dioxide concentration
!              (�mol[CO2] / mol[air])
! CONTROL     Composite variable containing variables related to control
!               and/or timing of simulation.    See Appendix A.
! CROP        Crop identification code
! DLAYR(L)    Thickness of soil layer L (cm)
! DUL(L)      Volumetric soil water content at Drained Upper Limit in soil
!               layer L (cm3[water]/cm3[soil])
! EF          Evaporation rate from flood surface (mm / d)
! EM          Evaporation rate from surface mulch layer (mm / d)
! EO          Potential evapotranspiration rate (mm/d)
! EOP         Potential plant transpiration rate (mm/d)
! EORATIO     Ratio of increase in potential evapotranspiration with
!               increase in LAI (up to LAI=6.0) for use with FAO-56 Penman
!               reference potential evapotranspiration.
! EOS         Potential rate of soil evaporation (mm/d)
! EP          Actual plant transpiration rate (mm/d)
! ES          Actual soil evaporation rate (mm/d)
! ET          Actual evapotranspiration rate (mm/d)
! FLOOD       Current depth of flooding (mm)
! FLOODWAT    Composite variable containing information related to bund
!               management. Structure of variable is defined in
!               ModuleDefs.for.
! IDETW       Y=detailed water balance output, N=no detailed output
! ISWITCH     Composite variable containing switches which control flow of
!               execution for model.  The structure of the variable
!               (SwitchType) is defined in ModuleDefs.for.
! ISWWAT      Water simulation control switch (Y or N)
! KSEVAP      Light extinction coefficient used for computation of soil
!               evaporation
! KTRANS      Light extinction coefficient used for computation of plant
!               transpiration
! LL(L)       Volumetric soil water content in soil layer L at lower limit
!              (cm3 [water] / cm3 [soil])
! MEEVP       Method of evapotranspiration ('P'=Penman,
!               'R'=Priestly-Taylor, 'Z'=Zonal)
! MEPHO       Method for photosynthesis computation ('C'=Canopy or daily,
!               'L'=hedgerow or hourly)
! NLAYR       Actual number of soil layers
! PORMIN      Minimum pore space required for supplying oxygen to roots for
!               optimal growth and function (cm3/cm3)
! RLV(L)      Root length density for soil layer L (cm[root] / cm3[soil])
! RWU(L)      Root water uptake from soil layer L (cm/d)
! RWUMX       Maximum water uptake per unit root length, constrained by
!               soil water (cm3[water] / cm [root])
! MSALB       Soil albedo with mulch and soil water effects (fraction)
! SAT(L)      Volumetric soil water content in layer L at saturation
!              (cm3 [water] / cm3 [soil])
! SOILPROP    Composite variable containing soil properties including bulk
!               density, drained upper limit, lower limit, pH, saturation
!               water content.  Structure defined in ModuleDefs.
! SRAD        Solar radiation (MJ/m2-d)
! SRFTEMP     Temperature of soil surface litter (�C)
! ST(L)       Soil temperature in soil layer L (�C)
! SUMES1      Cumulative soil evaporation in stage 1 (mm)
! SUMES2      Cumulative soil evaporation in stage 2 (mm)
! SW(L)       Volumetric soil water content in layer L
!              (cm3 [water] / cm3 [soil])
! SW_AVAIL(L) Soil water content in layer L available for evaporation,
!               plant extraction, or movement through soil
!               (cm3 [water] / cm3 [soil])
! SWDELTS(L)  Change in soil water content due to drainage in layer L
!              (cm3 [water] / cm3 [soil])
! SWDELTU(L)  Change in soil water content due to evaporation and/or upward
!               flow in layer L (cm3 [water] / cm3 [soil])
! SWDELTX(L)  Change in soil water content due to root water uptake in
!               layer L (cm3 [water] / cm3 [soil])
! T           Number of days into Stage 2 evaporation (WATBAL); or time
!               factor for hourly temperature calculations
! TA          Daily normal temperature (�C)
! TAMP        Amplitude of temperature function used to calculate soil
!               temperatures (�C)
! TAV         Average annual soil temperature, used with TAMP to calculate
!               soil temperature. (�C)
! TAVG        Average daily temperature (�C)
! TMAX        Maximum daily temperature (�C)
! TMIN        Minimum daily temperature (�C)
! TRWU        Actual daily root water uptake over soil profile (cm/d)
! TRWUP       Potential daily root water uptake over soil profile (cm/d)
! U           Evaporation limit (cm)
! WINDSP      Wind speed at 2m (km/d)
! WINF        Water available for infiltration - rainfall minus runoff plus
!               net irrigation (mm / d)
! XHLAI       Healthy leaf area index (m2[leaf] / m2[ground])
! XLAT        Latitude (deg.)
!-----------------------------------------------------------------------
!     END SUBROUTINE SPAM
!-----------------------------------------------------------------------

