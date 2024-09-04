C=======================================================================
      SUBROUTINE INTF_C2ML_SIMPL(INPITF, OUTITF)

      USE ModuleDefs
      USE ModuleData
      USE InterfaceDataMod
      USE Snowcovercalculatormod
      USE Stmpsimcalculatormod
      IMPLICIT NONE
      SAVE

!     Control and data structures
      TYPE (INTFDataInput) INPITF
      TYPE (INTFDataOutput) OUTITF

!     State model variables
      INTEGER AgeOfSnow, rAgeOfSnowRate, cInitialAgeOfSnow
      REAL cFirstDayMeanTemp
      REAL cAverageBulkDensity, iSoilWaterContent
      REAL rSnowWaterContentRate, cInitialSnowWaterContent
      REAL rSoilSurfaceTemperatureRate
      REAL SnowIsolationIndex
      REAL cSnowIsolationFactorA, cSnowIsolationFactorB
      REAL pInternalAlbedo, iSoilSurfaceTemperature
      REAL cCarbonContent, cDampingDepth
      REAL , ALLOCATABLE, DIMENSION(: ):: SoilTempArray
      REAL , ALLOCATABLE, DIMENSION(: ):: rSoilTempArrayRate
      REAL , ALLOCATABLE, DIMENSION(: ):: pSoilLayerDepth
      REAL, DIMENSION(1:INPITF % NLAYR) :: cSoilLayerDepth

!*********************************************************************** 
!     INTERFACE INDIVIDUAL ATTRIBUTIONS
!***********************************************************************
      ! Conversion of variable units
      iSoilWatercontent = INPITF % TSW * 10 ! cm to mm
      cCarbonContent = INPITF % SLOC(1)
      cSoilLayerDepth = INPITF % SLLB(1:INPITF % NLAYR) * 0.01 ! cm to m
      cDampingDepth = 6.0
      AgeOfSnow = 0
      cFirstDayMeanTemp = INPITF % TAV
      cInitialAgeOfSnow = 0
      cInitialSnowWaterContent = 0.0
      cSnowIsolationFactorA = 0.47
      cSnowIsolationFactorB = 0.62
      pInternalAlbedo = INPITF % SALB
!*********************************************************************** 
!     SEASINIT
!***********************************************************************
      IF (INPITF % DYNAMIC .EQ. SEASINIT) THEN


            CALL init_snowcovercalculator(
     &            cCarbonContent,
     &            cInitialAgeOfSnow, 
     &            cInitialSnowWaterContent, 
     &            INPITF % SALB, ! Albedo, 
     &            cSnowIsolationFactorA, 
     &            cSnowIsolationFactorB, 
     &            INPITF % TMAX, ! iTempMax, 
     &            INPITF % TMIN, ! iTempMin, 
     &            INPITF % SRAD, ! iRadiation, 
     &            INPITF % RAIN, ! iRAIN, 
     &            INPITF % MLTHK,! iCropResidues,
     &            INPITF % EOAD, ! iPotentialSoilEvaporation
     &            INPITF % LAID, ! iLeafAreaIndex
     &            SoilTempArray,
     &            pInternalAlbedo, 
     &            INPITF % SNOW, ! SnowWaterContent
     &            OUTITF % TSLD(0), ! SoilSurfaceTemperature
     &            AgeOfSnow)
     
            CALL init_stmpsimcalculator(
     &            cSoilLayerDepth, 
     &            cFirstDayMeanTemp, 
     &            INPITF % TAV, ! cAVT: cAverageGroundTemperature
     &            INPITF % SABDM, ! cABD: cAverageBulkDensity
     &            cDampingDepth, 
     &            iSoilWaterContent, 
     &            OUTITF % TSLD(0), ! SoilSurfaceTemperature
     &            SoilTempArray,
     &            rSoilTempArrayRate,
     &            pSoilLayerDepth)

!*********************************************************************** 
!     RATE
!***********************************************************************
      ELSEIF (INPITF % DYNAMIC .EQ. RATE) THEN

            CALL model_snowcovercalculator(
     &            cCarbonContent, 
     &            cInitialAgeOfSnow, 
     &            cInitialSnowWaterContent, 
     &            INPITF % SALB, ! Albedo,
     &            pInternalAlbedo, 
     &            cSnowIsolationFactorA, 
     &            cSnowIsolationFactorB, 
     &            INPITF % TMAX, ! iTempMax, 
     &            INPITF % TMIN, ! iTempMin, 
     &            INPITF % SRAD, ! iRadiation, 
     &            INPITF % RAIN, ! iRAIN, 
     &            INPITF % MLTHK,! iCropResidues,
     &            INPITF % EOAD, ! iPotentialSoilEvaporation
     &            INPITF % LAID, ! iLeafAreaIndex
     &            SoilTempArray,
     &            INPITF % SNOW, ! SnowWaterContent
     &            OUTITF % TSLD(0), ! SoilSurfaceTemperature
     &            AgeOfSnow, 
     &            rSnowWaterContentRate, 
     &            rSoilSurfaceTemperatureRate, 
     &            rAgeOfSnowRate, 
     &            SnowIsolationIndex)

            CALL model_stmpsimcalculator(
     &            cSoilLayerDepth, 
     &            cFirstDayMeanTemp, 
     &            INPITF % TAV, ! cAVT: cAverageGroundTemperature
     &            INPITF % SABDM, ! cABD: cAverageBulkDensity
     &            cDampingDepth, 
     &            iSoilWaterContent, 
     &            OUTITF % TSLD(0), ! SoilSurfaceTemperature
     &            SoilTempArray,
     &            rSoilTempArrayRate, 
     &            pSoilLayerDepth)

            OUTITF % TSLX = -99.0
            OUTITF % TSLN = -99.0
            OUTITF % TSLD(1:INPITF%NLAYR)=SoilTempArray(1:INPITF%NLAYR)
      ENDIF
C***********************************************************************      

      RETURN
      END SUBROUTINE
C=======================================================================