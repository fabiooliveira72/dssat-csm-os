C=======================================================================
      SUBROUTINE INTF_C2ML_APSIM(INPITF, OUTITF)

      USE ModuleDefs
      USE InterfaceDataMod
      USE Soiltemperatureapsimmod
      IMPLICIT NONE
      SAVE

!     Control and data structures
      TYPE (INTFDataInput) INPITF
      TYPE (INTFDataOutput) OUTITF
      
      REAL weather_AirPressure, weather_Wind
      INTEGER clock_Today_DayOfYear
      REAL microClimate_CanopyHeight
      REAL ps
      REAL, DIMENSION(1:INPITF%NLAYR) :: physical_Rocks
      REAL, DIMENSION(1:INPITF%NLAYR) :: physical_ParticleSizeSand
      REAL, DIMENSION(1:INPITF%NLAYR) :: physical_ParticleSizeSilt
      REAL, DIMENSION(1:INPITF%NLAYR) :: physical_ParticleSizeClay
      REAL, ALLOCATABLE :: waterBalance_SW(:)
      REAL, DIMENSION(1:INPITF%NLAYR) :: organic_Carbon
      REAL, DIMENSION(1:INPITF%NLAYR) :: physical_Thickness
      REAL, DIMENSION(1:INPITF%NLAYR) :: physical_BD
      REAL, DIMENSION(:), ALLOCATABLE :: pInitialValues
      REAL DepthToConstantTemperature
      REAL timestep
      REAL latentHeatOfVapourisation
      REAL stefanBoltzmannConstant
      INTEGER airNode
      INTEGER surfaceNode
      INTEGER topsoilNode
      INTEGER numPhantomNodes
      REAL constantBoundaryLayerConductance
      INTEGER numIterationsForBoundaryLayerConductance
      REAL defaultTimeOfMaximumTemperature
      REAL defaultInstrumentHeight
      REAL bareSoilRoughness
      REAL, ALLOCATABLE :: nodeDepth(:)
      REAL, ALLOCATABLE :: thermCondPar1(:)
      REAL, ALLOCATABLE :: thermCondPar2(:)
      REAL, ALLOCATABLE :: thermCondPar3(:)
      REAL, ALLOCATABLE :: thermCondPar4(:)
      REAL pom
      REAL soilRoughnessHeight
      REAL nu
      CHARACTER(65) boundarLayerConductanceSource
      CHARACTER(65) netRadiationSource
      REAL MissingValue
      CHARACTER(65) , DIMENSION(8) :: soilConstituentNames
      REAL, DIMENSION(:), ALLOCATABLE :: InitialValues
      LOGICAL doInitialisationStuff
      REAL internalTimeStep
      REAL timeOfDaySecs
      INTEGER numNodes
      INTEGER numLayers
      REAL, ALLOCATABLE :: volSpecHeatSoil(:)
      REAL, ALLOCATABLE :: soilTemp(:)
      REAL, ALLOCATABLE :: morningSoilTemp(:)
      REAL, ALLOCATABLE :: heatStorage(:)
      REAL, ALLOCATABLE :: thermalcon(:)
      REAL, ALLOCATABLE :: thermalConductivity(:)
      REAL boundaryLayerConductance
      REAL , DIMENSION(: ), ALLOCATABLE :: newTemperature
      REAL airTemperature
      REAL maxTempYesterday
      REAL minTempYesterday
      REAL, ALLOCATABLE :: soilWater(:)
      REAL, ALLOCATABLE :: minSoilTemp(:)
      REAL, ALLOCATABLE :: maxSoilTemp(:)
      REAL, ALLOCATABLE :: aveSoilTemp(:)
      REAL, ALLOCATABLE :: aveSoilWater(:)
      REAL, ALLOCATABLE :: thickness(:)
      REAL, ALLOCATABLE :: bulkDensity(:)
      REAL, ALLOCATABLE :: rocks(:)
      REAL, ALLOCATABLE :: carbon(:)
      REAL, ALLOCATABLE :: sand(:)
      REAL, ALLOCATABLE :: silt(:)
      REAL, ALLOCATABLE :: clay(:)
      REAL instrumentHeight
      REAL netRadiation
      REAL canopyHeight
      REAL instrumHeight
!*********************************************************************** 
!     INTERFACE INDIVIDUAL ATTRIBUTIONS
!***********************************************************************
      physical_ParticleSizeSand = INPITF%SLSND(1:INPITF%NLAYR)
      physical_ParticleSizeSilt = INPITF%SLSIL(1:INPITF%NLAYR)
      physical_ParticleSizeClay = INPITF%SLCLY(1:INPITF%NLAYR)
      physical_Thickness = INPITF % THICK(1:INPITF%NLAYR) * 10
      physical_BD = INPITF % SLBDM(1:INPITF%NLAYR)
      organic_Carbon = INPITF % SLOC(1:INPITF%NLAYR)
      waterBalance_SW = INPITF % SWLD(1:INPITF%NLAYR)
      stefanBoltzmannConstant = 0.0000000567
      weather_AirPressure = 1010.0
      weather_Wind = 3.0
      clock_Today_DayOfYear = 1
      microClimate_CanopyHeight = 0.0
      ps = 2.63
      physical_Rocks = 0.0
      DepthToConstantTemperature = 10000.0
      timestep = 24 * 60 * 60
      latentHeatOfVapourisation = 2465000.0
      airNode = 0.0
      surfaceNode = 1
      topsoilNode = 2
      numPhantomNodes = 5
      constantBoundaryLayerConductance = 20.0
      numIterationsForBoundaryLayerConductance = 1
      defaultTimeOfMaximumTemperature = 14.0
      defaultInstrumentHeight = 1.2
      bareSoilRoughness = 57
      pom = 1.3
      nu = 0.6
      boundarLayerConductanceSource = 'calc'
      netRadiationSource = 'calc'
      MissingValue = 999999
      soilConstituentNames(1) = 'Rocks'
      soilConstituentNames(2) = 'OrganicMatter'
      soilConstituentNames(3) = 'Sand'
      soilConstituentNames(4) = 'Silt'
      soilConstituentNames(5) = 'Clay'
      soilConstituentNames(6) = 'Water'
      soilConstituentNames(7) = 'Ice'
      soilConstituentNames(8) = 'Air'

!*********************************************************************** 
!     SEASINIT
!***********************************************************************
      IF (INPITF % DYNAMIC .EQ. SEASINIT) THEN

        doInitialisationStuff = .FALSE.
        instrumentHeight = 0.0 
        soilRoughnessHeight = 0.0 
        
        CALL init_soiltemperature_APSIM(
     &     INPITF % TMIN, !weather_MinT, 
     &     INPITF % TMAX, !weather_MaxT, 
     &     INPITF % T2M, !weather_MeanT, 
     &     INPITF % TAV, !weather_Tav, 
     &     INPITF % TAMP, !weather_Amp, 
     &     weather_AirPressure, 
     &     weather_Wind, 
     &     INPITF % XLAT, !weather_Latitude, 
     &     INPITF % SRAD, !weather_Radn, 
     &     clock_Today_DayOfYear, 
     &     microClimate_CanopyHeight, 
     &     physical_Thickness,
     &     physical_BD, 
     &     ps, 
     &     physical_Rocks, 
     &     physical_ParticleSizeSand, 
     &     physical_ParticleSizeSilt, 
     &     physical_ParticleSizeClay,
     &     organic_Carbon,
     &     waterBalance_SW, 
     &     INPITF % ESP, !waterBalance_Eos, 
     &     INPITF % EOAD, !waterBalance_Eo, 
     &     INPITF % ES, !waterBalance_Es, 
     &     INPITF % SALB, !waterBalance_Salb, 
     &     pInitialValues, 
     &     DepthToConstantTemperature, 
     &     timestep, 
     &     latentHeatOfVapourisation, 
     &     stefanBoltzmannConstant, 
     &     airNode, 
     &     surfaceNode, 
     &     topsoilNode, 
     &     numPhantomNodes, 
     &     constantBoundaryLayerConductance, 
     &     numIterationsForBoundaryLayerConductance, 
     &     defaultTimeOfMaximumTemperature, 
     &     defaultInstrumentHeight, 
     &     bareSoilRoughness, 
     &     nodeDepth, 
     &     thermCondPar1, 
     &     thermCondPar2, 
     &     thermCondPar3, 
     &     thermCondPar4, 
     &     pom, 
     &     soilRoughnessHeight, 
     &     nu, 
     &     boundarLayerConductanceSource, 
     &     netRadiationSource, 
     &     MissingValue, 
     &     soilConstituentNames, 
     &     InitialValues, 
     &     doInitialisationStuff, 
     &     internalTimeStep, 
     &     timeOfDaySecs, 
     &     numNodes, 
     &     numLayers, 
     &     volSpecHeatSoil, 
     &     soilTemp, 
     &     morningSoilTemp, 
     &     heatStorage, 
     &     thermalcon, 
     &     thermalConductivity, 
     &     boundaryLayerConductance, 
     &     newTemperature, 
     &     airTemperature, 
     &     maxTempYesterday, 
     &     minTempYesterday, 
     &     soilWater, 
     &     minSoilTemp, 
     &     maxSoilTemp, 
     &     aveSoilTemp, 
     &     aveSoilWater, 
     &     thickness, 
     &     bulkDensity, 
     &     rocks, 
     &     carbon, 
     &     sand, 
     &     silt, 
     &     clay, 
     &     instrumentHeight, 
     &     netRadiation, 
     &     canopyHeight, 
     &     instrumHeight)

!*********************************************************************** 
!     RATE
!***********************************************************************
      ELSEIF (INPITF % DYNAMIC .EQ. RATE) THEN

        CALL model_soiltemperature_APSIM(
     &     INPITF % TMIN, !weather_MinT,
     &     INPITF % TMAX, !weather_MaxT, 
     &     INPITF % T2M, !weather_MeanT, 
     &     INPITF % TAV, !weather_Tav, 
     &     INPITF % TAMP, !weather_Amp,
     &     weather_AirPressure, 
     &     weather_Wind, 
     &     INPITF % XLAT, !weather_Latitude, 
     &     INPITF % SRAD, !weather_Radn, 
     &     clock_Today_DayOfYear, 
     &     microClimate_CanopyHeight, 
     &     physical_Thickness,
     &     physical_BD, 
     &     ps, 
     &     physical_Rocks, 
     &     physical_ParticleSizeSand, 
     &     physical_ParticleSizeSilt, 
     &     physical_ParticleSizeClay, 
     &     organic_Carbon, 
     &     waterBalance_SW, 
     &     INPITF % ESP, !waterBalance_Eos, 
     &     INPITF % EOAD, !waterBalance_Eo, 
     &     INPITF % ES, !waterBalance_Es, 
     &     INPITF % SALB, !waterBalance_Salb, 
     &     InitialValues, 
     &     pInitialValues, 
     &     DepthToConstantTemperature, 
     &     timestep, 
     &     latentHeatOfVapourisation, 
     &     stefanBoltzmannConstant, 
     &     airNode, 
     &     surfaceNode, 
     &     topsoilNode, 
     &     numPhantomNodes, 
     &     constantBoundaryLayerConductance, 
     &     numIterationsForBoundaryLayerConductance, 
     &     defaultTimeOfMaximumTemperature, 
     &     defaultInstrumentHeight, 
     &     bareSoilRoughness, 
     &     doInitialisationStuff, 
     &     internalTimeStep, 
     &     timeOfDaySecs, 
     &     numNodes, 
     &     numLayers, 
     &     nodeDepth, 
     &     thermCondPar1, 
     &     thermCondPar2, 
     &     thermCondPar3, 
     &     thermCondPar4, 
     &     volSpecHeatSoil, 
     &     soilTemp, 
     &     morningSoilTemp, 
     &     heatStorage, 
     &     thermalcon,
     &     thermalConductivity, 
     &     boundaryLayerConductance, 
     &     newTemperature, 
     &     airTemperature, 
     &     maxTempYesterday, 
     &     minTempYesterday, 
     &     soilWater, 
     &     minSoilTemp, 
     &     maxSoilTemp, 
     &     aveSoilTemp, 
     &     aveSoilWater, 
     &     thickness, 
     &     bulkDensity, 
     &     rocks, 
     &     carbon, 
     &     sand, 
     &     pom, 
     &     silt, 
     &     clay, 
     &     soilRoughnessHeight, 
     &     instrumentHeight, 
     &     netRadiation, 
     &     canopyHeight, 
     &     instrumHeight, 
     &     nu, 
     &     boundarLayerConductanceSource, 
     &     netRadiationSource, 
     &     MissingValue, 
     &     soilConstituentNames)
     
        OUTITF % TSLX(0:INPITF%NLAYR) = maxSoilTemp(3:INPITF%NLAYR+3)
        OUTITF % TSLN(0:INPITF%NLAYR) = minSoilTemp(3:INPITF%NLAYR+3)
        OUTITF % TSLD(0) = aveSoilTemp(2)
        OUTITF % TSLD(1:INPITF%NLAYR) = aveSoilTemp(3:INPITF%NLAYR+2)

      ENDIF
C***********************************************************************      

      RETURN
      END SUBROUTINE
C=======================================================================