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
      ! Double precision comes from SoilTemeprature apsim mod above
      !INTEGER, PARAMETER :: DP = SELECTED_REAL_KIND(15)

      REAL(dp) weather_MinT      
      REAL(dp) weather_MaxT      
      REAL(dp) weather_MeanT     
      REAL(dp) weather_Tav       
      REAL(dp) weather_Amp       
      REAL(dp) weather_Latitude  
      REAL(dp) weather_Radn      
      REAL(dp) waterBalance_Eos  
      REAL(dp) waterBalance_Eo   
      REAL(dp) waterBalance_Es   
      REAL(dp) waterBalance_Salb
      REAL(dp) weather_AirPressure, weather_Wind
      INTEGER clock_Today_DayOfYear
      REAL(dp) microClimate_CanopyHeight
      REAL(dp) ps
      REAL(dp), DIMENSION(1:INPITF%NLAYR) :: physical_Rocks
      REAL(dp), DIMENSION(1:INPITF%NLAYR) :: physical_ParticleSizeSand
      REAL(dp), DIMENSION(1:INPITF%NLAYR) :: physical_ParticleSizeSilt
      REAL(dp), DIMENSION(1:INPITF%NLAYR) :: physical_ParticleSizeClay
      REAL(dp), ALLOCATABLE :: waterBalance_SW(:)
      REAL(dp), DIMENSION(1:INPITF%NLAYR) :: organic_Carbon
      REAL(dp), DIMENSION(1:INPITF%NLAYR) :: physical_Thickness
      REAL(dp), DIMENSION(1:INPITF%NLAYR) :: physical_BD
      REAL(dp), DIMENSION(:), ALLOCATABLE :: pInitialValues
      REAL(dp) DepthToConstantTemperature
      REAL(dp) timestep
      REAL(dp) latentHeatOfVapourisation
      REAL(dp) stefanBoltzmannConstant
      INTEGER airNode
      INTEGER surfaceNode
      INTEGER topsoilNode
      INTEGER numPhantomNodes
      REAL(dp) constantBoundaryLayerConductance
      INTEGER numIterationsForBoundaryLayerConductance
      REAL(dp) defaultTimeOfMaximumTemperature
      REAL(dp) defaultInstrumentHeight
      REAL(dp) bareSoilRoughness
      REAL(dp), ALLOCATABLE :: nodeDepth(:)
      REAL(dp), ALLOCATABLE :: thermCondPar1(:)
      REAL(dp), ALLOCATABLE :: thermCondPar2(:)
      REAL(dp), ALLOCATABLE :: thermCondPar3(:)
      REAL(dp), ALLOCATABLE :: thermCondPar4(:)
      REAL(dp) pom
      REAL(dp) soilRoughnessHeight
      REAL(dp) nu
      CHARACTER(65) boundarLayerConductanceSource
      CHARACTER(65) netRadiationSource
      REAL(dp) MissingValue
      CHARACTER(65) , DIMENSION(dp) :: soilConstituentNames
      REAL(dp), DIMENSION(:), ALLOCATABLE :: InitialValues
      LOGICAL doInitialisationStuff
      REAL(dp) internalTimeStep
      REAL(dp) timeOfDaySecs
      INTEGER numNodes
      INTEGER numLayers
      REAL(dp), ALLOCATABLE :: volSpecHeatSoil(:)
      REAL(dp), ALLOCATABLE :: soilTemp(:)
      REAL(dp), ALLOCATABLE :: morningSoilTemp(:)
      REAL(dp), ALLOCATABLE :: heatStorage(:)
      REAL(dp), ALLOCATABLE :: thermalcon(:)
      REAL(dp), ALLOCATABLE :: thermalConductivity(:)
      REAL(dp) boundaryLayerConductance
      REAL(dp) , DIMENSION(: ), ALLOCATABLE :: newTemperature
      REAL(dp) airTemperature
      REAL(dp) maxTempYesterday
      REAL(dp) minTempYesterday
      REAL(dp), ALLOCATABLE :: soilWater(:)
      REAL(dp), ALLOCATABLE :: minSoilTemp(:)
      REAL(dp), ALLOCATABLE :: maxSoilTemp(:)
      REAL(dp), ALLOCATABLE :: aveSoilTemp(:)
      REAL(dp), ALLOCATABLE :: aveSoilWater(:)
      REAL(dp), ALLOCATABLE :: thickness(:)
      REAL(dp), ALLOCATABLE :: bulkDensity(:)
      REAL(dp), ALLOCATABLE :: rocks(:)
      REAL(dp), ALLOCATABLE :: carbon(:)
      REAL(dp), ALLOCATABLE :: sand(:)
      REAL(dp), ALLOCATABLE :: silt(:)
      REAL(dp), ALLOCATABLE :: clay(:)
      REAL(dp) instrumentHeight
      REAL(dp) netRadiation
      REAL(dp) canopyHeight
      REAL(dp) instrumHeight
      INTEGER YR, DOY
!*********************************************************************** 
!     INTERFACE INDIVIDUAL ATTRIBUTIONS
!***********************************************************************
      CALL YR_DOY(INPITF % YRDOY, YR, DOY)
      clock_Today_DayOfYear = DOY
      weather_MinT      = INPITF % TMIN
      weather_MaxT      = INPITF % TMAX
      weather_MeanT     = INPITF % T2M
      weather_Tav       = INPITF % TAV
      weather_Amp       = INPITF % TAMP
      weather_Latitude  = INPITF % XLAT
      weather_Radn      = INPITF % SRAD
      waterBalance_Eos  = INPITF % ESP
      waterBalance_Eo   = INPITF % EOAD
      waterBalance_Es   = INPITF % ES 
      waterBalance_Salb = INPITF % SALB
      physical_ParticleSizeSand = INPITF%SLSND(1:INPITF%NLAYR)
      physical_ParticleSizeSilt = INPITF%SLSIL(1:INPITF%NLAYR)
      physical_ParticleSizeClay = INPITF%SLCLY(1:INPITF%NLAYR)
      physical_Thickness = INPITF % THICK(1:INPITF%NLAYR) * 10
      physical_BD = INPITF % SLBDM(1:INPITF%NLAYR)
      organic_Carbon = INPITF % SLOC(1:INPITF%NLAYR)
      waterBalance_SW = INPITF % SWLD(1:INPITF%NLAYR)
      weather_AirPressure = 1010.0
      weather_Wind = 3.0
      microClimate_CanopyHeight = 0.0
      physical_Rocks = 0.0
      timestep = 24 * 60 * 60
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
      internalTimestep = 0.0
!*********************************************************************** 
!     SEASINIT
!***********************************************************************
      IF (INPITF % DYNAMIC .EQ. SEASINIT) THEN

        doInitialisationStuff = .FALSE.
        instrumentHeight = 0.0 
        soilRoughnessHeight = 0.0 
        ps = 2.63
        nu = 0.6
        DepthToConstantTemperature = 10000.0
        latentHeatOfVapourisation = 2465000.0
        stefanBoltzmannConstant = 0.0000000567
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
        
        CALL init_soiltemperature_APSIM(
     &     weather_MinT, 
     &     weather_MaxT, 
     &     weather_MeanT, 
     &     weather_Tav, 
     &     weather_Amp, 
     &     weather_AirPressure, 
     &     weather_Wind, 
     &     weather_Latitude, 
     &     weather_Radn, 
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
     &     waterBalance_Eos, 
     &     waterBalance_Eo, 
     &     waterBalance_Es, 
     &     waterBalance_Salb, 
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
     &     weather_MinT,
     &     weather_MaxT, 
     &     weather_MeanT, 
     &     weather_Tav, 
     &     weather_Amp,
     &     weather_AirPressure, 
     &     weather_Wind, 
     &     weather_Latitude, 
     &     weather_Radn, 
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
     &     waterBalance_Eos, 
     &     waterBalance_Eo, 
     &     waterBalance_Es, 
     &     waterBalance_Salb, 
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
     
        OUTITF % TSLX(0:INPITF%NLAYR) = maxSoilTemp(2)
        OUTITF % TSLN(0:INPITF%NLAYR) = minSoilTemp(2)
        OUTITF % TSLD(0) = aveSoilTemp(2)
        OUTITF % TSLX(1:INPITF%NLAYR) = maxSoilTemp(3:INPITF%NLAYR+2)
        OUTITF % TSLN(1:INPITF%NLAYR) = minSoilTemp(3:INPITF%NLAYR+2)
        OUTITF % TSLD(1:INPITF%NLAYR) = aveSoilTemp(3:INPITF%NLAYR+2)

      ENDIF
C***********************************************************************      

      RETURN
      END SUBROUTINE
C=======================================================================