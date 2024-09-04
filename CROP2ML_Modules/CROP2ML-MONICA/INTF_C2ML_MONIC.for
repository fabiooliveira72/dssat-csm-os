C=======================================================================
      SUBROUTINE INTF_C2ML_MONIC(INPITF, OUTITF)

      USE ModuleDefs
      USE ModuleData
      USE InterfaceDataMod
      USE Soiltemperaturecompmod
      IMPLICIT NONE
      SAVE

!     Control and data structures
      TYPE (INTFDataInput) INPITF
      TYPE (INTFDataOutput) OUTITF
      REAL timeStep, baseTemp
      REAL initialSurfaceTemp, densityAir, densityHumus
      REAL specificHeatCapacityHumus, densityWater
      REAL specificHeatCapacityAir
      REAL specificHeatCapacityWater, quartzRawDensity
      REAL specificHeatCapacityQuartz, nTau
      INTEGER noOfTempLayers, noOfSoilLayers, noOfTempLayersPlus1
      INTEGER I, LTK, TMPLY
      REAL, DIMENSION(:), ALLOCATABLE :: layerThickness
      REAL, DIMENSION(:), ALLOCATABLE :: soilBulkDensity
      REAL, DIMENSION(:), ALLOCATABLE :: saturation
      REAL, DIMENSION(:), ALLOCATABLE :: soilOrganicMatter
      REAL, DIMENSION(:), ALLOCATABLE :: soilMoistureConst
      REAL, DIMENSION(:), ALLOCATABLE :: V
      REAL, DIMENSION(:), ALLOCATABLE :: B
      REAL, DIMENSION(:), ALLOCATABLE :: volumeMatrix
      REAL, DIMENSION(:), ALLOCATABLE :: volumeMatrixOld
      REAL, DIMENSION(:), ALLOCATABLE :: matrixPrimaryDiagonal
      REAL, DIMENSION(:), ALLOCATABLE :: matrixSecondaryDiagonal
      REAL, DIMENSION(:), ALLOCATABLE :: heatConductivity
      REAL, DIMENSION(:), ALLOCATABLE :: heatConductivityMean
      REAL, DIMENSION(:), ALLOCATABLE :: heatCapacity
      REAL, DIMENSION(:), ALLOCATABLE :: solution 
      REAL, DIMENSION(:), ALLOCATABLE :: matrixDiagonal
      REAL, DIMENSION(:), ALLOCATABLE :: matrixLowerTriangle
      REAL, DIMENSION(:), ALLOCATABLE :: heatFlow
      REAL, DIMENSION(:), ALLOCATABLE :: soilTemperature
      REAL dampingFactor, soilCoverage, auxstemp
      REAL soilSurfaceTemperatureBelowSnow
      LOGICAL hasSnowCover
!*********************************************************************** 
!     INTERFACE INDIVIDUAL ATTRIBUTIONS
!***********************************************************************
      timeStep = 1.0
      soilCoverage = 1 - EXP(-0.5 * INPITF % LAID);
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
      noOfTempLayers = 44
      noOfSoilLayers = 42
      noOfTempLayersPlus1 = 45
      hasSnowCover = .FALSE.

!*********************************************************************** 
!     SEASINIT
!***********************************************************************
      IF (INPITF % DYNAMIC .EQ. SEASINIT) THEN
            
            IF(.NOT. allocated(layerThickness)) THEN
              allocate(layerThickness(noOfTempLayers))
              allocate(soilBulkDensity(noOfSoilLayers))
              allocate(saturation(noOfSoilLayers))
              allocate(soilOrganicMatter(noOfSoilLayers))
              allocate(soilMoistureConst(noOfSoilLayers))
              allocate(V(noOfTempLayers))
              allocate(volumeMatrix(noOfTempLayers))
              allocate(volumeMatrixOld(noOfTempLayers))
              allocate(B(noOfTempLayers))
              allocate(matrixPrimaryDiagonal(noOfTempLayers))
              allocate(matrixSecondaryDiagonal(noOfTempLayersPlus1))
              allocate(heatConductivity(noOfTempLayers))
              allocate(heatConductivityMean(noOfTempLayers))
              allocate(heatCapacity(noOfTempLayers))
              allocate(solution(noOfTempLayers))
              allocate(matrixDiagonal(noOfTempLayers))
              allocate(matrixLowerTriangle(noOfTempLayers))
              allocate(heatFlow(noOfTempLayers))
              allocate(soilTemperature(noOfTempLayers))
            ENDIF

            ! Thickness is m. However every layer is 5cm
            layerThickness = 0.05
            
            TMPLY= 1
            DO I = 1, INPITF % NLAYR
              LTK = INPITF % THICK(I) / 5
              DO WHILE (LTK > 0 .AND. TMPLY <= noOfSoilLayers)
                saturation(TMPLY) = INPITF % SLSAT(I)
                !Convert from g/cm3 to kg/m3
                soilBulkDensity(TMPLY) = INPITF % SLBDM(I) * 1000
                soilOrganicMatter(TMPLY) =(INPITF % SLOC(I)/0.57)/100
                soilMoistureConst(TMPLY)= INPITF % SLLL(I) + 
     &              INPITF % AWC * (INPITF%SLDUL(I) - INPITF%SLLL(I))
                
                LTK = LTK - 1
                TMPLY = TMPLY + 1
              ENDDO
            ENDDO
            
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
     &        OUTITF % TSLD(0), ! soilSurfaceTemperature, 
     &        soilTemperature, 
     &        V, 
     &        B, 
     &        volumeMatrix, 
     &        volumeMatrixOld, 
     &        matrixPrimaryDiagonal, 
     &        matrixSecondaryDiagonal, 
     &        heatConductivity, 
     &        heatConductivityMean, 
     &        heatCapacity, 
     &        solution, 
     &        matrixDiagonal, 
     &        matrixLowerTriangle, 
     &        heatFlow)
!*********************************************************************** 
!     RATE
!***********************************************************************
      ELSEIF (INPITF % DYNAMIC .EQ. RATE) THEN

          CALL model_soiltemperaturecomp(
     &            INPITF % TMIN, ! tmin
     &            INPITF % TMAX, ! tmax
     &            INPITF % SRAD, ! globrad
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
     &            B, 
     &            volumeMatrix, 
     &            volumeMatrixOld, 
     &            matrixPrimaryDiagonal, 
     &            matrixSecondaryDiagonal, 
     &            heatConductivity, 
     &            heatConductivityMean, 
     &            heatCapacity, 
     &            solution, 
     &            matrixDiagonal, 
     &            matrixLowerTriangle, 
     &            heatFlow, 
     &            OUTITF % TSLD(0), ! soilSurfaceTemperature
     &            soilTemperature)

            ! Matching SQ outputs for ST.
            auxstemp = 0
            DO I = 1, noOfSoilLayers
              auxstemp = auxstemp + soilTemperature(I) 
              SELECT CASE(I)
                CASE(1)
                  OUTITF % TSLD(1) = auxstemp
                  auxstemp = 0
                CASE(3)
                  OUTITF % TSLD(2) = auxstemp / 2
                  auxstemp = 0
                CASE(6)
                  OUTITF % TSLD(3) = auxstemp / 3
                  auxstemp = 0
                CASE(9)
                  OUTITF % TSLD(4) = auxstemp / 3
                  auxstemp = 0
                CASE(12)
                  OUTITF % TSLD(5) = auxstemp / 3
                  auxstemp = 0
                CASE(18)
                  OUTITF % TSLD(6) = auxstemp / 6
                  auxstemp = 0
                CASE(24)
                  OUTITF % TSLD(7) = auxstemp / 6
                  auxstemp = 0
                CASE(30)
                  OUTITF % TSLD(8) = auxstemp / 6
                  auxstemp = 0
                CASE(36)
                  OUTITF % TSLD(9) = auxstemp / 6
                  auxstemp = 0
                CASE(42)
                  OUTITF % TSLD(10) = auxstemp / 6
                  auxstemp = 0
                CASE DEFAULT
              END SELECT
            ENDDO
            
            OUTITF % TSLX = -99.0
            OUTITF % TSLN = -99.0      

      ENDIF
C***********************************************************************      

      RETURN
      END SUBROUTINE
C=======================================================================