C=======================================================================
      SUBROUTINE INTF_C2ML_SIRIUS(INPITF, OUTITF)

      USE ModuleDefs
      USE ModuleData
      USE InterfaceDataMod
      USE Soiltemperaturemod_SIRIUS
      IMPLICIT NONE
      SAVE

!     Control and data structures
      TYPE (INTFDataInput) INPITF
      TYPE (INTFDataOutput) OUTITF

!     Control variables for SIRIUS
      REAL deepLayerT, deepLayerT_t1, lambda_, a,b,c
      REAL maxTSoil, minTSoil
      REAL, DIMENSION(24) :: hourlySoilT

!*********************************************************************** 
!     INTERFACE INDIVIDUAL ATTRIBUTIONS
!***********************************************************************
      lambda_ = 2.454
      hourlySoilT = 0.0
      a = 0.0
      b = 0.0
      c = 0.0
      OUTITF % TSLX = -99.0
      OUTITF % TSLN = -99.0
!*********************************************************************** 
!     SEASINIT
!***********************************************************************
      IF (INPITF % DYNAMIC .EQ. SEASINIT) THEN
            
            deepLayerT = INPITF % TAV
            deepLayerT_t1 = deepLayerT
!*********************************************************************** 
!     RATE
!***********************************************************************
      ELSEIF (INPITF % DYNAMIC .EQ. RATE) THEN

            CALL model_soiltemperature(
     &            deepLayerT,
     &            lambda_, ! lambda_
     &            INPITF % G, ! heatFlux (g/m2/d)
     &            INPITF % T2M,  ! meanTAir
     &            INPITF % TMIN, ! minTAir
     &            INPITF % TMAX, ! maxTAir
     &            a, ! a
     &            b, ! b
     &            c, ! c
     &            INPITF % DAYLD, ! dayLength
     &            deepLayerT_t1, ! deepLayerT_t1
     &            maxTSoil, ! maxTSoil
     &            minTSoil, ! minTSoil
     &            hourlySoilT) ! hourlySoilT

            deepLayerT = deepLayerT_t1
            
            OUTITF % TSLD(0) = -99
            OUTITF % TSLD(1) = (maxTsoil + minTSoil)/2
            OUTITF % TSLD(2:INPITF % NLAYR) = deepLayerT_t1
            OUTITF % TSLX(1) = maxTSoil
            OUTITF % TSLN(1) = minTSoil
      ENDIF
C***********************************************************************      

      RETURN
      END SUBROUTINE
C=======================================================================