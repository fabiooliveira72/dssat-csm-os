C=======================================================================
      SUBROUTINE INTF_C2ML_DSSAT(INPITF, OUTITF)

      USE ModuleDefs
      USE InterfaceDataMod
      IMPLICIT NONE
      EXTERNAL YR_DOY, init_stemp, model_stemp
      SAVE
      
!     Control and data structures
      TYPE (INTFDataInput) INPITF
      TYPE (INTFDataOutput) OUTITF

!     C2ML DSSAT model variables
      CHARACTER ISWWAT
      INTEGER YR, DOY
      REAL HDAY, CUMDPT, TDL, ATOT
      REAL, DIMENSION(5)  :: TMA
      REAL, DIMENSION(NL) :: DSMID
      
!*********************************************************************** 
!     INTERFACE INDIVIDUAL ATTRIBUTIONS
!***********************************************************************
      ISWWAT = 'Y'
      
      CALL YR_DOY(INPITF % YRDOY, YR, DOY)
!*********************************************************************** 
!     SEASINIT
!***********************************************************************
      IF (INPITF % DYNAMIC .EQ. SEASINIT) THEN
        CALL init_stemp(NL, ISWWAT,                                    
     &            INPITF % SLBDM,                                      
     &            INPITF % THICK(1:INPITF % NLAYR),                    
     &            INPITF % SLLB(1:INPITF % NLAYR),                       
     &            INPITF % SLDUL, INPITF % SLLL,                       
     &            INPITF % NLAYR, INPITF % SALB,                       
     &            INPITF % SRAD, INPITF % SWLD, INPITF % T2M,           
     &            INPITF % TMAX, INPITF % XLAT, INPITF % TAV,          
     &            INPITF % TAMP,                                       
     &            DOY,                                                 
     &            CUMDPT,                                              
     &            DSMID,                                               
     &            TDL,                                                 
     &            TMA,                                                 
     &            ATOT,                                                
     &            OUTITF % TSLD(0), OUTITF % TSLD(1:INPITF % NLAYR),   
     &            HDAY)
!*********************************************************************** 
!     RATE
!***********************************************************************
      ELSEIF (INPITF % DYNAMIC .EQ. RATE) THEN
        CALL model_stemp(NL, ISWWAT,                                   
     &            INPITF % SLBDM,                                      
     &            INPITF % THICK(1:INPITF % NLAYR),                    
     &            INPITF % SLLB(1:INPITF % NLAYR),                       
     &            INPITF % SLDUL, INPITF % SLLL,                       
     &            INPITF % NLAYR, INPITF % SALB,                       
     &            INPITF % SRAD, INPITF % SWLD, INPITF % T2M,           
     &            INPITF % TMAX, INPITF % XLAT, INPITF % TAV,          
     &            INPITF % TAMP,                                       
     &            CUMDPT,                                              
     &            DSMID,                                               
     &            TDL,                                                 
     &            TMA,                                                 
     &            ATOT,                                                
     &            OUTITF % TSLD(0), OUTITF % TSLD(1:INPITF % NLAYR),   
     &            DOY, HDAY)
        OUTITF % TSLX = -99.0
        OUTITF % TSLN = -99.0
      ENDIF
C***********************************************************************      
      RETURN
      END SUBROUTINE
C=======================================================================