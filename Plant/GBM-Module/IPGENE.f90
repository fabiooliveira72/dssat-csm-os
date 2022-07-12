!=======================================================================
!  IPGENE, Subroutine, Fabio Oliveira
!  Reads Genetic input data for Gene-based module.
!-----------------------------------------------------------------------
!  REVISION   HISTORY
!-----------------------------------------------------------------------
!     Calls:          
!======================================================================= 

      SUBROUTINE IPGENE(FILEIO, QTL, GENID)                !Input/Output
                                    
!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
                         
      IMPLICIT NONE

      CHARACTER*6  SECTION,ERRKEY
      CHARACTER*6  GEN, GENID
      CHARACTER*12 FILEG
      CHARACTER*30 FILEIO
      CHARACTER*80 PATHGN
      CHARACTER*92 FILEGN

      INTEGER LUNIO, LUNGN
      INTEGER ERRNUM, FOUND, LINC, LNUM, TRTNUM

      LOGICAL HEADER
      
      PARAMETER (ERRKEY = 'IPGENE')
      
      REAL, DIMENSION(70) :: QTL   !Vector of genotype (INTEGER) markers / QTLs      
            
!----------------------------------------------------------------------- 
!     Read in values from input file.
!-----------------------------------------------------------------------
      CALL GETLUN('FILEIO', LUNIO)
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)
      
!-----------------------------------------------------------------------
!    Read FILE names and paths from file CUL
!-----------------------------------------------------------------------
      READ(LUNIO,'(////////,15X,A12,1X,A80)', IOSTAT=ERRNUM) FILEG, PATHGN
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LUNIO)
      
!-----------------------------------------------------------------------
!    Find and Read first Cultivar Section
!-----------------------------------------------------------------------
      SECTION = '*CULTI'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) CALL ERROR (SECTION, 42, FILEIO,LNUM)
      READ(LUNIO,'(6X,A6)', IOSTAT=ERRNUM) GENID ; LNUM = LNUM +1
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)

      CLOSE(LUNIO)
      
!-----------------------------------------------------------------------
!    Find and Read Genetic file      
!-----------------------------------------------------------------------
      FILEG(9:12) = '.GEN'
      FILEGN = TRIM(PATHGN) // FILEG
      CALL GETLUN('FILEGN', LUNGN)
      
      OPEN (LUNGN, FILE = FILEGN, STATUS = 'OLD', IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEGN,0)

      FOUND = 0
      HEADER = .TRUE.
      DO WHILE (FOUND .EQ. 0)
        
        IF(HEADER) THEN
          READ(LUNGN,'(A6)', IOSTAT=ERRNUM) GEN
          IF(ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEGN,0)
          HEADER = .FALSE.
        ENDIF
        
        IF(.NOT. HEADER) THEN
          READ(LUNGN,'(A6, 17x, 12F6.0)', IOSTAT=ERRNUM) &
            GEN, QTL(1),QTL(2),QTL(3),QTL(4),QTL(5),QTL(6),QTL(7),QTL(8),QTL(9),QTL(10),QTL(11),QTL(12)
          IF(INDEX('!',GEN) .GT. 0) THEN
            READ(LUNGN,'(A6, 17x, 12F6.0)', IOSTAT=ERRNUM) &
              GEN, QTL(1),QTL(2),QTL(3),QTL(4),QTL(5),QTL(6),QTL(7),QTL(8),QTL(9),QTL(10),QTL(11),QTL(12)
            IF(ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEGN,0)
          ENDIF
        ENDIF
        
        
        IF( GEN .EQ. GENID) THEN          
          FOUND = 1
          EXIT
        ENDIF
        
      END DO
      
      CLOSE(LUNGN)
!-----------------------------------------------------------------------
      RETURN
      END !SUBROUTINE IPGENE
!-----------------------------------------------------------------------            
!***********************************************************************