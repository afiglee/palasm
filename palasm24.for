C**PALASM24**PALASM24**PALASM24**PALASM24**PALASM24**PALASM24**PALASM24*
C
C  P A L A S M  2 4  -  TRANSLATES SYMBOLIC EQUATIONS INTO PAL OBJECT
C                       CODE FORMATTED FOR DIRECT INPUT TO STANDARD
C                       PROM AND PAL PROGRAMMERS.
C
C
C                       REV LEVEL:   VERSION 1.6C  (08/16/83)  
C                       (C)  COPYRIGHT 1983 MONOLITHIC MEMORIES
C
C                                  
C                       ***********************************************
C                       *                                             *
C                       *                  APPROVAL                   *
C                       *                                             *
C                       *                                             *
C                       *   1: JOHN BIRKNER                           *
C                       *                                             *
C                       *  PROGRAMMABLE LOGIC PLANNER                 *
C                       *                                             *
C               	*                                             *
C                       *   2: VINCENT COLI                           *
C	                *                                             *
C                       *  APPLICATIONS ENGINEER                      *
C                       *                                             *
C                       *                                             *
C                       *   3: MANOUCHEHR VAFAI                       *
C                       *                                             *
C                       *  APPLICATIONS ENGINEER                      *
C                       *                                             *
C                       *                                             *
C                       *********************************************** 
C
C
C                       INPUT:       PAL DESIGN SPECIFICATION ASSIGNED
C                                    TO RPD(1).  OPERATION CODES ARE
C                                    ASSIGNED TO ROP(5).
C
C                       OUTPUT:      ECHO, SIMULATION, AND FUSE PATTERN
C                                    ARE ASSIGNED TO POF(6).  JEDEC, HEX,
C                                    AND BINARY PROGRAMMING FORMATS ARE
C                                    ASSIGNED TO PDF(6).  PROMPTS AND
C                                    ERROR MESSAGES ARE ASSIGNED TO
C                                    PMS(6).
C
C                       PART NUMBER: THE PAL PART NUMBER MUST APPEAR
C                                    IN COLUMN ONE OF LINE ONE.
C
C                       PIN LIST:    24 SYMBOLIC PIN NAMES MUST APPEAR
C                                    STARTING ON LINE FIVE.
C
C                       EQUATIONS:   STARTING FIRST LINE AFTER THE
C                                    PIN LIST IN THE FOLLOWING FORMS:
C
C                                       A = B*C + D
C
C                                       A := B*C + D
C
C
C                                       IF( A*B )  C = D + E
C
C                                    ALL CHARACTERS FOLLOWING ';' ARE
C                                    IGNORED UNTIL THE NEXT LINE.
C
C                                    BLANKS ARE IGNORED.
C                       OPERATORS:   ( IN HIERARCHY OF EVALUATION )
C
C                                     ;    COMMENT FOLLOWS
C                                     /    COMPLEMENT
C                                     *    AND (PRODUCT)
C                                     +    OR (SUM)
C                                    :+:   XOR (EXCLUSIVE OR)
C                                    ( )   CONDITIONAL THREE-STATE
C                                     =    EQUALITY
C                                    :=    REPLACED BY (AFTER CLOCK)
C
C                       FUNCTION     L, H, X, Z, AND C ARE VALID
C                         TABLE:     FUNCTION TABLE VECTOR ENTRIES.
C
C                       REFERENCE:   A COMPLETE USERS GUIDE TO
C                                    DESIGNING WITH PALS USING PALASM
C                                    IS PROVIDED IN THE MONOLITHIC
C                                    MEMORIES PAL HANDBOOK.
C
C                       SUBROUTINES: INITLZ,GETSYM,INCR,MATCH,IXLATE,
C                                    ECHO,CAT,PINOUT,PLOT,PLOTF,SUMCHK
C                                    ICONV,HEX,SUMCHK,TWEEK,BINR,SLIP,
C                                    FANTOM,IODC2,IODC4,TEST,INTEL
C
C
C
C                       AUTHORS:     JOHN BIRKNER AND VINCENT COLI
C                                    FAULT TESTING BY IMTIYAZ BENGALI
C                                    REVISED JEDEC FORMAT BY MANO VAFAI
C                                    MONOLITHIC MEMORIES INC.
C                                    1165 EAST ARQUES AVENUE
C                                    SUNNYVALE, CALIFORNIA 94043
C                                    (408) 739-3535
C
C                       FINE PRINT:  MONOLITHIC MEMORIES TAKES NO
C                                    RESPONSIBILITY FOR THE OPERATION
C                                    OR MAINTENANCE OF THIS PROGRAM.
C                                    THE SOURCE CODE AS PRINTED HERE
C                                    PRODUCED THE OBJECT CODE OF THE
C                                    EXAMPLES IN THE APPLICATIONS
C                                    SECTION ON A VAX/VMS 11/780
C                                    COMPUTER WITH FORTRAN 77 AND A
C                                    NATIONAL CSS IBM SYSTEM/370
C                                    WITH FORTRAN IV (LEVEL G).
C
C***********************************************************************
C
C
C     MAIN PROGRAM
C
      IMPLICIT INTEGER (A-Z)
      INTEGER IPAL(3),INAME(5),REST(72),PATNUM(80),TITLE(80),COMP(80),
     1        ISYM(8,24),IBUF(8,24),JPROD(80)
C
      LOGICAL LBLANK,LLEFT,LAND,LOR,LSLASH,LEQUAL,LRIGHT,LXOR,LFIRST,
     1        LMATCH,LFUSES(40,80),LPHASE(24),LBUF(24),LPROD(80),
     2        LSAME,LACT,LOPERR,LINP,LERR,LSA11,LSA01,LPHANT(40,80)
      INTEGER BEL
      COMMON  LBLANK,LLEFT,LAND,LOR,LSLASH,LEQUAL,LRIGHT,LXOR
      COMMON /PGE/ IPAGE(80,200)
      COMMON /LFUSES/LFUSES,LPHANT
      COMMON /FTEST/ IFUNCT,IDESC,IEND
      COMMON /LUNIT/ PMS,POF,PDF
      DATA E/'E'/,O/'O'/,T/'T'/,P/'P'/,B/'B'/,D/'D'/,H/'H'/,S/'S'/,
     1     L/'L'/,N/'N'/,C/'C'/,Q/'Q'/,U/'U'/,F/'F'/,Y/'Y'/
      DATA BB/'B'/,CC/'C'/,DD/'D'/,EE/'E'/,FF/'F'/,II/'I'/,NN/'N'/,
     1     OO/'O'/,PP/'P'/,RR/'R'/,SS/'S'/,TT/'T'/,UU/'U'/,JJ/'J'/
      DATA BEL/007/
C
C
C     ASSIGNMENT OF DATA SET REFERENCES 
C     RPD - PAL DESIGN SPECIFICATION (INPUT)
C     ROC - OPERATION CODE (INPUT)
C     POF - ECHO, PINOUT, TEST, AND PLOT (OUTPUT) 
C     PDF - JEDEC, HEX, AND BINARY PROGRAMMING FORMATS (OUTPUT)
C     PMS - PROMPTS AND ERROR MESSAGES (OUTPUT)
      WRITE(6,3)
    3 FORMAT(/,' MONOLITHIC MEMORIES 24-PIN PALASM VERSION 1.6C')
      WRITE(6,31)
   31 FORMAT(' (C) COPYRIGHT 1983 MONOLITHIC MEMORIES')
      WRITE(6,1)
    1 FORMAT(/,' WHAT IS THE LOGICAL UNIT NUMBER FOR OUTPUT(6)?: '$)
      READ(5,2) LUN
    2 FORMAT(I4)
      RPD=1
      ROC=5
      POF=LUN
      PDF=LUN
      PMS=LUN
      IFUNCT=0
      IDESC=0
C     INITIALIZE LSAME AND LACT TO FALSE (ACTIVE HIGH/LOW ERROR)
      LSAME=.FALSE.
      LACT=.FALSE.
C     INITIALIZE LOPERR TO FALSE (OUTPUT PIN ERROR)
      LOPERR=.FALSE.
C     READ IN FIRST 4 LINES OF PAL DESIGN SPECIFICATION
      READ(RPD,10) IPAL,INAME,REST,PATNUM,TITLE,COMP
   10 FORMAT(3A1,5A1,72A1,/,80A1,/,80A1,/,80A1)
C     READ IN PIN LIST (LINE 5) THROUGH THE END OF THE PAL DESIGN
C      SPECIFICATION
      DO 15 J=1,200
          READ(RPD,11,END=16) (IPAGE(I,J),I=1,80)
   11     FORMAT(80A1)
C         CHECK FOR 'FUNCTION TABLE' AND SAVE ITS LINE NUMBER
          IF(     IFUNCT.EQ.0 .AND.IPAGE(1,J).EQ.FF.AND.
     1        IPAGE(2,J).EQ.UU.AND.IPAGE(3,J).EQ.NN.AND.
     2        IPAGE(4,J).EQ.CC.AND.IPAGE(5,J).EQ.TT.AND.
     3        IPAGE(6,J).EQ.II.AND.IPAGE(7,J).EQ.OO.AND.
     4        IPAGE(8,J).EQ.NN.AND.IPAGE(10,J).EQ.TT.AND.
     5        IPAGE(12,J).EQ.BB.AND.IPAGE(14,J).EQ.EE ) IFUNCT=J
C         CHECK FOR 'DESCRIPTION' AND SAVE ITS LINE NUMBER
C
          IF(      IDESC.EQ.0 .AND.IPAGE(1,J).EQ.DD.AND.
     1        IPAGE(2,J).EQ.EE.AND.IPAGE(3,J).EQ.SS.AND.
     2        IPAGE(4,J).EQ.CC.AND.IPAGE(5,J).EQ.RR.AND.
     3        IPAGE(6,J).EQ.II.AND.IPAGE(7,J).EQ.PP.AND.
     4        IPAGE(8,J).EQ.TT.AND.IPAGE(9,J).EQ.II.AND.
     5        IPAGE(10,J).EQ.OO.AND.IPAGE(11,J).EQ.NN ) IDESC=J
   15 CONTINUE
C     SAVE THE LAST LINE NUMBER OF THE PAL DESIGN SPECIFICATION
   16 IEND=J-1
      CALL INITLZ(INAME,ITYPE,LFUSES,LPHANT,IC,IL,IBLOW,IPCTR)
C     PRINT ERROR MESSAGE FOR INVALID PAL PART TYPE
      IF(ITYPE.NE.0) GO TO 17
      WRITE(PMS,18) IPAL,INAME
   18 FORMAT(/,' PAL PART TYPE ',3A1,5A1,' IS INCORRECT')
      CALL EXIT
C     GET 24 PIN NAMES
   17 DO 20 J=1,24
   20     CALL GETSYM(LPHASE,ISYM,J,IC,IL)
          IF(.NOT.(LEQUAL.OR.LLEFT.OR.LAND.OR.LOR.OR.LRIGHT)) GO TO 24
              WRITE(PMS,23)
   23         FORMAT(/, ' LESS THAN 24 PIN NAMES IN PIN LIST')
              CALL EXIT
   24 ILE=IL
C     BYPASS FUSE PLOT ASSEMBLY IF HAL ('H' IN LINE 1, COLUMN 1)
      IF( IPAL(1).EQ.H ) GO TO 108
   25 CALL GETSYM(LBUF,IBUF,1,IC,IL)
   28     IF(.NOT.LEQUAL) GO TO 25
          ILL=IL
          CALL MATCH(IMATCH,IBUF,ISYM)
          IF( IMATCH.EQ.0 ) GO TO 100
C         CHECK FOR VALID POLARITY (ACTIVE LOW)
          LSAME = ( (     LPHASE(IMATCH)).AND.(     LBUF(1)).OR.
     1              (.NOT.LPHASE(IMATCH)).AND.(.NOT.LBUF(1)) )
          IF( ITYPE.NE.6.AND.(LSAME) ) LACT=.TRUE.
C         CHECK FOR VALID OUTPUT PIN
   29     IF( (ITYPE.EQ.1.OR.ITYPE.EQ.7.OR.ITYPE.EQ.8.OR.ITYPE.EQ.9.OR.
     1         ITYPE.EQ.10).AND.(IMATCH.LT.14.OR.IMATCH.GT.23) )
     2         LOPERR=.TRUE.
          IF( (ITYPE.EQ.2.OR.ITYPE.EQ.11.OR.ITYPE.EQ.12.OR.ITYPE.EQ.13
     1         .OR.ITYPE.EQ.14).AND.(IMATCH.LT.15.OR.IMATCH.GT.22) )
     2         LOPERR=.TRUE.
          IF(  ITYPE.EQ.3.AND.(IMATCH.LT.16.OR.IMATCH.GT.21) )
     1         LOPERR=.TRUE.
          IF(  ITYPE.EQ.4.AND.(IMATCH.LT.17.OR.IMATCH.GT.20) )  
     1         LOPERR=.TRUE.
          IF( (ITYPE.EQ.5.OR.ITYPE.EQ.6).AND.
     1        (IMATCH.LT.18.OR.IMATCH.GT.19) ) LOPERR=.TRUE.
          IF( (LACT).OR.(LOPERR) ) GO TO 100
          I88PRO=(23-IMATCH)*8 + 1
C         START PAL20C1 ON PRODUCT LINE 32 (I88PRO=33)
          IF(INAME(3).EQ.C) I88PRO=33
          IC=0
   30       CALL INCR(IC,IL)
            IF( .NOT.(LEQUAL.OR.LLEFT) ) GO TO 30
            LPROD(I88PRO)=.TRUE.
            IF(.NOT.LLEFT) CALL SLIP(LFUSES,I88PRO,ITYPE,IBLOW)
C
          DO 70 I8PRO=1,16
              IF( (LXOR).AND.I8PRO.NE.3 ) GO TO 70
              IPROD = I88PRO + I8PRO - 1
              LPROD(IPROD)=.TRUE.
              LFIRST=.TRUE.
   50           ILL=IL
                CALL GETSYM(LBUF,IBUF,1,IC,IL)
                CALL MATCH(IMATCH,IBUF,ISYM)
C               CHECK FOR INVALID INPUT PIN
                IF( ITYPE.EQ.1.AND.(IMATCH.GE.14.AND.IMATCH.LE.23) )
     1              LINP=.TRUE.
                IF( ITYPE.EQ.2.AND.(IMATCH.GE.15.AND.IMATCH.LE.22) )
     1              LINP=.TRUE.
                IF( ITYPE.EQ.3.AND.(IMATCH.GE.16.AND.IMATCH.LE.21) )
     1              LINP=.TRUE.
                IF( ITYPE.EQ.4.AND.(IMATCH.GE.17.AND.IMATCH.LE.20) )
     1              LINP=.TRUE.
                IF( ITYPE.EQ.5.AND.(IMATCH.EQ.18.OR.IMATCH.EQ.19) )
     1              LINP=.TRUE.
                IF( ITYPE.EQ.6.AND.(IMATCH.EQ.18.OR.IMATCH.EQ.19) )
     1              LINP=.TRUE.
                IF( ITYPE.EQ.7.AND.(IMATCH.EQ.14.OR.IMATCH.EQ.23) )
     1              LINP=.TRUE.
                IF( ITYPE.EQ.8..AND.(IMATCH.EQ.1.OR.IMATCH.EQ.13) )
     1              LINP=.TRUE.
                IF( ITYPE.EQ.9..AND.(IMATCH.EQ.1.OR.IMATCH.EQ.13) )
     1              LINP=.TRUE.
                IF( ITYPE.EQ.10..AND.(IMATCH.EQ.1.OR.IMATCH.EQ.13) )
     1              LINP=.TRUE.
                IF( ITYPE.EQ.11.AND.(IMATCH.EQ.15.OR.IMATCH.EQ.22) )
     1              LINP=.TRUE.
                IF( ITYPE.GE.12.AND.(IMATCH.EQ.1.OR.IMATCH.EQ.13) )
     1              LINP=.TRUE.
                ILL=IL
                IF(LINP) GO TO 100
                IF(IMATCH.EQ.0) GO TO 100
                IF(IMATCH.EQ.12) GO TO 64
                IF(.NOT.LFIRST) GO TO 58
                    LFIRST=.FALSE.
                    DO 56 I=1,40
                        IBLOW = IBLOW + 1
   56                   LFUSES(I,IPROD)=.TRUE.
   58           CALL IXLATE(IINPUT,LPHASE,IMATCH,LBUF,ITYPE)
                IF(IINPUT.LE.0) GO TO 60
                IBLOW = IBLOW - 1
                LFUSES(IINPUT,IPROD)=.FALSE.
                CALL PLOT(LBUF,IBUF,LFUSES,IPROD,TITLE,.FALSE.,ITYPE,
     1                    LPROD,IOP,IBLOW)
   60           IF(LAND) GO TO 50
   64           IF(.NOT.LRIGHT) GO TO 68
   66           CALL INCR(IC,IL)
                IF(.NOT.LEQUAL) GO TO 66
   68         IF( .NOT.(LOR.OR.LEQUAL) ) GO TO 74
   70         CONTINUE
   74     ILL=IL
          CALL GETSYM(LBUF,IBUF,1,IC,IL)
C
           IF(LLEFT.OR.LEQUAL) GO TO 28
  100 IF( ILL.EQ.IFUNCT.OR.ILL.EQ.IDESC.OR.ILL.EQ.IEND ) GO TO 102
C     PRINT AN ERROR MESSAGE FOR AN UNRECOGNIZABLE SYMBOL
      ILERR=ILL+4
      WRITE(PMS,99) BEL
   99 FORMAT(' ',A1)
      WRITE(PMS,101) (IBUF(I,1),I=1,8),ILERR,(IPAGE(I,ILL),I=1,80)
  101 FORMAT(/,' ERROR SYMBOL =  ',8A1,'      IN LINE NUMBER ',I3,
     1       /,' ',80A1)
C     PRINT AN ERROR MESSAGE FOR ACTIVE HIGH/LOW ERRORS
      IF( (LACT).AND.(.NOT.LOPERR) ) WRITE(PMS,103) IPAL,INAME
  103 FORMAT(' OUTPUT MUST BE INVERTED SINCE ',3A1,5A1,
     1       ' IS AN ACTIVE LOW DEVICE')
C     PRINT AN ERROR MESSAGE FOR AN INVALID OUTPUT PIN
      IF( (LOPERR).AND.IMATCH.NE.0 ) WRITE(PMS,105) IMATCH,IPAL,INAME
  105 FORMAT(' THIS PIN, NUMBER ',I2,' IS AN INVALID OUTPUT PIN',
     1       ' FOR ',3A1,5A1)
C     PRINT AN ERROR MESSAGE FOR AN INVALID INPUT PIN
      IF(LINP) WRITE(PMS,115) IMATCH,IPAL,INAME
  115 FORMAT(' THIS PIN NUMBER ',I2,' IS AN INVALID INPUT PIN',
     1       ' FOR ',3A1,5A1)
      CALL EXIT
  102 CALL TWEEK(ITYPE,LFUSES,LPHANT)
  108 WRITE(6,106)
  106 FORMAT(/,' OPERATION CODES:')
      WRITE(6,107)
  107 FORMAT(/,' E=ECHO INPUT  O=PINOUT  T=SIMULATE  F=FAULT TEST',
     2       /,' P=PLOT  B=BRIEF  J=JEDEC FORMAT  H=HEX  S=SHORT',
     3       /,' L=BHLF  N=BNPF   I=INTEL FORMAT C=CATALOG  Q=QUIT')
      WRITE(6,110)
  110 FORMAT(/,' ENTER OPERATION CODE: ',$)
      READ(ROC,120) IOP
  120 FORMAT(A1)
C     CALL IODC2
      IF(POF.NE.6) WRITE(POF,125)
  125 FORMAT('1')
      IF(IOP.EQ.E) CALL ECHO(IPAL,INAME,REST,PATNUM,TITLE,COMP)
      IF(IOP.EQ.O) CALL PINOUT(IPAL,INAME,TITLE)
      IF(IOP.EQ.T) CALL TEST(LPHASE,LBUF,TITLE,IC,IL,ILE,ISYM,IBUF,
     1                  ITYPE,IPCTR,LERR,ISAF,IPCTR1,.FALSE.,.FALSE.,
     2                  IOP.NE.JJ)
      IF (IOP.NE.JJ) GOTO 130
      CALL TEST(LPHASE,LBUF,TITLE,IC,IL,ILE,ISYM,IBUF,
     1           ITYPE,IPCTR,LERR,ISAF,IPCTR1,.FALSE.,.FALSE.,.FALSE.,
     2           IOP.NE.JJ)
      CALL PLOTF(ITYPE,IOT)
  130 ISAF=0
      IF(IOP.EQ.F) GO TO 200
  135 IF(IOP.EQ.P) CALL PLOT(LBUF,IBUF,LFUSES,IPROD,TITLE,.TRUE.,ITYPE,
     1                       LPROD,IOP,IBLOW,IPCTR0)
      IF(IOP.EQ.B) CALL PLOT(LBUF,IBUF,LFUSES,IPROD,TITLE,.TRUE.,ITYPE,
     1                       LPROD,IOP,IBLOW,IPCTR0)
      IF(IOP.EQ.H) CALL HEX(LFUSES,H)
      IF(IOP.EQ.S) CALL HEX(LFUSES,S)
      IF(IOP.EQ.L) CALL BINR(LFUSES,H,L)
      IF(IOP.EQ.N) CALL BINR(LFUSES,P,N)
      IF(IOP.EQ.C) CALL CAT
C
C     CALL IODC4
C    (INTEL PROGRAMMING FORMAT)
      IF(IOP.EQ.II) CALL INTEL(LFUSES,II)
      IF(IOP.NE.Q ) GO TO 108
      CALL EXIT
C  SETTING THE PARAMETERS FOR THE SA0/SA1 TESTS
  200 IPCTR=0
      CALL TEST(LPHASE,LBUF,TITLE,IC,IL,ILE,ISYM,IBUF,ITYPE,IPCTR,
     1          LERR,ISAF,IPCTR1,.FALSE.,.FALSE.,IOP.NE.JJ)
      IPCTR0=IPCTR
C  LOOPING FOR SA1 TEST
      DO 210 IPCTR1=1,IPCTR0
      LSA11=.TRUE.
      CALL TEST(LPHASE,LBUF,TITLE,IC,IL,ILE,ISYM,IBUF,ITYPE,IPCTR,
     1          LERR,ISAF,IPCTR1,LSA11,.FALSE.,IOP.NE.JJ)
  210 CONTINUE
      ISA1=ISAF
C   LOOPING FOR SA0 TEST
      DO 215 IPCTR1=1,IPCTR0
      LSA01=.TRUE.
      CALL TEST(LPHASE,LBUF,TITLE,IC,IL,ILE,ISYM,IBUF,ITYPE,IPCTR,
     1          LERR,ISAF,IPCTR1,.FALSE.,LSA01,IOP.NE.JJ)
  215 CONTINUE
      ISA0=ISAF-ISA1
      IFAULT=(ISAF*100)/(2*IPCTR0)
      WRITE(POF,220) ISA1
  220 FORMAT(/,' NUMBER OF STUCK AT ONE  (SA1) FAULTS ARE =',I3)
      WRITE(POF,225) ISA0
  225 FORMAT(/,' NUMBER OF STUCK AT ZERO (SA0) FAULTS ARE =',I3)
      WRITE(POF,230)IFAULT
  230 FORMAT(/,' PRODUCT   TERM   COVERAGE                ='I4,'%',/)
      GO TO 135        
      END
C
C***********************************************************************
C
      SUBROUTINE INITLZ(INAME,ITYPE,LFUSES,LPHANT,IC,IL,IBLOW,IPCTR)
C     THIS SUBROUTINE INITIALIZIES VARIABLES AND MATCHES PAL PART
C     NUMBER WITH ITYPE
      IMPLICIT INTEGER (A-Z)
      INTEGER INAME(5),INFO(6,14),TSTVEC
      LOGICAL LBLANK,LLEFT,LAND,LOR,LSLASH,LEQUAL,LRIGHT,LFUSES(40,80),
     1        LMATCH,LXOR,LPHANT(40,80)
      COMMON  LBLANK,LLEFT,LAND,LOR,LSLASH,LEQUAL,LRIGHT,LXOR
      COMMON /PGE/ IPAGE(80,200)
      COMMON /TSTVEC/ NTEST,TSTVEC
      DATA INFO
     1    /'1','2','L','1','0',1,
     2     '1','4','L','8',' ',2,
     3     '1','6','L','6',' ',3,
     4     '1','8','L','4',' ',4,
     5     '2','0','L','2',' ',5,
     6     '2','0','C','1',' ',6,
     7     '2','0','L','1','0',7,
C
     8     '2','0','X','1','0',8,
     9     '2','0','X','8',' ',9,
     A     '2','0','X','4',' ',10,
     B     '2','0','L','8',' ',11,
     C     '2','0','R','8',' ',12,
     D     '2','0','R','6',' ',13,
     E     '2','0','R','4',' ',14/
C     INITIALIZE LFUSES ARRAY (FUSE ARRAY)
      DO 20 J=1,80
         DO 20 I=1,40
            LPHANT(I,J)=.FALSE.
   20       LFUSES(I,J)=.FALSE.
            NTEST = 0
C     INITIALIZE IBLOW (NUMBER OF FUSES BLOWN)
      IBLOW=0
      IPCTR=0
C     INITIALIZE IC AND IL (COLUMN AND LINE POINTERS)
      IC=0
      IL=1
C     INITIALIZE ITYPE (PAL PART TYPE)
      ITYPE=0
C     ITYPE IS ASSIGNED THE FOLLOWING VALUES FOR THESE PAL PART TYPES:
C     PAL12L10 =  1   PAL14L8  =  2   PAL16L6  =  3   PAL18L4  =  4
C     PAL20L2  =  5   PAL20C1  =  6   PAL20L10 =  7   PAL20X10 =  8
C     PAL20X8  =  9   PAL20X4  = 10   PAL20L8  = 11   PAL20R8  = 12
C     PAL20R6  = 13   PAL20R4  = 14
      DO 40 J=1,14
         LMATCH=.TRUE.
         DO 30 I=1,4
   30       IF(INAME(I).NE.INFO(I,J)) LMATCH=.FALSE.
         IF(LMATCH) ITYPE=INFO(6,J)
         IF (LMATCH) GO TO 50
   40 CONTINUE
      IF(ITYPE.EQ.0) RETURN
   50 CALL INCR(IC,IL)
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE GETSYM(LPHASE,ISYM,J,IC,IL)
C     THIS SUBROUTINE GETS THE PIN NAME, / IF COMPLEMENT LOGIC, AND
C      THE FOLLOWING OPERATION SYMBOL IF ANY
      IMPLICIT INTEGER (A-Z)
      INTEGER ISYM(8,24)
      LOGICAL LBLANK,LLEFT,LAND,LOR,LSLASH,LEQUAL,LRIGHT,LXOR,LPHASE(24)
      COMMON  LBLANK,LLEFT,LAND,LOR,LSLASH,LEQUAL,LRIGHT,LXOR
      COMMON /PGE/ IPAGE(80,200)
      DATA IBLANK/' '/
      IF( .NOT.(LLEFT.OR.LAND.OR.LOR.OR.LEQUAL.OR.LRIGHT) )  GO TO 10
      CALL INCR(IC,IL)
   10 LPHASE(J)=(.NOT.LSLASH)
      IF(LPHASE(J)) GO TO 15
      CALL INCR(IC,IL)
   15 DO 20 I=1,8
   20     ISYM(I,J)=IBLANK
   25 DO 30 I=1,7
C
   30     ISYM(I,J)=ISYM(I+1,J)
      ISYM(8,J)=IPAGE(IC,IL)
      CALL INCR(IC,IL)
      IF( LLEFT.OR.LBLANK.OR.LAND.OR.LOR.OR.LRIGHT.OR.LEQUAL ) RETURN
      GO TO 25
      END
C
C**********************************************************************
C
      SUBROUTINE INCR(IC,IL)
C     THIS SUBROUTINE INCREMENTS COLUMN AND LINE POINTERS
C      BLANKS AND CHARACTERS AFTER ';' ARE IGNORED
      IMPLICIT INTEGER (A-Z)
      LOGICAL LBLANK,LLEFT,LAND,LOR,LSLASH,LEQUAL,LRIGHT,LXOR,LXOR1
      COMMON  LBLANK,LLEFT,LAND,LOR,LSLASH,LEQUAL,LRIGHT,LXOR
      COMMON /PGE/ IPAGE(80,200)
      COMMON /LUNIT/ PMS,POF,PDF
      DATA IBLANK/' '/,ILEFT/'('/,IAND/'*'/,IOR/'+'/,COMENT/';'/,
     1        ISLASH/'/'/,IEQUAL/'='/,IRIGHT/')'/,ICOLON/':'/,TAB/009/
      LBLANK=.FALSE.
      LXOR=.FALSE.
      LXOR1=.FALSE.
   10 IC=IC+1
      IF( IC.LE.79.AND.IPAGE(IC,IL).NE.COMENT ) GO TO 30
      IL=IL+1
      IF(IL.LE.200) GO TO 20
          WRITE(PMS,15)
   15     FORMAT(/,' SOURCE FILE EXCEEDS 200 LINES OR MISSING',
     1             ' DESCRIPTION OR FUNCTION TABLE KEY WORD')
          CALL EXIT
   20 IC=0
      GO TO 10
   30 IF ((IPAGE(IC,IL).NE.IBLANK).AND.(IPAGE(IC,IL).NE.TAB)) GO TO 31
      LBLANK=.TRUE.
      GO TO 10
   31 IF(IPAGE(IC,IL).NE.ICOLON) GO TO 32
      IF(LXOR) GO TO 33
      LXOR1=.TRUE.
      GO TO 10
   33 LOR=.TRUE.
      RETURN
   32 IF( .NOT.(IPAGE(IC,IL).EQ.IOR.AND.(LXOR1)) ) GO TO 34
      LXOR=.TRUE.
      GO TO 10
   34 LLEFT =(IPAGE(IC,IL).EQ.ILEFT)
      LAND  =(IPAGE(IC,IL).EQ.IAND)
      LOR   =(IPAGE(IC,IL).EQ.IOR)
      LSLASH=(IPAGE(IC,IL).EQ.ISLASH)
      LEQUAL=(IPAGE(IC,IL).EQ.IEQUAL)
      LRIGHT=(IPAGE(IC,IL).EQ.IRIGHT)
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE MATCH(IMATCH,IBUF,ISYM)
C     THIS SUBROUTINE FINDS A MATCH BETWEEN THE PIN NAME IN THE EQUATION
C
C      AND THE PIN NAME IN THE PIN LIST OR FUNCTION TABLE PIN LIST
      IMPLICIT INTEGER (A-Z)
      INTEGER IBUF(8,24),ISYM(8,24)
      LOGICAL LMATCH
      IMATCH=0
      DO 20 J=1,24
          LMATCH=.TRUE.
          DO 10 I=1,8
   10         LMATCH=LMATCH.AND.(IBUF(I,1).EQ.ISYM(I,J))
          IF(LMATCH) IMATCH=J
   20     CONTINUE
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE IXLATE(IINPUT,LPHASE,IMATCH,LBUF,ITYPE)
C     THIS SUBROUTINE FINDS A MATCH BETWEEN INPUT PIN NUMBER AND
C      THE INPUT LINE NUMBER FOR A SPECIFIC PAL.  ADD 1 TO THE INPUT
C      LINE NUMBER IF THE PIN IS A COMPLEMENT
      IMPLICIT INTEGER (A-Z)
      INTEGER ITABLE(24,14)
      LOGICAL LPHASE(24),LBUF(24)
      DATA    ITABLE/
     1 3,1,5,9,13,17,21,25,29,33,37,0,39, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,
     2 3,1,5,9,13,17,21,25,29,33,37,0,39,35, 0, 0, 0, 0, 0, 0, 0, 0,7,0,
     3 3,1,5,9,13,17,21,25,29,33,37,0,39,35,31, 0, 0, 0, 0, 0, 0,11,7,0,
     4 3,1,5,9,13,17,21,25,29,33,37,0,39,35,31,27, 0, 0, 0, 0,15,11,7,0,
     5 3,1,5,9,13,17,21,25,29,33,37,0,39,35,31,27,23, 0, 0,19,15,11,7,0,
     6 3,1,5,9,13,17,21,25,29,33,37,0,39,35,31,27,23, 0, 0,19,15,11,7,0,
     7 3,1,5,9,13,17,21,25,29,33,37,0,39, 0,35,31,27,23,19,15,11, 7,0,0,
     8 0,1,5,9,13,17,21,25,29,33,37,0, 0,39,35,31,27,23,19,15,11, 7,3,0,
     9 0,1,5,9,13,17,21,25,29,33,37,0, 0,39,35,31,27,23,19,15,11, 7,3,0,
     A 0,1,5,9,13,17,21,25,29,33,37,0, 0,39,35,31,27,23,19,15,11, 7,3,0,
     B 3,1,5,9,13,17,21,25,29,33,37,0,39,35, 0,31,27,23,19,15,11, 0,7,0,
     C 0,1,5,9,13,17,21,25,29,33,37,0, 0,39,35,31,27,23,19,15,11, 7,3,0,
     D 0,1,5,9,13,17,21,25,29,33,37,0, 0,39,35,31,27,23,19,15,11, 7,3,0,
     E 0,1,5,9,13,17,21,25,29,33,37,0, 0,39,35,31,27,23,19,15,11, 7,3,0/
      IBUBL=0
      IF( ((     LPHASE(IMATCH)).AND.(.NOT.LBUF(1))).OR.
     1    ((.NOT.LPHASE(IMATCH)).AND.(     LBUF(1))) ) IBUBL=1
      IINPUT=ITABLE(IMATCH,ITYPE)+IBUBL
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE ECHO(IPAL,INAME,REST,PATNUM,TITLE,COMP)
C     THIS SUBROUTINE PRINTS THE PAL DESIGN SPECIFICATION INPUT FILE
      IMPLICIT INTEGER (A-Z)
      INTEGER IPAL(3),INAME(5),REST(72),PATNUM(80),TITLE(80),COMP(80)
      COMMON /PGE/ IPAGE(80,200)
      COMMON /LUNIT/ PMS,POF,PDF
      COMMON /FTEST/ IFUNCT,IDESC,IEND
      DATA IBLANK/' '/
      WRITE(POF,5) IPAL,INAME,REST,PATNUM,TITLE,COMP
    5 FORMAT(/,' ',3A1,5A1,72A1,/,' ',80A1,/,' ',80A1,/,' ',80A1)
C
      DO 20 IL=1,IEND
          IC=81
   10     IC=IC-1
          IF( IPAGE(IC,IL).EQ.IBLANK.AND.IC.GT.1 ) GO TO 10
          WRITE(POF,15) (IPAGE(I,IL),I=1,IC)
   15     FORMAT(' ',80A1)
   20 CONTINUE
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE CAT
C     THIS SUBROUTINE PRINTS THE PALASM CATALOG
      IMPLICIT INTEGER (A-Z)
      COMMON /LUNIT/ PMS,POF,PDF
      WRITE(PMS,10)
   10 FORMAT(/,' MONOLITHIC MEMORIES 24-PIN PALASM VERSION 1.6C')
      WRITE(PMS,15)
   15 FORMAT(' (C) COPYRIGHT 1983 MONOLITHIC MEMORIES')
      WRITE(PMS,20)
   20 FORMAT(/,' THIS PALASM AIDS THE USER IN THE DESIGN AND',
     1         ' PROGRAMMING OF THE',/,' SERIES 24 PAL FAMILY.  THE',
     2         ' FOLLOWING OPTIONS ARE PROVIDED:',
     3       /,'    ECHO (E)     - PRINTS THE PAL DESIGN',
     4         ' SPECIFICATION',
     5      //,'    PINOUT (O)   - PRINTS THE PINOUT OF THE PAL',
     6      //,'    SIMULATE (T) - EXERCISES THE FUNCTION TABLE',
     7         ' VECTORS IN THE LOGIC',/,'                  ',
     8         ' EQUATIONS AND GENERATES TEST VECTORS',
     9      //,'    FAULT (F)    - PERFORMS FAULT TESTING',
     A      //,'    PLOT (P)     - PRINTS THE ENTIRE FUSE PLOT')
      WRITE(PMS,30)
   30 FORMAT(/,'    BRIEF (B)    - PRINTS ONLY THE USED PRODUCT LINES',
     1         ' OF THE FUSE PLOT',/,'                   PHANTOM',
     2         ' FUSES ARE OMITTED',
     3      //,'    JEDEC (J)    - GENERATES JEDEC PROGRAMMING FORMAT',
     4      //,'    HEX (H)      - GENERATES HEX PROGRAMMING FORMAT',
     5      //,'    SHORT (S)    - GENERATES HEX PROGRAMMING FORMAT',
     6      //,'    BHLF (L)     - GENERATES BHLF PROGRAMMING FORMAT',
     7      //,'    BNPF (N)     - GENERATES BNPF PROGRAMMING FORMAT',
     8      //,'    INTEL (I)    - GENERATES INTEL PROGRAMMING FORMAT',
     9      //,'    CATALOG (C)  - PRINTS THE PALASM CATALOG',
     A      //,'    QUIT (Q)     - EXIT PALASM')
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE PINOUT(IPAL,INAME,TITLE)
C     THIS SUBROUTINE PRINTS THE PINOUT OF THE PAL
      IMPLICIT INTEGER (A-Z)
      INTEGER IPAL(3),INAME(5),TITLE(80),PIN(12,24),IIN(8,2)
      COMMON /PGE/ IPAGE(80,200)
      COMMON /LUNIT/ PMS,POF,PDF
      DATA IBLANK/' '/,ISTAR/'*'/
      DO 10 J=1,24
          DO 5 I=1,12
    5         PIN(I,J)=IBLANK
C
   10 CONTINUE
   15 DO 25 J=1,2
          DO 20 I=1,8
   20         IIN(I,J)=IBLANK
   25 CONTINUE
      IIN(2,1)=IPAL(1)
      IIN(4,1)=IPAL(2)
      IIN(6,1)=IPAL(3)
      IIN(1,2)=INAME(1)
      IIN(3,2)=INAME(2)
      IIN(5,2)=INAME(3)
      IIN(7,2)=INAME(4)
      IIN(8,2)=INAME(5)
      J=0
      IL=0
   30 IC=0
      IL=IL+1
   35 IC=IC+1
   40 IF( IC.GT.80 ) GO TO 30
      IF( IPAGE(IC,IL).EQ.IBLANK ) GO TO 35
      J=J+1
      IF(J.GT.24) GO TO 60
      DO 55 I=1,12
          PIN(I,J)=IPAGE(IC,IL)
          IC=IC+1
          IF( IC.GT.80 ) GO TO 40
          IF( IPAGE(IC,IL).EQ.IBLANK ) GO TO 40
   55     CONTINUE
   60 DO 75 J=1,12
          II=0
   65     II=II+1
          IF(II.EQ.13) GO TO 75
          IF( PIN(II,J).NE.IBLANK ) GO TO 65
          I=13
   70     I=I-1
          II=II-1
          PIN(I,J)=PIN(II,J)
          PIN(II,J)=IBLANK
          IF(II.NE.1) GO TO 70
   75 CONTINUE
      WRITE(POF,76) TITLE
   76 FORMAT(/,' ',80A1)
      WRITE(POF,78) ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,
     1              ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,
     2              ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,
     3              ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR
   78 FORMAT(/,' ',18X,14A1,3X,14A1,
     1       /,' ',18X,A1,13X,A1,1X,A1,13X,A1)
      JJ=24
      DO 88 J=1,12
          WRITE(POF,80) ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR
   80     FORMAT(' ',15X,4A1,29X,4A1)
          WRITE(POF,81) (PIN(I,J),I=1,12),ISTAR,J,ISTAR,
     1         (IIN(I,1),I=1,8),ISTAR,JJ,ISTAR,(PIN(I,JJ),I=1,12)
   81     FORMAT(' ',12A1,3X,A1,I2,A1,11X,8A1,10X,A1,I2,A1,3X,12A1)
          WRITE(POF,82) ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR
   82     FORMAT(' ',15X,4A1,29X,4A1)
C
          WRITE(POF,84) ISTAR,(IIN(I,2),I=1,8),ISTAR
   84     FORMAT(' ',18X,A1,11X,8A1,10X,A1)
          DO 86 II=1,2
              DO 85 I=1,8
   85             IIN(I,II)=IBLANK
   86     CONTINUE
          JJ=JJ-1
   88 CONTINUE
      WRITE(POF,90) ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,
     1              ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,
     2              ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,
     3              ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR
   90 FORMAT(' ',18X,31A1)
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE PLOT(LBUF,IBUF,LFUSES,IPROD,TITLE,LDUMP,ITYPE,LPROD,
     1                IOP,IBLOW,IPCTR0)
C     THIS SUBROUTINE PRODUCES THE FUSE PLOT
      IMPLICIT INTEGER (A-Z)
      INTEGER IBUF(8,24),IOUT(64),ISAVE(80,40),TITLE(80),IDATA(40)
      LOGICAL LBUF(24),LFUSES(40,80),LDUMP,LPROD(80)
      INTEGER STX,ETX
      COMMON /LUNIT/ PMS,POF,PDF
      DATA ISAVE/3200*' '/,IAND/'*'/,IOR/'+'/,ISLASH/'/'/,
     1     IDASH/'-'/,X/'X'/,IBLANK/' '/,P/'P'/,B/'B'/,
     2     D/'D'/,ZERO/'0'/,ONE/'1'/,FX/'0'/,FIDASH/'O'/
      DATA STX/002/,ETX/003/
      IF(LDUMP) GO TO 58
      IF(ISAVE(IPROD,1).NE.IBLANK) RETURN
      IF(LBUF(1)) GO TO 5
      DO 30 J=1,39
   30     ISAVE(IPROD,J)=ISAVE(IPROD,J+1)
      ISAVE(IPROD,40)=ISLASH
    5 DO 20 I=1,8
         IF( ISAVE(IPROD,1).NE.IBLANK ) RETURN
          IF( IBUF(I,1).EQ.IBLANK ) GO TO 20
          DO 10 J=1,39
   10         ISAVE(IPROD,J)=ISAVE(IPROD,J+1)
          ISAVE(IPROD,40)=IBUF(I,1)
   20     CONTINUE
      IF(ISAVE(IPROD,1).NE.IBLANK) RETURN
   40 DO 50 J=1,39
   50     ISAVE(IPROD,J)=ISAVE(IPROD,J+1)
      ISAVE(IPROD,40)=IAND
      RETURN
C     PRINT FUSE PLOT
   58 IF(IOP.EQ.D) GO TO 62
      WRITE(POF,61) TITLE
   61 FORMAT(/,' ',80A1,//,
     1 '                11 1111 1111 2222 2222 2233 3333 3333',/,
     2 '    0123 4567 8901 2345 6789 0123 4567 8901 2345 6789',/)
      GO TO 64
C***** STX DETERMINES THE STARTING CHARACTER FOR DATA I/O FORMAT
   62 WRITE(PDF,63) STX
C
   63 FORMAT(' ',A1,'*L0000'/)
   64 DO 100 I88PRO=1,73,8
          DO 94 I8PRO=1,8
              IPROD=I88PRO+I8PRO-1
              ISAVE(IPROD,40)=IBLANK
              DO 70 I=1,40
                  IF( ISAVE(IPROD,1).NE.IBLANK ) GO TO 70
                  DO 65 J=1,39
   65                 ISAVE(IPROD,J)=ISAVE(IPROD,J+1)
                  ISAVE(IPROD,40)=IBLANK
   70             CONTINUE
              DO 75 I=1,24
                  IOUT(I+40)=ISAVE(IPROD,I)
   75         CONTINUE
              IF( ISAVE(IPROD,25).NE.IBLANK ) IOUT(64)=IDASH
              DO 80 I=1,40
                  IOUT(I)=X
                  IF(LFUSES(I,IPROD)) IOUT(I)=IDASH
   80         CONTINUE
              CALL FANTOM(ITYPE,IOP,IOUT,IPROD,I8PRO)
              IF(IOP.NE.D) GO TO 85
              K=0
   81         DO 82 I=1,40
                
                 IF((IOUT(I).EQ.FX).OR.(IOUT(I).EQ.FIDASH)) GO TO 82
                 K=K+1
                 IF(IOUT(I).EQ.X)     IDATA(K)=ZERO
                 IF(IOUT(I).EQ.IDASH) IDATA(K)=ONE
   82         CONTINUE
              DO 83 I=1,40
                 IF( (IOUT(I).EQ.X).OR.(IOUT(I).EQ.IDASH) ) GO TO 86
   83         CONTINUE
              GO TO 94
   86         WRITE(PDF,84) IDATA
   84         FORMAT(' ',40(A1,' '))
              GO TO 94
   85         IPROD=IPROD-1
              IF( (IOP.EQ.P).OR.(IOP.EQ.B.AND.(LPROD(IPROD+1))) )
     1        WRITE(POF,90) IPROD,IOUT
   90         FORMAT(' ',I2,10(' ',4A1),' ',24A1)
   94         CONTINUE
          WRITE(POF,96)
   96     FORMAT(1X)
  100     CONTINUE
      IF(IOP.NE.D) GO TO 105
      WRITE(PDF,101) ETX
  101 FORMAT(' ',A1)
      RETURN
  105 WRITE(POF,110)
  110 FORMAT(/,
     1' LEGEND:  X : FUSE NOT BLOWN (L,N,0)   - : FUSE BLOWN   (H,P,1)')
      IF(IOP.EQ.P) WRITE(POF,111)
  111 FORMAT(
     1'          0 : PHANTOM FUSE   (L,N,0)   O : PHANTOM FUSE (H,P,1)')
      WRITE(POF,112) IBLOW
  112 FORMAT(/,' NUMBER OF FUSES BLOW = ',I4)
      WRITE(POF,113)
  113 FORMAT(//)
C
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE PLOTF(ITYPE,IOT)
C     THIS SUBROUTINE GENERATES THE JEDEC PROGRAMMING FORMAT WHICH IS
C      COMPATIBLE WITH THE DATA I/O PROGRAMMABLE LOGIC PAK (PLDS)
      IMPLICIT INTEGER (A-Z)
      LOGICAL LFUSES(40,80),LPHANT(40,80)
      INTEGER IPBUF(40),ZERO,ONE,TSTVEC(24,42)
      INTEGER ISUM(4),IADR,STX,ETX,IDEC(4),IPT,IINP,J1,J2,PINOUT(2)
      INTEGER IOUT(64)
      INTEGER IDECIO(4),ISUMV(4),ISUMIO(4),BUFIO(40),NFUSE,NTEST,IOT
      COMMON /LUNIT/PMS,POF,PDF
      COMMON /IPT/IPT
      COMMON /LFUSES/LFUSES,LPHANT
      COMMON /SUM/ISUM,IDEC,IPBUF,BUFIO,NFUSE
      COMMON /BLK/ PR8X10(10,14),PROD8(8,11),PRODLN(40,7)
      COMMON /TSTVEC/ NTEST,TSTVEC
      DATA ZERO/'0'/,ONE/'1'/,FX/'0'/,FIDASH/'O'/,L/'L'/,H/'H'/,X/'X'/,
     1     C/'C'/,Z/'Z'/,N0/'0'/,N1/'1'/,STAR/'*'/,NN/'N'/
      IADR=0
      STX=2
      ETX=3  
C	DATA I/O PROGRAMMING ASSIGNMENT
      IF (ITYPE.NE.11) GOTO 444
          PINOUT(1) = 2
          PINOUT(2) = 6
          GOTO 999
 444  IF ((ITYPE.NE.12).AND.(ITYPE.NE.14).AND.(ITYPE.NE.13))
     1  GOTO 555
          PINOUT(1) = 2
          PINOUT(2) = 7
          GOTO 999    
 555  IF (ITYPE.LE.5) GOTO 666
          PINOUT(1) = 0
 	  PINOUT(2) = 6
          GOTO 999    
 666  	  PINOUT(2) = ITYPE
	  IF(IOT.EQ.'H') ITYPE = ITYPE + 6
          PINOUT(1) = MOD(PINOUT(2)/10,10)
          PINOUT(2) = MOD(PINOUT(2),10)
 999	  ISUM(4)=536+PINOUT(1)+PINOUT(2)
	  ISUM(2)=MOD(ISUM(4)/256,256)
	  ISUM(4)=MOD(ISUM(4),256)
	  WRITE(PDF,10) STX,PINOUT(1),PINOUT(2)
   10     FORMAT(' ',A1,'*D22',2I1,'*F0*')
         DO 300 IPT=1,80
         NFUSE = 0
         DO 50 IINP=1,40
	 IF (LPHANT(IINP,IPT))  GOTO 50
            NFUSE = NFUSE + 1
            IF(LFUSES(IINP,IPT)) IPBUF(NFUSE)=ONE
            IF(.NOT.(LFUSES(IINP,IPT))) IPBUF(NFUSE)=ZERO
   50    CONTINUE
         IF(LFUSES(1,IPT)) GO TO 100
         IF(.NOT.LFUSES(2,IPT)) GO TO 250
  100    IDEC(4)=IADR
C
          DO 150 J=1,3
            J1=5-J
            J2=4-J
            IDEC(J2)=IDEC(J1)/10
            IDEC(J1)=IDEC(J1)-10*IDEC(J2)
            IDECIO(J1)=ICONV(IDEC(J1))
  150    CONTINUE
         IDECIO(1)=ICONV(IDEC(1))
         CALL SUMCHK
         WRITE(PDF,201) IDECIO,(IPBUF(I),I=1,NFUSE),STAR
  201    FORMAT(' L',4A1,' ',40A1,A1)
  250    IADR=IADR+NFUSE
  300  CONTINUE
       IF (NTEST.EQ.0) GOTO 371
          IF(NTEST.GT.42) NTEST = 42
          DO 370 J = 1,NTEST
          ISUM(4)  = ISUM(4) + 215
          ISUM(2)  = MOD(ISUM(2)+ISUM(4)/256,256)
          ISUM(4)  = MOD(ISUM(4),256)
          DO 350 I = 1,24
          J1 =  (TSTVEC(I,J))
          IF (J1.EQ.C) ISUM(4) = ISUM(4) + 67
          IF (J1.EQ.X) ISUM(4) = ISUM(4) + 88
          IF (J1.EQ.NN) ISUM(4) = ISUM(4) + 78
          IF (J1.EQ.L) ISUM(4) = ISUM(4) + 76
          IF (J1.EQ.H) ISUM(4) = ISUM(4) + 72
          IF (J1.EQ.Z) ISUM(4) = ISUM(4) + 90
          IF (J1.EQ.N1) ISUM(4) = ISUM(4) + 49
          IF (J1.EQ.N0) ISUM(4) = ISUM(4) + 48
          ISUM(2) = MOD(ISUM(2)+ISUM(4)/256,256)
          ISUM(4) = MOD(ISUM(4),256)
350       CONTINUE
          DO 360 I = 1,4
          IDECIO(I) = MOD(J/10**(4-I),10)
          ISUM(4)   = ISUM(4) + IDECIO(I) + 48
          ISUM(2)   = MOD(ISUM(2)+ISUM(4)/256,256)
          ISUM(4)   = MOD(ISUM(4),256)
360	  CONTINUE
          WRITE(PDF,410) IDECIO,(TSTVEC(I,J),I=1,24)
410       FORMAT(' V',4I1,1X,24A1,' *')
370       CONTINUE
371      ISUMIO(1)=ICONV(ISUM(2)/16)
      ISUM(2)=MOD(ISUM(2),16)
      ISUMIO(2)=ICONV(ISUM(2))
      ISUMIO(3)=ICONV(ISUM(4)/16)
      ISUM(4)=MOD(ISUM(4),16)
      ISUMIO(4)=ICONV(ISUM(4))
      WRITE(PDF,400) ETX,ISUMIO
  400 FORMAT(' ',A1,4A1,/)
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE SUMCHK
C     THIS SUBROUTINE GENERATES THE CHECK SUM FOR THE JEDEC PROGRAMMING
C     FORMAT
      IMPLICIT INTEGER(A-Z)
      LOGICAL LFUSES(40,80),LPHANT(40,80)
      INTEGER IPBUF(40), BUFIO(40)
      INTEGER ISUM(4), IDEC(4)
      COMMON /IPT/IPT
      COMMON /LFUSES/ LFUSES,LPHANT
      COMMON /SUM/ ISUM, IDEC,IPBUF,BUFIO,NFUSE
      DO 5 J=1,40
         IF (LPHANT(J,IPT)) GO TO 5
         IF(LFUSES(J,IPT)) BUFIO(J)=49
         IF(.NOT.LFUSES(J,IPT)) BUFIO(J)=48
         ISUM(4)=ISUM(4)+BUFIO(J)
         ISUM(2)=MOD(ISUM(2)+ISUM(4)/256,256)
         ISUM(4)=MOD(ISUM(4),256)
   5  CONTINUE
C
         DO 10 J=1,4
         ISUM(4)=ISUM(4)+IDEC(J)+48
         ISUM(2)=MOD(ISUM(2)+ISUM(4)/256,256)
         ISUM(4)=MOD(ISUM(4),256)
  10  CONTINUE
      ISUM(4)=ISUM(4)+173 
      ISUM(2)=MOD(ISUM(2)+ISUM(4)/256,256)
      ISUM(4)=MOD(ISUM(4),256)
      RETURN
      END
C
C***********************************************************************
C      
  
      INTEGER FUNCTION ICONV(K)
C     THIS INTEGER FUNCTION CONVERTS DECIMAL INTO ASCII CODE FOR THE
C      JEDEC PROGRAMMING FORMAT
      IMPLICIT INTEGER (A-Z)
      DATA A/'0'/,B/'1'/,C/'2'/,D/'3'/,E/'4'/,F/'5'/,G/'6'/,H/'7'/,
     1     I/'8'/,J/'9'/,X/'A'/,L/'B'/,M/'C'/,N/'D'/,O/'E'/,P/'F'/
      IF(K.EQ.0) ICONV=A
      IF(K.EQ.1) ICONV=B
      IF(K.EQ.2) ICONV=C
      IF(K.EQ.3) ICONV=D
      IF(K.EQ.4) ICONV=E
      IF(K.EQ.5) ICONV=F
      IF(K.EQ.6) ICONV=G
      IF(K.EQ.7) ICONV=H
      IF(K.EQ.8) ICONV=I
      IF(K.EQ.9) ICONV=J
      IF(K.EQ.10)ICONV=X
      IF(K.EQ.11)ICONV=L
      IF(K.EQ.12)ICONV=M
      IF(K.EQ.13)ICONV=N
      IF(K.EQ.14)ICONV=O
      IF(K.EQ.15)ICONV=P
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE HEX(LFUSES,IOP)
C     THIS SUBROUTINE GENERATES HEX PROGRAMMING FORMATS
      IMPLICIT INTEGER (A-Z)
      INTEGER ITEMP(80),ZTABL1(32),ZTABL2(16),ZCSUM(4)
      LOGICAL LFUSES(40,80)
      INTEGER SOH,STX,ETX,BEL
      COMMON /LUNIT/PMS,POF,PDF
      DATA H/'H'/,S/'S'/,IBLANK/' '/,
     1     ZTABL1/'00','01','02','03','04','05','06','07',
     2            '08','09','0A','0B','0C','0D','0E','0F',
     3            '10','11','12','13','14','15','16','17',
     4            '18','19','1A','1B','1C','1D','1E','1F'/,
     5     ZTABL2/'0','1','2','3','4','5','6','7',
     6            '8','9','A','B','C','D','E','F'/
      DATA SOH/001/,STX/002/,ETX/003/,BEL/007/
      CSUM=0
C
      IF(IOP.EQ.H) WRITE(PDF,10)
   10 FORMAT(//,80(' '),//)
C***** NOTE: SOME PROM PROGRAMMERS NEED A START CHARACTER.
C*****       THIS PROGRAM OUTPUTS AN STX FOR THE DATA I/O MODEL 9
C*****         (USE SOH FOR MODEL 5)
      WRITE(PDF,5) BEL,BEL,BEL,BEL,BEL,BEL,BEL,STX,SOH
    5 FORMAT(' ',9A1)
      DO 40 I=1,41,40
      INC=I-1
        DO 40 IPROD=1,7,2
          DO 20 J=1,2
              DO 20 IINPUT=1,40
              IHEX=0
              ISUM2=IPROD + J-1 + INC
              IF(LFUSES(IINPUT,ISUM2 +  0 )) IHEX=IHEX+1
              IF(LFUSES(IINPUT,ISUM2 +  8 )) IHEX=IHEX+2
              IF(LFUSES(IINPUT,ISUM2 + 16 )) IHEX=IHEX+4
              IF(LFUSES(IINPUT,ISUM2 + 24 )) IHEX=IHEX+8
              IF(LFUSES(IINPUT,ISUM2 + 32 )) IHEX=IHEX+16
              CSUM=CSUM+IHEX
              ISUMX=IINPUT+(40*(J-1))
   20         ITEMP(ISUMX)=ZTABL1(IHEX+1)
          IF(IOP.EQ.H) WRITE(PDF,60) ITEMP
   60     FORMAT(4(' ',20(A2,' '),'.',/))
   40     IF(IOP.EQ.S) WRITE(PDF,61) ITEMP
   61     FORMAT(4(' ',20A2,'.',/))
      IF(IOP.EQ.H) WRITE(PDF,70)
   70 FORMAT(//,80(' '),//)
      WRITE(PDF,80) ETX
   80 FORMAT(' ',A1)
C     CONVERT DECIMAL CHECK SUM INTO HEX CHECK SUM
      DO 85 I=1,4
           ZTEMP=CSUM-16*(CSUM/16)
           ZCSUM(5-I)=ZTABL2(ZTEMP+1)
           CSUM=CSUM/16
   85 CONTINUE
      IF(ZCSUM(1).EQ.ZTABL2(1)) ZCSUM(1)=IBLANK
      WRITE(PMS,90) ZCSUM(1),ZCSUM(2),ZCSUM(3),ZCSUM(4)
   90 FORMAT(/,' HEX CHECK SUM = ',4A1)
      RETURN
      END
C
C***********************************************************************
C
      BLOCK DATA
      IMPLICIT INTEGER (A-Z)
      COMMON /BLK/ PR8X10(10,14),PROD8(8,11),PRODLN(40,7)
      DATA PR8X10/
     1      4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
     2      3, 6, 5, 5, 5, 5, 5, 5, 6, 3,
     3      3, 3, 7, 7, 8, 8, 7, 7, 3, 3,
     4      3, 3, 3, 9,10,10, 9, 3, 3, 3,
     5      3, 3, 3, 3, 1, 1, 3, 3, 3, 3,
     6      2, 2, 2, 2, 1, 1, 3, 3, 3, 3,
     7     11,11,11,11,11,11,11,11,11,11,
     8     11,11,11,11,11,11,11,11,11,11,
     9     11,11,11,11,11,11,11,11,11,11,
C
     A     11,11,11,11,11,11,11,11,11,11,
     B      3, 1, 1, 1, 1, 1, 1, 1, 1, 3,
     C      3, 1, 1, 1, 1, 1, 1, 1, 1, 3,
     D      3, 1, 1, 1, 1, 1, 1, 1, 1, 3,
     E      3, 1, 1, 1, 1, 1, 1, 1, 1, 3/
      DATA PROD8/
     1     1,1,1,1,1,1,1,1,
     2     2,2,2,2,2,2,2,2,
     3     3,3,3,3,3,3,3,3,
     4     4,4,3,3,3,3,3,3,
     5     5,5,3,3,3,3,3,3,
     6     5,5,5,5,3,3,3,3,
     7     6,6,6,6,3,3,3,3,
     8     6,6,3,3,3,3,3,3,
     9     7,7,7,7,7,7,3,3,
     A     7,7,7,7,3,3,3,3,
     B     1,1,1,1,3,3,3,3/
      DATA PRODLN/
     1     40*1HX,
     2     40*1HP,
     3     40*1HN,
     4     6*1HX,2*1HP,2*1HX,2*1HP,2*1HX,2*1HP,2*1HX,
     4     2*1HP,2*1HX,2*1HP,2*1HX,2*1HP,2*1HX,2*1HP,
     4     2*1HX,2*1HP,4*1HX,
     5     10*1HX,2*1HP,2*1HX,2*1HP,2*1HX,2*1HP,2*1HX,
     5     2*1HP,2*1HX,2*1HP,2*1HX,2*1HP,8*1HX,
     6     14*1HX,2*1HP,2*1HX,2*1HP,2*1HX,2*1HP,2*1HX,
     6     2*1HP,12*1HX,
     7     18*1HX,2*1HP,2*1HX,2*1HP,16*1HX/
      END
C
C***********************************************************************
C
      SUBROUTINE TWEEK(ITYPE,LFUSES,LPHANT)
C     THIS SUBROUTINE TWEEKS LFUSES (THE PROGRAMMING FUSE PLOT)
C      FOR HIGH AND LOW PHANTOM FUSES
      IMPLICIT INTEGER (A-Z)
      LOGICAL LFUSES(40,80),LBLANK,LLEFT,LAND,LOR,LSLASH,
     1        LEQUAL,LRIGHT,LXOR,LPHANT(40,80)
      COMMON  LBLANK,LLEFT,LAND,LOR,LSLASH,LEQUAL,LRIGHT,LXOR
      COMMON /BLK/ PR8X10(10,14),PROD8(8,11),PRODLN(40,7)
      DATA P/'P'/,N/'N'/
      FUSPTR=1
      DO 30 OUTPUT=1,10
         GRTYPE=PR8X10(OUTPUT,ITYPE)
         DO 30 PRLINE=1,8
            LNTYPE=PROD8(PRLINE,GRTYPE)
            DO 20 COL=1,40
                 IF(PRODLN(COL,LNTYPE).NE.P)  GOTO 15  
                  LFUSES(COL,FUSPTR)=.TRUE.
                  LPHANT(COL,FUSPTR)=.TRUE.
   15            IF(PRODLN(COL,LNTYPE).NE.N) GOTO 20 
                      LFUSES(COL,FUSPTR)=.FALSE.
                      LPHANT(COL,FUSPTR)=.TRUE.
   20       CONTINUE
            FUSPTR=FUSPTR+1
   30 CONTINUE
C
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE BINR(LFUSES,H,L)
C     THIS SUBROUTINE GENERATES BINARY PROGRAMMING FORMATS
      IMPLICIT INTEGER (A-Z)
      INTEGER ITEMP(5,10)
      LOGICAL LFUSES(40,80)
      COMMON /LUNIT/ PMS,POF,PDF
      WRITE(PDF,10)
   10 FORMAT(//,'                                         .',//)
      DO 20 I=1,41,40
      INC=I-1
        DO 20 IPROD=1,8
            DO 20 J=1,31,10
              DO 15 K=1,10
                IINPUT=J+K-1
                ITEMP(1,K)=L
                ITEMP(2,K)=L
                ITEMP(3,K)=L
                ITEMP(4,K)=L
                ITEMP(5,K)=L
                ISUM3=IPROD+INC
                IF(LFUSES(IINPUT,ISUM3 +  0 )) ITEMP(5,K)=H
                IF(LFUSES(IINPUT,ISUM3 +  8 )) ITEMP(4,K)=H
                IF(LFUSES(IINPUT,ISUM3 + 16 )) ITEMP(3,K)=H
                IF(LFUSES(IINPUT,ISUM3 + 24 )) ITEMP(2,K)=H
                IF(LFUSES(IINPUT,ISUM3 + 32 )) ITEMP(1,K)=H
   15           CONTINUE
   20         WRITE(PDF,30) ITEMP
   30         FORMAT(' ',10('B',5A1,'F '))
      WRITE(PDF,10)
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE SLIP(LFUSES,I88PRO,ITYPE,IBLOW)
C     THIS SUBROUTINE WILL BLOW THE ENTIRE CONDITIONAL THREE-STATE
C      PRODUCT LINE WHEN 'IF(VCC)' CONDITION IS USED FOR THE
C      CORRESPONDING OUTPUT PIN
      IMPLICIT INTEGER (A-Z)
      INTEGER IENABL(10,14)
      LOGICAL LFUSES(40,80)
C     1=ENABLED OUTPUT.   0=ANYTHING ELSE FOR THAT OUTPUT
      DATA IENABL/
     1     0,0,0,0,0,0,0,0,0,0,
     2     0,0,0,0,0,0,0,0,0,0,
     3     0,0,0,0,0,0,0,0,0,0,
     4     0,0,0,0,0,0,0,0,0,0,
     5     0,0,0,0,0,0,0,0,0,0,
     6     0,0,0,0,0,0,0,0,0,0,
     7     1,1,1,1,1,1,1,1,1,1,
     8     0,0,0,0,0,0,0,0,0,0,
     9     1,0,0,0,0,0,0,0,0,1,
C
     A     1,1,1,0,0,0,0,1,1,1,
     B     0,1,1,1,1,1,1,1,1,0,
     C     0,0,0,0,0,0,0,0,0,0,
     D     0,1,0,0,0,0,0,0,1,0,
     E     0,1,1,0,0,0,0,1,1,0/
      IOUT=(I88PRO-1)/8+1
      IF(IENABL(IOUT,ITYPE).EQ.0) RETURN
      DO 10 I=1,40
      IBLOW = IBLOW + 1
   10 LFUSES(I,I88PRO) = .TRUE.
      I88PRO = I88PRO + 1
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE FANTOM(ITYPE,IOP,IOUT,IPROD,I8PRO)
C     THIS SUBROUTINE UPDATES IOUT (THE PRINTED FUSE PLOT)
C      FOR HIGH AND LOW PHANTOM FUSES
      IMPLICIT INTEGER (A-Z)
      INTEGER IOUT(64)
      LOGICAL LBLANK,LLEFT,LAND,LOR,LSLASH,LEQUAL,LRIGHT,LXOR
      COMMON  LBLANK,LLEFT,LAND,LOR,LSLASH,LEQUAL,LRIGHT,LXOR
      COMMON /BLK/ PR8X10(10,14),PROD8(8,11),PRODLN(40,7)

      DATA B/'B'/,N/'N'/,P/'P'/,LOFANT/'0'/,HIFANT/'O'/,IBLANK/' '/
C     GET OUTPUT GROUPING
      OUTPUT=(IPROD-1)/8+1
      GRTYPE=PR8X10(OUTPUT,ITYPE)
      LNTYPE=PROD8(I8PRO,GRTYPE)
      DO 10 COL=1,40
         IF( PRODLN(COL,LNTYPE).EQ.P.AND.IOP.EQ.P ) IOUT(COL)=HIFANT
         IF( PRODLN(COL,LNTYPE).EQ.P.AND.IOP.EQ.B ) IOUT(COL)=IBLANK
         IF( PRODLN(COL,LNTYPE).EQ.N )              IOUT(COL)=LOFANT
   10 CONTINUE
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE IODC2
C***** THIS ROUTINE IS OPTIONAL, IT MAY BE USED TO TURN PERIPHERALS ON
      IMPLICIT INTEGER (A-Z)
      INTEGER BEL,DC2
      COMMON /LUNIT/ PMS,POF,PDF
      DATA BEL/007/,DC2/022/
      WRITE(PDF,10) DC2,BEL
   10 FORMAT(' ',2A1)
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE IODC4
C***** THIS ROUTINE IS OPTIONAL, IT MAY BE USED TO TURN PERIPHERALS OFF
      IMPLICIT INTEGER (A-Z)
      INTEGER BEL,DC3,DC4
      DATA BEL/007/,DC3/023/,DC4/024/
C
      WRITE(PDF,10) BEL,DC3,DC4
   10 FORMAT(' ',3A1)
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE TEST(LPHASE,LBUF,TITLE,IC,IL,ILE,ISYM,IBUF,ITYPE,
     1                IPCTR,LERR,ISAF,IPCTR1,LSA11,LSA01,LPRINT)
C     THIS SUBROUTINE PERFORMS THE FUNCTION TABLE SIMULATION
C      AND GENERATES TEST VECTORS
      IMPLICIT INTEGER (A-Z)
      INTEGER ISYM(8,24),ISYM1(8,24),IBUF(8,24),IVECT(24),IVECTP(24),
     1        ISTATE(24),ISTATT(24),IPIN(24),TITLE(80),IPCTR,NTEST
     2        ,TSTVEC(24,42)
      LOGICAL LBLANK,LLEFT,LAND,LOR,LSLASH,LEQUAL,LRIGHT,LXOR,LSAME,
     1        XORFND,LERR,LPHASE(24),LPHAS1(24),LBUF(24),LOUT(24),
     2        LOUTP(24),LCLOCK,LPTRST,LCTRST,LENABL(24),NREG,
     3        LSA11,LSA12,LSA01,LSA02,LPRINT
      INTEGER BEL
      COMMON  LBLANK,LLEFT,LAND,LOR,LSLASH,LEQUAL,LRIGHT,LXOR
      COMMON /PGE/ IPAGE(80,200)
      COMMON /LUNIT/ PMS,POF,PDF
      COMMON /FTEST/ IFUNCT,IDESC,IEND
      COMMON /TSTVEC/ NTEST,TSTVEC
      DATA IDASH/'-'/,L/'L'/,H/'H'/,X/'X'/,C/'C'/,Z/'Z'/,N0/'0'/,
     1     N1/'1'/,ERR/'?'/,IBLANK/' '/,COMENT/';'/,NN/'N'/
      DATA BEL/007/
      NTEST = 0
C     PRINT AN ERROR MESSAGE IF NO FUNCTION TABLE IS SUPPLIED
      IF(IFUNCT.NE.0) GO TO 3
      WRITE(PMS,2)
    2 FORMAT(/,' FUNCTION TABLE MUST BE SUPPLIED IN ORDER TO PERFORM',
     1         ' SIMULATION')
      RETURN
C     PRINT TITLE
    3 IF((.NOT.LSA11).AND.(.NOT.LSA01).AND.LPRINT) WRITE(POF,4) TITLE
    4 FORMAT(/,' ',80A1,/)
C     INITIALIZE LERR (FUNCTION TABLE ERROR FLAG) TO NO ERROR
      LERR=.FALSE.
C     INITIALIZE NERR (NUMBER OF FUNCTION TABLE ERRORS) TO NO ERROR
      NERR=0
C     SET THE STARTING POINT OF THE FUNCTION TABLE TO COLUMN 0
C      AND IFUNCT + 1
      IC=0
      IL=IFUNCT + 1
C     INITIALISE SA1/SA0 PARAMETERS
      IPCTR3=0
      IEQN=0
      IPCTR=0
C     INITIALIZE ITRST (THREE-STATE ENABLE FUNCTION TABLE PIN NUMBER)
      ITRST=0
C     MAKE A DUMMY CALL TO INCR
      CALL INCR(IC,IL)
C     GET THE FUNCTION TABLE PIN LIST (UP TO 22)
C      GO ONE MORE THAN MAX TO LOOK FOR DASHED LINE
C
      DO 10 I=1,23
      CALL GETSYM(LPHAS1,ISYM1,I,IC,IL)
         DO 5 J=1,8
    5    IBUF(J,1)=ISYM1(J,I)
      IF(IBUF(8,1).EQ.IDASH) GO TO 12
      CALL MATCH(IMATCH,IBUF,ISYM)
      IF(IMATCH.NE.0) GO TO 7 
      WRITE(PMS,6) (IBUF(J,1),J=1,8)
    6 FORMAT(/,' FUNCTION TABLE PIN LIST ERROR AT', 8A1) 
      RETURN
    7 LOUT(I)=.FALSE.
      ISTATT(I)=X
      IVECTP(I)=X
C     IF APPROPIATE PAL TYPE, REMEMBER LOCATION OF CLOCK AND THREE-STATE    
C      ENABLE PIN IN FUNCTION TABLE PIN LIST
      IF( .NOT.(ITYPE.EQ.8.OR.ITYPE.EQ.9.OR.ITYPE.EQ.10.OR.
     1          ITYPE.EQ.12.OR.ITYPE.EQ.13.OR.ITYPE.EQ.14) ) GO TO 10
      IF(IMATCH.EQ.1)  ICLOCK=I
      IF(IMATCH.EQ.13) ITRST=I
   10 IPIN(I)=IMATCH 
C     ALL SIGNAL NAMES FOR THE FUNCTIONAL TEST HAVE BEEN READ IN
C      ADJUST COUNT
   12 IMAX=I-1
      NVECT=0
C
C*****START OF MAIN LOOP FOR SIMULATION*****
C
   90 IPCTR2=0
      IEQN=0
      IPCTR3=0
      LSA12=.FALSE.
      LSA02=.FALSE.
      NVECT=NVECT+1
      IC1=0
      IL1=ILE
C     GO PASSED COMMENT LINES
   23 IF(IPAGE(1,IL).NE.COMENT) GO TO 24
      IL=IL+1
      GO TO 23
   24 CONTINUE
C     GETS VECTORS FROM FUNCTION TABLE
      DO 20 I=1,IMAX
        IF(IPAGE(IC,IL).EQ.IBLANK) GO TO 21
        GO TO 22
   21   IC=IC+1
        IF(IPAGE(IC,IL).EQ.IBLANK) GO TO 21
   22   IVECT(I)=IPAGE(IC,IL)
        IC=IC+1
   20 CONTINUE
C     ADVANCE LINE COUNT TO SKIP FUNCTION TABLE COMMENTS
      IL=IL+1
      IC=1
      IF(IVECT(1).EQ.IDASH) GO TO 95
C     CHECK FOR VALID FUNCTION TABLE VALUES (L,H,X,Z,C)
      DO 11 I=1,IMAX
         IF( IVECT(I).EQ.L.OR.IVECT(I).EQ.H.OR.IVECT(I).EQ.X.OR.
C
     1       IVECT(I).EQ.Z.OR.IVECT(I).EQ.C) GO TO 11
         WRITE(PMS,8) IVECT(I),NVECT
    8    FORMAT(/,' ',A1,' IS NOT AN ALLOWED FUNCTION TABLE ENTRY',
     1                   ' IN VECTOR ',I3)
         RETURN
   11 CONTINUE
C     INITIALIZE CLOCK AND THREE-STATE ENABLE FLAGS
      LCLOCK=.FALSE.
      LCTRST=.TRUE.
      LPTRST=.TRUE.
      DO 13 I=1,IMAX
   13    LENABL(I)=.TRUE.
C     INITIALIZE NREG (NOT REGISTERED OUTPUT) TO FALSE
      NREG=.FALSE.
C     INITIALIZE ISTATE ARRAY TO ALL L & H 
      DO 15 I=1,24
      IF ((ISYM(1,I).NE.ISYM1(1,I))) GO TO 555
      ISTATE(I) = L
      GO TO 15
  555 ISTATE(I) = H
   15 CONTINUE
C     CHECK IF THIS PAL TYPE HAS REGISTERS
      IF( .NOT.(ITYPE.EQ.8.OR.ITYPE.EQ.9.OR.ITYPE.EQ.10.OR.
     1          ITYPE.EQ.12.OR.ITYPE.EQ.13.OR.ITYPE.EQ.14) ) GO TO 25
C     CHECK CLOCK AND THREE-STATE ENABLE PINS AND CHANGE FLAG IF NEEDED
      IF(IVECT(ICLOCK).EQ.C) LCLOCK=.TRUE.
      IF(ITRST.EQ.0) GO TO 25
      LSAME=( (     LPHASE(13)).AND.(     LPHAS1(ITRST)).OR.
     1        (.NOT.LPHASE(13)).AND.(.NOT.LPHAS1(ITRST)) )
      IF( IVECT(ITRST).EQ.L.AND.(.NOT.LSAME).OR.
     1    IVECT(ITRST).EQ.H.AND.(     LSAME) ) LPTRST=.FALSE.
      IF(LPTRST) GO TO 25
C     DISABLE REGISTERED OUTPUTS IF APPROPRIATE
      DO 46 I=1,IMAX
         J=IPIN(I)
         IF(J.EQ.17.OR.J.EQ.18.OR.J.EQ.19.OR.J.EQ.20) LENABL(I)=.FALSE.
         IF( (ITYPE.EQ.8.OR.ITYPE.EQ.9.OR.ITYPE.EQ.12.OR.
     1        ITYPE.EQ.13).AND.(J.EQ.16.OR.J.EQ.21) ) LENABL(I)=.FALSE.
         IF( (ITYPE.EQ.8.OR.ITYPE.EQ.9.OR.ITYPE.EQ.12).AND.
     1       (J.EQ.15.OR.J.EQ.22) )                   LENABL(I)=.FALSE.
         IF(  ITYPE.EQ.8.AND.(J.EQ.14.OR.J.EQ.23) )   LENABL(I)=.FALSE.
   46 CONTINUE
C
C*****SCAN THROUGH THE LOGIC EQUATIONS*****
C
C     MAKE A DUMMY CALL TO INCR
   25 CALL INCR(IC1,IL1)
   26 CALL GETSYM(LBUF,IBUF,1,IC1,IL1)
      IF(LLEFT) GO TO 29
   27 IF(.NOT.LEQUAL) GO TO 26
      IF(LEQUAL) IEQN=IEQN+1
C     EVALUATE CONDITIONAL THREE-STATE PRODUCT LINE
   29 IF(LEQUAL) GO TO 35
      NREG=.TRUE.
   33 CALL GETSYM(LBUF,IBUF,1,IC1,IL1)
      CALL MATCH(IINP,IBUF,ISYM1)
C     CHECK FOR GND, VCC, /GND, OR /VCC IN CONDITIONAL THREE-STATE
C     PRODUCT LINE
      IF(IINP.NE.0) GO TO 32
      CALL MATCH(IMATCH,IBUF,ISYM)
      ILL=IL1
C
      IF( IMATCH.EQ.12.AND.(LBUF(1)).OR.
     1    IMATCH.EQ.24.AND.(.NOT.LBUF(1)) ) LCTRST=.FALSE.
      IF( IINP.EQ.0.AND.IMATCH.NE.12.AND.IMATCH.NE.24 ) GO TO 100
      GO TO 34
   32 ITEST=IVECT(IINP)
      IF(  ITEST.EQ.L.AND.(     LPHAS1(IINP)).AND.(     LBUF(1)) 
     1.OR. ITEST.EQ.H.AND.(     LPHAS1(IINP)).AND.(.NOT.LBUF(1))
     2.OR. ITEST.EQ.H.AND.(.NOT.LPHAS1(IINP)).AND.(     LBUF(1))
     3.OR. ITEST.EQ.L.AND.(.NOT.LPHAS1(IINP)).AND.(.NOT.LBUF(1))
     4  )  LCTRST=.FALSE.
      IF(ITEST.EQ.X.OR.ITEST.EQ.Z) LCTRST=.FALSE.
   34 IF(LAND) GO TO 33
      GO TO 27
C
C     EVALUATE THE LOGIC EQUATION
C
C     FIND PIN NUMBER OF THE OUTPUT VECTORS
   35 IPCTR3=0
      CALL MATCH(IOUTP,IBUF,ISYM1)
C     FLAG FOR UNREGISTERED OUTPUTS
      CALL MATCH(IOUT,IBUF,ISYM)
      IF(ITYPE.LE.7.OR.ITYPE.EQ.11) NREG=.TRUE.
      IF( (ITYPE.EQ.9.OR.ITYPE.EQ.10).AND.(IOUT.EQ.14.OR.IOUT.EQ.23) )
     1     NREG=.TRUE.
      IF( (ITYPE.EQ.10.OR.ITYPE.EQ.13.OR.ITYPE.EQ.14).AND.
     1    (IOUT.EQ.15.OR.IOUT.EQ.22) ) NREG=.TRUE.
      IF( (ITYPE.EQ.10.OR.ITYPE.EQ.14).AND.(IOUT.EQ.16.OR.IOUT.EQ.21) )
     1     NREG=.TRUE.
      ILL=IL1
      IF(IOUTP.EQ.0) GO TO 100
      IF(NREG) LENABL(IOUTP)=LCTRST
      LOUT(IOUTP)=.TRUE.
      IF( .NOT.LCTRST ) LOUT(IOUTP)=.FALSE.
      LCTRST=.TRUE.
      LOUTP(IOUTP)=LBUF(1)
C     DETERMINE PRODUCT TERM AND EVENTUALLY SUM FOR OUTPUT KEEPING 
C      TRACK TO SEE IF AN XOR (EXCLUSIVE OR) HAS BEEN FOUND
      XORSUM=H
      XORFND=.FALSE.
      ISUM=L
   28 IPCTR2=IPCTR2+1
      IPCTR3=IPCTR3+1
      IPCTR=IPCTR+1        
      IPROD=H
   30 ILL=IL1
      CALL GETSYM(LBUF,IBUF,1,IC1,IL1)
      CALL MATCH(IINP,IBUF,ISYM1)
      IF(IINP.NE.0) GO TO 47
      CALL MATCH(IMATCH,IBUF,ISYM)
      IF(IMATCH.NE.12.AND.IMATCH.NE.24) GO TO 100
C     TWEEK FOR GND AND VCC IN PRODUCT LINE
      IF(IMATCH.EQ.12) ITEST=L
      IF(IMATCH.EQ.24) ITEST=H
      IINP=23
      LPHAS1(23)=.TRUE.
      GO TO 37
   47 ITEST=IVECT(IINP)
C     GET REGISTERED FEED BACK VALUES
C
      IF(NREG) GO TO 37
      CALL MATCH(IIFB,IBUF,ISYM)
      IF( (ITYPE.EQ.8.OR.ITYPE.EQ.9.OR.ITYPE.EQ.10.OR.ITYPE.EQ.12.OR.
     1     ITYPE.EQ.13.OR.ITYPE.EQ.14).AND.(IIFB.EQ.17.OR.IIFB.EQ.18.OR.
     2     IIFB.EQ.19.OR.IIFB.EQ.20) ) ITEST=IVECTP(IINP)
      IF( (ITYPE.EQ.8.OR.ITYPE.EQ.9.OR.ITYPE.EQ.12.OR.ITYPE.EQ.13).AND.
     1    (IIFB.EQ.16.OR.IIFB.EQ.21) ) ITEST=IVECTP(IINP)
      IF( (ITYPE.EQ.8.OR.ITYPE.EQ.9.OR.ITYPE.EQ.12).AND.
     1    (IIFB.EQ.15.OR.IIFB.EQ.22) ) ITEST=IVECTP(IINP)
      IF(  ITYPE.EQ.8.AND.(IIFB.EQ.14.OR.IIFB.EQ.23) )
     1                                 ITEST=IVECTP(IINP)
   37 IF(ITEST.EQ.X.OR.ITEST.EQ.Z) ITEST=L
      IF(  ITEST.EQ.L.AND.(     LPHAS1(IINP)).AND.(     LBUF(1)) 
     1.OR. ITEST.EQ.H.AND.(     LPHAS1(IINP)).AND.(.NOT.LBUF(1))
     2.OR. ITEST.EQ.H.AND.(.NOT.LPHAS1(IINP)).AND.(     LBUF(1))
     3.OR. ITEST.EQ.L.AND.(.NOT.LPHAS1(IINP)).AND.(.NOT.LBUF(1)) 
     4  )  IPROD=L
      IF((IPCTR2.EQ.IPCTR1).AND.(LSA11)) GO TO 110
   38 IF(LAND) GO TO 30
      IF ((IPCTR2.EQ.IPCTR1).AND.(LSA01)) GO TO 120
  121 IF(ISUM.EQ.L.AND.IPROD.EQ.X) ISUM=X
      IF( (ISUM.NE.H).AND.IPROD.EQ.H ) ISUM=H
C     CHECK FOR XOR (EXCLUSIVE OR) AND SAVE INTERMEDIATE VALUE 
      IF(.NOT.LXOR) GO TO 31
      XORSUM=ISUM
      XORFND=.TRUE.
      ISUM=L
      GO TO 28
   31 IF(LOR) GO TO 28
      IPCTR3=0
C     IF END OF EQUATION HAS BEEN FOUND, DETERMINE FINAL SUM AND SAVE IT    
      IF(.NOT.XORFND)    ISTATT(IOUTP)=ISUM
      IF( (XORFND).AND.((ISUM.EQ.L.AND.XORSUM.EQ.L).OR.
     1                  (ISUM.EQ.H.AND.XORSUM.EQ.H)) ) ISTATT(IOUTP)=L
      IF( (XORFND).AND.((ISUM.EQ.H.AND.XORSUM.EQ.L).OR.
     1                  (ISUM.EQ.L.AND.XORSUM.EQ.H)) ) ISTATT(IOUTP)=H
      IF( (XORFND).AND. (ISUM.EQ.X.OR. XORSUM.EQ.X) )  ISTATT(IOUTP)=X
C     REGISTER DOES NOT CHANGE STATE IF NO CLOCK PULSE IS RECEIVED
      IF( (LCLOCK).OR.(NREG) ) GO TO 36
      LSAME = ( (     LOUTP(IOUTP)).AND.(     LPHAS1(IOUTP)).OR.
     1          (.NOT.LOUTP(IOUTP)).AND.(.NOT.LPHAS1(IOUTP)) )
      IF( IVECTP(IOUTP).EQ.L.AND.(     LSAME) ) ISTATT(IOUTP)=L
      IF( IVECTP(IOUTP).EQ.H.AND.(     LSAME) ) ISTATT(IOUTP)=H
      IF( IVECTP(IOUTP).EQ.L.AND.(.NOT.LSAME) ) ISTATT(IOUTP)=H
      IF( IVECTP(IOUTP).EQ.H.AND.(.NOT.LSAME) ) ISTATT(IOUTP)=L
   36 NREG=.FALSE.
C     CHECK IF ALL EQUATIONS HAVE BEEN PROCESSED BY COMPARING CURRENT
C      LINE NUMBER WITH FUNCTION TABLE LINE NUMBER
      IF(IDESC.NE.0.AND.IL1.LT.IFUNCT.AND.IL1.LT.IDESC.OR.
     1   IDESC.EQ.0.AND.IL1.LT.IFUNCT) GO TO 27
C     DETERMINE OUTPUT LOGIC VALUES
C      COMPARE OUTPUTS TO SEE IF VECTOR AGREES WITH RESULTS
      DO 50 I=1,IMAX
      IF(.NOT.LOUT(I)) GO TO 50
      IF(ISTATT(I).EQ.X.AND.IVECT(I).EQ.X) GO TO 50
      LSAME = ( (     LOUTP(I)).AND.(     LPHAS1(I)).OR.
     1          (.NOT.LOUTP(I)).AND.(.NOT.LPHAS1(I)) )
C
      IMESS=40
      IF(ISTATT(I).EQ.L.AND.IVECT(I).EQ.L.AND.(.NOT.LSAME))  IMESS=41
      IF(ISTATT(I).EQ.H.AND.IVECT(I).EQ.H.AND.(.NOT.LSAME))  IMESS=42
      IF(ISTATT(I).EQ.L.AND.IVECT(I).EQ.H.AND.(     LSAME))  IMESS=42
      IF(ISTATT(I).EQ.H.AND.IVECT(I).EQ.L.AND.(     LSAME))  IMESS=41
      IF( (     LENABL(I)).AND.IVECT(I).EQ.Z )               IMESS=43
      IF( (.NOT.LENABL(I)).AND.(LOUT(I)).AND.IVECT(I).NE.Z ) IMESS=44
      IF(IMESS.NE.40) LERR=.TRUE.
      IF((.NOT.LERR).AND.((LSA11).OR.(LSA01))) GO TO 50
      IF((LERR).AND.((LSA11).OR.(LSA01))) GO TO 115
      IF(IMESS.EQ.41) WRITE(PMS,41) NVECT,(ISYM1(J,I),J=1,8)
   41 FORMAT(/,' FUNCTION TABLE ERROR IN VECTOR',I3,'  PIN =',8A1,
     1         '  EXPECT = H  ACTUAL = L')
      IF(IMESS.EQ.42) WRITE(PMS,42) NVECT,(ISYM1(J,I),J=1,8)
   42 FORMAT(/,' FUNCTION TABLE ERROR IN VECTOR',I3,'  PIN =',8A1,
     1         '  EXPECT = L  ACTUAL = H')
      IF(IMESS.EQ.43) WRITE(PMS,43) NVECT,(ISYM1(J,I),J=1,8)
   43 FORMAT(/,' FUNCTION TABLE ERROR IN VECTOR',I3,'  PIN =',8A1, 
     1       /,'  EXPECT  = OUTPUT ENABLE  ACTUAL = Z')
      IF(IMESS.EQ.44) WRITE(PMS,44) NVECT,(ISYM1(J,I),J=1,8),IVECT(I)
   44 FORMAT(/,' FUNCTION TABLE ERROR IN VECTOR',I3,'  PIN =',8A1,
     1         '  EXPECT = Z  ACTUAL = ',A1)
      IF( (IMESS.NE.40).AND.(PMS.EQ.6) ) WRITE(PMS,45) BEL
   45 FORMAT(' ',A1)
      IF(IMESS.NE.40) IVECT(I)=ERR
      IF(IMESS.NE.40) NERR=NERR+1
   50 CONTINUE
C     CHANGE THE ORDER OF VECTORS FROM THE ORDER OF APPEARANCE IN THE
C      FUNCTION TABLE TO THAT OF THE PIN LIST AND TWEEK FOR OUTPUT
      DO 65 I=1,24
         DO 55 J=1,IMAX
         IF(IPIN(J).NE.I) GO TO 55
         IF( IVECT(J).EQ.L.OR.IVECT(J).EQ.H ) GO TO 51
         ISTATE(I)=IVECT(J)
         GO TO 65
   51    LSAME=( (     LPHASE(I)).AND.(     LPHAS1(J)).OR.    
     1           (.NOT.LPHASE(I)).AND.(.NOT.LPHAS1(J)) )
         IF( ITYPE.EQ.6.AND.(I.EQ.18.OR.I.EQ.19) )  LOUT(J)=.TRUE.
         IF( (.NOT.LOUT(J)).AND.(     LSAME).AND.
     1         IVECT(J).EQ.L )                      ISTATE(I)=N0
         IF( (.NOT.LOUT(J)).AND.(     LSAME).AND.
     1         IVECT(J).EQ.H )                      ISTATE(I)=N1
         IF( (.NOT.LOUT(J)).AND.(.NOT.LSAME).AND.
     1         IVECT(J).EQ.L )                      ISTATE(I)=N1
         IF( (.NOT.LOUT(J)).AND.(.NOT.LSAME).AND.
     1         IVECT(J).EQ.H )                      ISTATE(I)=N0
         IF( (     LOUT(J)).AND.(     LSAME).AND.
     1         IVECT(J).EQ.L.AND.(     LENABL(J)) ) ISTATE(I)=L
         IF( (     LOUT(J)).AND.(     LSAME).AND.
     1         IVECT(J).EQ.H.AND.(     LENABL(J)) ) ISTATE(I)=H
         IF( (     LOUT(J)).AND.(.NOT.LSAME).AND.
     1         IVECT(J).EQ.L.AND.(     LENABL(J)) ) ISTATE(I)=H
         IF( (     LOUT(J)).AND.(.NOT.LSAME).AND.
     1         IVECT(J).EQ.H.AND.(     LENABL(J)) ) ISTATE(I)=L
         IF( IVECT(J).EQ.ERR )                      ISTATE(I)=ERR
         GO TO 65
   55 CONTINUE
C
C     SAVE PRESENT VECTORS FOR FEED BACK USED WITH NEXT SET OF VECTORS
C      IF CLOCK PULSE AND NOT Z (HI-Z IS ASYNCHRONOUS)
   65 IF( (LCLOCK).AND.IVECT(J).NE.Z ) IVECTP(J)=IVECT(J)
C     ASSIGN X TO GROUND PIN AND 1 TO VCC PIN
      ISTATE(12)=X
      ISTATE(24)=N1
C     PRINT TEST VECTORS
      IF((.NOT.LSA11).AND.(.NOT.LSA01).AND.LPRINT) WRITE(POF,60) NVECT,
     1   (ISTATE(I),I=1,24)
   60 FORMAT(' ',I3,' ',24A1)
	IF(NVECT.EQ.43) WRITE(PMS,1000)
1000	FORMAT(' WARNING: MORE THAN 42 VECTORS HAVE BEEN PROVIDED')
	IF (NVECT.GT.42) GO TO 90
	NTEST = NTEST + 1
        DO 1010 I=1,24
        IF (ISTATE(I).EQ.L) TSTVEC(I,NTEST) = L
	IF (ISTATE(I).EQ.H) TSTVEC(I,NTEST) = H
	IF (ISTATE(I).EQ.Z) TSTVEC(I,NTEST) = Z
	IF (ISTATE(I).EQ.X) TSTVEC(I,NTEST) = X
	IF (ISTATE(I).EQ.C) TSTVEC(I,NTEST) = C
	IF (ISTATE(I).EQ.N0) TSTVEC(I,NTEST) =N0
	IF (ISTATE(I).EQ.N1) TSTVEC(I,NTEST) =N1
        TSTVEC(12,NTEST) = NN
        TSTVEC(24,NTEST) = NN
        IF ( TSTVEC(13,NTEST).EQ.X ) TSTVEC(13,NTEST) = N0
1010    CONTINUE
	
      GO TO 90
C     TERMINATE SIMULATION
   95 IF((.NOT.LERR).AND.(LSA11).AND.LPRINT) WRITE(POF,150) IPCTR4,IEQN1
  150 FORMAT(' ',' PRODUCT: ',I3,' OF ','EQUATION',I3,
     1 ' UNTESTED(SA1) FAULT')
      IF((.NOT.LERR).AND.(LSA01).AND.LPRINT) WRITE(POF,155) IPCTR4,IEQN1
  155 FORMAT(' ',' PRODUCT: ',I3,' OF ','EQUATION',I3,
     1 ' UNTESTED(SA0) FAULT')
      IF((.NOT.LERR).AND.((.NOT.LSA11).AND.(.NOT.LSA01)).AND.LPRINT)
     1 WRITE(POF,67)
   67 FORMAT(/,' PASS SIMULATION')
      IPCTR=IPCTR/(NVECT-1)
      IF((     LERR).AND.((.NOT.LSA11).AND.(.NOT.LSA01)).AND.LPRINT)
     1 WRITE(POF,68) NERR
   68 FORMAT(/,' NUMBER OF FUNCTION TABLE ERRORS =',I3)
      RETURN
C     PRINT AN ERROR MESSAGE FOR AN UNDEFINED PIN NAME
  100 ILERR=ILL+4
      WRITE(PMS,101) (IBUF(I,1),I=1,8),ILERR,(IPAGE(I,ILL),I=1,80)
  101 FORMAT(/,' ERROR SYMBOL =  ',8A1,'      IN LINE NUMBER ',I3,
     1       /,' ',80A1,/,' THIS PIN NAME IS NOT DEFINED IN THE',
     2                    ' FUNCTION TABLE PIN LIST')
      RETURN
  110 IPROD=H
      LSA12=.TRUE.
      IEQN1=IEQN
      IPCTR4=IPCTR3
      GO TO 38
  120 IPROD=L
      LSA02=.TRUE.
C
      IEQN1=IEQN
      IPCTR4=IPCTR3
      GO TO    121
  115 ISAF=ISAF+1
      LERR=.FALSE.
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE INTEL(LFUSES,IOP)
C     THIS SUBROUTINE GENERATES THE INTEL HEX PROGRAMMING FORMAT
      IMPLICIT INTEGER (A-Z)
      LOGICAL LFUSES(40,80)
      INTEGER  ZTABLE(16),IOP,ITEMP(2,40)
      COMMON /LUNIT/ PMS,POF,PDF
      DATA
     1    ZTABLE/'0','1','2','3','4','5','6','7',
     2           '8','9','A','B','C','D','E','F'/
      ADDR = 0
      DO 15 I=1,41,40
      INC=I-1
        DO 15 IPROD=1,8
          CSUM = MOD(ADDR/256+MOD(ADDR,256)+40,256)
          DO 10 IINPUT=1,40
              IHEX=0
              ISUM2=IPROD + INC
              IF(LFUSES(IINPUT,ISUM2 +  0 )) IHEX=IHEX+1
              IF(LFUSES(IINPUT,ISUM2 +  8 )) IHEX=IHEX+2
              IF(LFUSES(IINPUT,ISUM2 + 16 )) IHEX=IHEX+4
              IF(LFUSES(IINPUT,ISUM2 + 24 )) IHEX=IHEX+8
              IF(LFUSES(IINPUT,ISUM2 + 32 )) IHEX=IHEX+16
              CSUM=MOD(CSUM+IHEX,256)
              ITEMP(1,IINPUT)=ZTABLE(IHEX/16+1)
              ITEMP(2,IINPUT)=ZTABLE(MOD(IHEX,16)+1)
   10     CONTINUE
          IF(CSUM.NE.0) CSUM=256-CSUM
          WRITE(PDF,60) ZTABLE(ADDR/4096+1),ZTABLE(MOD(ADDR/256,16)+1),
     1                  ZTABLE(MOD(ADDR/16,16)+1),
     2                  ZTABLE(MOD(ADDR,16)+1),ITEMP,
     3                  ZTABLE(CSUM/16+1),ZTABLE(MOD(CSUM,16)+1)
          ADDR = ADDR + 40
   15   CONTINUE
   60 FORMAT(' :28',4A1,'00',80A1,2A1)
      WRITE(PDF,25)
   25 FORMAT(' :00000001FF')
      RETURN
      END
C
C  ********************************************************************
C 
      SUBROUTINE EXIT
      STOP
      END
C
C  ******************************END************************************
