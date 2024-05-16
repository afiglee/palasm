C**PALASM20**PALASM20**PALASM20**PALASM20**PALASM20**PALASM20**PALASM20
C
C  P A L A S M  2 0  -  TRANSLATES SYMBOLIC EQUATIONS INTO PAL OBJECT
C                       CODE  FORMATTED FOR DIRECT INPUT TO STANDARD
C                       PROM PROGRAMMERS.
C
C                       REV LEVEL:   VERSION 1.6C (08/16/83)
C                       (C) COPYRIGHT 1983 MONOLITHIC MEMORIES
C
C
C                       *********************************************       
C                       *                                           *
C                       *            APPROVAL                       *
C                       *                                           *
C                       * 1:JOHN BIRKNER                            *
C                       *                                           *
C                       *  PROGRAMMABLE LOGIC PLANNER               *
C                       *                                           *
C                       *                                           *
C                       * 2:VINCENT COLI                            *
C                       *                                           *
C                       * APPLICATIONS ENGINEER                     *
C                       *                                           *
C                       *                                           *
C                       * 3:MANOUCHEHR VAFAI                        *
C                       *                                           *
C                       * APPLICATIONS ENGINEER                     *
C                       *                                           *
C                       *                                           *
C                       *********************************************
C
C  
C
C                       INPUT:       PAL DESIGN SPECIFICATION ASSIGNED
C                                    TO RPD(1).  OPERATION CODES ARE
C                                    ASSIGNED TO ROP(5).
C
C                       OUTPUT:      ECHO, SIMULATION, AND FUSE PATTERN
C                                    ARE ASSIGNED TO *(6).  HEX AND
C                                    BINARY PROGRAMMING FORMATS ARE
C                                    ASSIGNED TO *(6).  PROMPTS AND
C                                    ERROR MESSAGES ARE ASSIGNED TO
C                                    *(6).
C
C                       PART NUMBER: THE PAL PART NUMBER MUST APPEAR
C                                    IN COLUMN ONE OF LINE ONE.
C
C                       PIN LIST:    20 SYMBOLIC PIN NAMES MUST APPEAR
C                                    STARTING ON LINE FIVE.
C
C                       EQUATIONS:   STARTING FIRST LINE AFTER THE
C                                    PIN LIST IN THE FOLLOWING FORMS:
C
C                                       A = B*C + D
C
C                                       A := B*C + D
C
C                                       IF( A*B )  C = D + E
C
C                                       A2 := (A1:*:B1) + /C
C
C
C                                    ALL CHARACTERS FOLLOWING ';' ARE
C                                    IGNORED UNTIL THE NEXT LINE.
C
C
C                                    BLANKS ARE IGNORED.
C
C                       OPERATORS:   ( IN HIERARCHY OF EVALUATION )
C
C                                     ;    COMMENT FOLLOWS
C                                     /    COMPLEMENT
C                                     *    AND (PRODUCT)
C                                     +    OR (SUM)
C                                    :+:   XOR (EXCLUSIVE OR)
C                                    :*:   XNOR (EXCLUSIVE NOR)
C                                    ( )   CONDITIONAL THREE-STATE
C                                          OR FIXED SYMBOL
C                                     =    EQUALITY
C                                    :=    REPLACED BY (AFTER CLOCK)
C
C                       FIXED SYMBOLS
C                       FOR PAL16X4
C                       AND PAL16A4
C                       ONLY:        (AN+/BN)     WHERE N = 0,1,2,3
C                                    (AN+BN)      FOR OUTPUT PINS
C                                    (AN)         17,16,15,14, RESP
C                                    (/AN+/BN)    A IS OUTPUT
C                                    (/BN)        B IS INPUT
C                                    (AN:+:BN)
C                                    (AN*/BN)
C                                    (/AN+BN)
C                                    (AN:*:BN)
C                                    (BN)
C                                    (AN*BN)
C                                    (/AN)
C                                    (/AN*/BN)
C                                    (/AN*BN)
C
C                       FUNCTION     L, H, X, Z, AND C ARE VALID
C                         TABLE:     FUNCTION TABLE VECTOR ENTRIES.
C
C                       REFERENCE:   A COMPLETE USERS GUIDE TO
C                                    DESIGNING WITH PALS USING PALASM
C                                    IS PROVIDED IN THE MONOLITHIC
C                                    MEMORIES PAL HANDBOOK.
C
C                       SUBROUTINES: INITLZ,GETSYM,INCR,MATCH,FIXSYM,
C                                    IXLATE,ECHO,CAT,PINOUT,PLOT,TWEEK,
C                                    BINR,HEX,SLIP,FANTOM,IODC2,IODC4,
C                                    TEST,FIXTST,PLOTF,SUMCHK,INTEL
C
C                       AUTHORS:     JOHN BIRKNER AND VINCENT COLI
C                                    FAULT TESTING BY IMTIAZ BENGALI
C                                    JEDEC FORMAT  BY MANO VAFAI 
C                                    MONOLITHIC MEMORIES INC.
C                                    1165 EAST ARQUES AVENUE
C
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
C                                    COMPUTER AND A NATIONAL CSS IBM
C                                    SYSTEM/370 FORTRAN IV(G).
C
C***********************************************************************
C
C
C***********************************************************************
C
C
C     MAIN PROGRAM
C
      IMPLICIT INTEGER (A-Z)
      LOGICAL LBLANK,LLEFT,LAND,LOR,LSLASH,LEQUAL,LRIGHT,LXOR,LXNOR,
     1        LFIX,LFIRST,LFUSES(32,64),LPHASE(20),LBUF(20),
     2        LPROD(80),LSAME,LACT,LOPERR,LINP,LPRD,LERR,LSA11,LSA01,
     3        LPHANT(32,64)
      INTEGER IPROD,IBUF(8,20),
     3         ISYM(8,20),
     4         PATNUM(80),COMP(80)
      CHARACTER INOO,IOT,INOAI,IPAL(4),E,O,T,P,B,H,S,L,N,C,Q,F,A,
     1         JJ,BB,CC,DD,EE,FF,II,NN,OO,PP,RR,SS,TT,UU,
     2         IPAGE(80,200),REST(73),IOP,TITLE(80)
      INTEGER BEL
      COMMON  LBLANK,LLEFT,LAND,LOR,LSLASH,LEQUAL,LRIGHT,LXOR,LXNOR
      COMMON /PGE/ IPAGE
      COMMON /LFUSES/LFUSES,LPHANT
      COMMON /FTEST/ IFUNCT,IDESC,IEND
      DATA E/'E'/,O/'O'/,T/'T'/,P/'P'/,B/'B'/,H/'H'/,S/'S'/,L/'L'/,
     1     N/'N'/,C/'C'/,Q/'Q'/,F/'F'/,A/'A'/,JJ/'J'/
      DATA BB/'B'/,CC/'C'/,DD/'D'/,EE/'E'/,FF/'F'/,II/'I'/,NN/'N'/,
     1     OO/'O'/,PP/'P'/,RR/'R'/,SS/'S'/,TT/'T'/,UU/'U'/
      DATA BEL /007/
C
C
C     ASSIGNMENT OF DATA SET REFERENCES
C     RPD - PAL DESIGN SPECIFICATION (INPUT)
C     ROC - OPERATION CODE (INPUT)
C     * - ECHO, PINOUT, TEST, AND PLOT (OUTPUT)
C     * - HEX AND BINARY FORMAT PROGRAM TAPES (OUTPUT)
C     * - PROMPTS AND ERROR MESSAGES (OUTPUT)
      WRITE(6,3)
    3 FORMAT(/,' MONOLITHIC MEMORIES INC. PALASM VERSION 1.6C')
      WRITE(6,33)
   33 FORMAT(' (C) COPYRIGHT 1983 MONOLITHIC MEMORIES')
      WRITE(6,1)
    1 FORMAT(/,' WHAT IS THE LOGICAL UNIT NUMBER FOR OUTPUT(6)?: '$)
C
C      READ(5,2) LUN
C    2 FORMAT(I4)
C
C

      IFUNCT=0
      IDESC=0
C     INITIALIZE LSAME AND LACT TO FALSE (ACTIVE HIGH/LOW ERROR)
      LSAME=.FALSE.
      LACT=.FALSE.
C     INITIALIZE LOPERR TO FALSE (OUTPUT PIN ERROR)
      LOPERR=.FALSE.
C     INITIALIZE LINP TO FALSE (INPUT PIN ERROR)
      LINP=.FALSE.
C     INITIALIZE LPRD TO FALSE (PRODUCT LINE ERROR)
      LPRD=.FALSE.
C    
C
C          
C
C     READ IN FIRST 4 LINES OF THE PAL DESIGN SPECIFICATION
C      READ(*,10) IPAL,INOAI,IOT,INOO,REST,PATNUM,TITLE,COMP
C   10 FORMAT(4A1,A1,A1,A1,73A1,/,80A1,/,80A1,/,80A1)
      IPAL(1)='P'
      IPAL(2)='A'
      IPAL(3)='L'
      IPAL(4)='1'
      INOAI='6'
      IOT='L'
      INOO='8'

C     READ IN PIN LIST (LINE 5) THROUGH THE END OF THE PAL DESIGN
C      SPECIFICATION
      DO 15 J=1,200
          READ(*,11,END=16) (IPAGE(I,J),I=1,80)
   11     FORMAT(80A1)
C     CHECK FOR 'FUNCTION TABLE' AND SAVE ITS LINE NUMBER
      IF(     IFUNCT.EQ.0 .AND.IPAGE(1,J).EQ.FF.AND.
     1    IPAGE(2,J).EQ.UU.AND.IPAGE(3,J).EQ.NN.AND.
     2    IPAGE(4,J).EQ.CC.AND.IPAGE(5,J).EQ.TT.AND.
     3    IPAGE(6,J).EQ.II.AND.IPAGE(7,J).EQ.OO.AND.
     4    IPAGE(8,J).EQ.NN.AND.IPAGE(10,J).EQ.TT.AND.
     5    IPAGE(12,J).EQ.BB.AND.IPAGE(14,J).EQ.EE ) IFUNCT=J
C     CHECK FOR 'DESCRIPTION' AND SAVE ITS LINE NUMBER
      IF(      IDESC.EQ.0 .AND.IPAGE(1,J).EQ.DD.AND.
     1    IPAGE(2,J).EQ.EE.AND.IPAGE(3,J).EQ.SS.AND.
     2    IPAGE(4,J).EQ.CC.AND.IPAGE(5,J).EQ.RR.AND.
     3    IPAGE(6,J).EQ.II.AND.IPAGE(7,J).EQ.PP.AND.
     4    IPAGE(8,J).EQ.TT.AND.IPAGE(9,J).EQ.II.AND.
     5    IPAGE(10,J).EQ.OO.AND.IPAGE(11,J).EQ.NN ) IDESC=J
   15 CONTINUE
C     SAVE THE LAST LINE NUMBER OF THE PAL DESIGN SPECIFICATION
   16 IEND=J-1
      CALL INITLZ(INOAI,IOT,INOO,ITYPE,LFUSES,LPHANT,IC,IL,IBLOW,
     1            LFIX,IPCTR)
      ILE=IL+1
C     PRINT ERROR MESSAGE FOR INVALID PAL PART TYPE
      IF(ITYPE.NE.0) GO TO 17
          WRITE(*,18) IPAL,INOAI,IOT,INOO
C
   18     FORMAT(/,' PAL PART TYPE ',4A1,A1,A1,A1,' IS INCORRECT')
          CALL EXIT1
C     GET 20 PIN NAMES
   17 DO 20 J=1,20
   20     CALL GETSYM(LPHASE,ISYM,J,IC,IL,LFIX)
          IF(.NOT.(LEQUAL.OR.LLEFT.OR.LAND.OR.LOR.OR.LRIGHT)) GO TO 24
              WRITE(*,23)
   23         FORMAT(/,' LESS THAN 20 PIN NAMES IN PIN LIST')
              CALL EXIT1
   24 ILE=IL
C     BYPASS FUSE PLOT ASSEMBLY IF HAL (H IN COLUMN 1, LINE 1)
      IF( IPAL(1).EQ.H ) GO TO 108
   25 CALL GETSYM(LBUF,IBUF,1,IC,IL,LFIX)
   28     IF(.NOT.LEQUAL) GO TO 25
          COUNT=0
          ILL=IL
          CALL MATCH(IMATCH,IBUF,ISYM)
          IF( IMATCH.EQ.0 ) GO TO 100
          IPRD=IMATCH
C         CHECK FOR VALID POLARITY
          LSAME = ( (     LPHASE(IMATCH)).AND.(     LBUF(1)).OR.
     1              (.NOT.LPHASE(IMATCH)).AND.(.NOT.LBUF(1)) )
          IF( IOT.EQ.H.AND.(.NOT.LSAME) )                 LACT=.TRUE.
          IF( (.NOT.(IOT.EQ.H.OR.IOT.EQ.C)).AND.(LSAME) ) LACT=.TRUE.
C         CHECK FOR VALID OUTPUT PIN
          IF( (ITYPE.EQ.1.OR.ITYPE.EQ.5.OR.ITYPE.EQ.6).AND.IOT.NE.A.
     1    AND.(IMATCH.LT.12.OR.IMATCH.GT.19) ) LOPERR=.TRUE.
          IF(  ITYPE.EQ.2.AND.(IMATCH.LT.13.OR.IMATCH.GT.18) )
     1                                         LOPERR=.TRUE.
          IF(  ITYPE.EQ.3.AND.(IMATCH.LT.14.OR.IMATCH.GT.17) )
     1                                         LOPERR=.TRUE.
          IF(  ITYPE.EQ.4.AND.(IMATCH.LT.15.OR.IMATCH.GT.16) )
     1                                         LOPERR=.TRUE.
          IF( (LACT).OR.(LOPERR) ) GO TO 100
          I88PRO=(19-IMATCH)*8 + 1
C         START PAL16C1 ON PRODUCT LINE 24 (I88PRO=25)
          IF(IOT.EQ.C) I88PRO=25
          IC=0
   30       CALL INCR(IC,IL,LFIX)
            IF( .NOT.(LEQUAL.OR.LLEFT) ) GO TO 30
            LPROD(I88PRO)=.TRUE.
            IF( (.NOT.LLEFT).AND.(REST(3).NE.PP) ) CALL SLIP(LFUSES,
     1         I88PRO,INOAI,IOT,INOO,IBLOW)
          DO 70 I8PRO=1,16
              COUNT = COUNT + 1
              IF( (LXOR).AND.I8PRO.NE.5 ) GO TO 70
              IPROD = I88PRO + I8PRO - 1
              LPROD(IPROD)=.TRUE.
              LFIRST=.TRUE.
   50           ILL=IL
                CALL GETSYM(LBUF,IBUF,1,IC,IL,LFIX)
              IF( (ITYPE.EQ.1.OR.ITYPE.EQ.2.AND.IPRD.GT.13
     1             .AND.IPRD.LT.18).AND.COUNT.GT.2 ) LPRD=.TRUE.
              IF( (ITYPE.EQ.3.OR.ITYPE.EQ.2.AND.(IPRD.EQ.13.OR.
     1             IPRD.EQ.18)).AND.COUNT.GT.4 ) LPRD=.TRUE.
C
              IF( IOT.NE.A.AND.IOT.NE.C.AND.COUNT.GT.8 ) LPRD=.TRUE.
              IF( .NOT.LPRD ) GO TO 69
              IF(IL.NE.IFUNCT.AND.IL.NE.IDESC) ILL=IL
              IPROD = IPROD - 1
              GO TO 118
   69           IF(LFIX) GO TO 59
                CALL MATCH(IMATCH,IBUF,ISYM)
C               CHECK FOR INVALID INPUT PIN
                IF( ITYPE.EQ.1.AND.(IMATCH.GE.12.AND.IMATCH.LE.19) )
     1              LINP=.TRUE.
                IF( ITYPE.EQ.2.AND.(IMATCH.GE.13.AND.IMATCH.LE.18) )
     1              LINP=.TRUE.
                IF( ITYPE.EQ.3.AND.(IMATCH.GE.14.AND.IMATCH.LE.17) )
     1              LINP=.TRUE.
                IF( ITYPE.EQ.4.AND.(IMATCH.EQ.15.OR.IMATCH.EQ.16) )
     1              LINP=.TRUE.
                IF( ITYPE.EQ.5.AND.(IMATCH.EQ.12.OR.IMATCH.EQ.19) )
     1              LINP=.TRUE.
                IF( ITYPE.EQ.6.AND.(IMATCH.EQ.1.OR.IMATCH.EQ.11) )
     1              LINP=.TRUE.
                ILL=IL
                IF(LINP) GO TO 100
                IF( IMATCH.EQ.0 ) GO TO 100
                IF( IMATCH.EQ.10.OR.IMATCH.EQ.99 ) GO TO 64
                IF(.NOT.LFIRST) GO TO 58
                    LFIRST=.FALSE.
                    DO 56 I=1,32
                        IBLOW = IBLOW + 1
   56                   LFUSES(I,IPROD)=.TRUE.
   58           CALL IXLATE(IINPUT,IMATCH,LPHASE,LBUF,ITYPE)
                IF(IINPUT.LE.0) GO TO 60
                IBLOW = IBLOW - 1
                LFUSES(IINPUT,IPROD)=.FALSE.
                CALL PLOT(LBUF,IBUF,LFUSES,IPROD,TITLE,.FALSE.,ITYPE,
     1                    LPROD,IOP,IBLOW,IPCTR) 
                GO TO 60
   59           CALL FIXSYM(LBUF,IBUF,IC,IL,LFIRST,LFUSES,IBLOW,
     1                      IPROD,LFIX)
   60           IF(LAND) GO TO 50
   64         IF(.NOT.LRIGHT) GO TO 68
   66           CALL INCR(IC,IL,LFIX)
                IF(.NOT.LEQUAL)  GO TO 66
   68         IF( .NOT.(LOR.OR.LEQUAL) ) GO TO 74
   70         CONTINUE
   74     ILL=IL
          CALL GETSYM(LBUF,IBUF,1,IC,IL,LFIX)
          IF(LLEFT.OR.LEQUAL) GO TO 28
  100 IF( ILL.EQ.IFUNCT.OR.ILL.EQ.IDESC ) GO TO 102
C     PRINT AN ERROR MESSAGE IF UNRECOGNIZABLE SYMBOL
      ILERR=ILL+4
      WRITE(*,99) BEL
   99 FORMAT(' ',A1)
      WRITE(*,101) (IBUF(I,1),I=1,8),ILERR,(IPAGE(I,ILL),I=1,80)
  101 FORMAT(/,' ERROR SYMBOL = ',8A1,'      IN LINE NUMBER ',I4,
     1       /,' ',80A1)
C
C     PRINT AN ERROR MESSAGE FOR ACTIVE HIGH/LOW PART
      IF( (LACT).AND.(     LSAME).AND.(.NOT.LOPERR) )
     1         WRITE(*,103) IPAL,INOAI,IOT,INOO
  103 FORMAT(' OUTPUT MUST BE INVERTED SINCE ',4A1,A1,A1,A1,
     1       ' IS AN ACTIVE LOW DEVICE')
      IF( (LACT).AND.(.NOT.LSAME).AND.(.NOT.LOPERR) )
     1         WRITE(*,109) IPAL,INOAI,IOT,INOO
  109 FORMAT(' OUTPUT CANNOT BE INVERTED SINCE ',4A1,A1,A1,A1,
     1       ' IS AN ACTIVE HIGH DEVICE')
C     PRINT AN ERROR MESSAGE FOR AN INVALID OUTPUT PIN
      IF( (LOPERR).AND.IMATCH.NE.0 )
     1         WRITE(*,105) IMATCH,IPAL,INOAI,IOT,INOO
  105 FORMAT(' THIS PIN NUMBER ',I2,' IS AN INVALID OUTPUT PIN',
     1       ' FOR ',4A1,A1,A1,A1)
C     PRINT AN ERROR MESSAGE FOR AN INVALID INPUT PIN
      IF(LINP) WRITE(*,115) IMATCH,IPAL,INOAI,IOT,INOO
  115 FORMAT(' THIS PIN NUMBER ',I2,' IS AN INVALID INPUT PIN',
     1       ' FOR ',4A1,A1,A1,A1)
C     PRINT AN ERROR MESSAGE FOR INVALID PRODUCT LINE
  118 ILERR=ILL+4
      IF(LPRD) WRITE(*,119)
     1 (ISYM(I,IPRD),I=1,8),IPRD,ILERR,(IPAGE(I,ILL),I=1,80)
  119 FORMAT(/,' OUTPUT PIN NAME = ',8A1,'  OUTPUT PIN NUMBER = ',I2,
     1       /,' MINTERM IN LINE NUMBER ',I4,/,' ',80A1)
      IF( LPRD.AND.COUNT.LT.8 )
     1         WRITE(*,116) IPROD,IPAL,INOAI,IOT,INOO
  116 FORMAT(' THIS PRODUCT LINE NUMBER ',I2,' IS NOT VALID',
     1       ' FOR ',4A1,A1,A1,A1)
      IF( LPRD.AND.COUNT.GT.8 )
     1         WRITE(*,117) IPAL,INOAI,IOT,INOO
  117 FORMAT(' MAXIMUM OF 8 PRODUCT LINES ARE VALID FOR ',4A1,A1,A1,A1,
     1     /,' TOO MANY MINTERMS ARE SPECIFIED IN THIS EQUATION')
      CALL EXIT1
  102 IF(ITYPE.LE.4) CALL TWEEK(ITYPE,IOT,LFUSES,LPHANT)
  108 WRITE(6,106)
  106 FORMAT(/,' OPERATION CODES:')
      WRITE(6,107)
  107 FORMAT(/,' E=ECHO INPUT  O=PINOUT  T=SIMULATE  P=PLOT ',
     1       /,' B=BRIEF H=HEX  S=SHORT  L=BHLF  N=BNPF   ',
     2       /,' C=CATALOG      Q=QUIT      F=FAULT TESTING  ',
     3       /,'      J=JEDEC FORMAT   I= INTEL HEX')
      WRITE(6,110)
  110 FORMAT(/,' ENTER OPERATION CODE: ',$)
      READ(*,120) IOP
  120 FORMAT(A1)
C     CALL IODC2
C     IF(*.NE.6) WRITE(*,125)
C 125 FORMAT('1')
      IF(IOP.EQ.E) CALL ECHO(IPAL,INOAI,IOT,INOO,REST,PATNUM,TITLE,
     1                       COMP)
      IF(IOP.EQ.O) CALL PINOUT(IPAL,INOAI,IOT,INOO,TITLE)
      IF(IOP.EQ.T) CALL TEST(LPHASE,LBUF,TITLE,IC,IL,ILE,ISYM,IBUF,
     1                       ITYPE,INOO,LFIX,IPCTR,LERR,ISAF,IPCTR1,
     2                       .FALSE.,.FALSE.,IOP.NE.JJ)
C     THE FOLLOWING IS ADDED FOR SA1 TEST
C
C     INITIALIZING THE TOTAL FAULTS. CALLING FOR SA1/SA0 TEST 
      ISAF=0
      IF(IOP.EQ.F) GO TO 200
C     END OF ADDITION
C
C        ADDITIONS MADE  TO GENERATE TEST VECTORS
C        FOR JEDEC FORMAT
C
         IF (IOP.NE.JJ) GOTO 135 
              CALL TEST(LPHASE,LBUF,TITLE,IC,IL,ILE,ISYM,IBUF,
     1                   ITYPE,INOO,LFIX,IPCTR,LERR,ISAF,IPCTR1,
     2                   .FALSE.,.FALSE.,IOP.NE.JJ)
                  CALL PLOTF(ITYPE,IOT)
C
C        
C
  135 IF(IOP.EQ.P) CALL PLOT(LBUF,IBUF,LFUSES,IPROD,TITLE,.TRUE.,ITYPE,
     1                       LPROD,IOP,IBLOW,IPCTR0)
      IF(IOP.EQ.B) CALL PLOT(LBUF,IBUF,LFUSES,IPROD,TITLE,.TRUE.,ITYPE,
     1                       LPROD,IOP,IBLOW,IPCTR0)
      IF(IOP.EQ.H) CALL HEX(LFUSES,H)
      IF(IOP.EQ.S) CALL HEX(LFUSES,S)
      IF(IOP.EQ.L) CALL BINR(LFUSES,H,L)
      IF(IOP.EQ.N) CALL BINR(LFUSES,P,N)
      IF(IOP.EQ.C) CALL CAT
C     CALL IODC4
C
C        
C
C - Dmitriy - commented out IF(G.EQ.II) CALL INTEL(LFUSES,II)
      IF(IOP.NE.Q ) GO TO 108
      CALL EXIT1
C     ADDITION FOR SA1/SA0 TESTS
C     SETTING THE PARAMETERS FOR SA1/SA0 TESTS
  200 IPCTR=0
      CALL TEST(LPHASE,LBUF,TITLE,IC,IL,ILE,ISYM,IBUF,ITYPE,INOO,
     1          IFIX,IPCTR,LERR,ISAF,IPCTR1,.FALSE.,.FALSE.,IOP.NE.JJ)
      IF(IFUNCT.EQ.0) GO TO 135
      IPCTR0=IPCTR
C     LOOPING FOR SA1 TEST
      DO 210 IPCTR1=1,IPCTR0
      LSA11=.TRUE.
      CALL TEST(LPHASE,LBUF,TITLE,IC,IL,ILE,ISYM,IBUF,ITYPE,INOO,
     1          IFIX,IPCTR,LERR,ISAF,IPCTR1,LSA11,.FALSE.,IOP.NE.JJ)
  210 CONTINUE
      ISA1=ISAF
C     LOOPING FOR SA0 TEST
      DO 215 IPCTR1=1,IPCTR0
      LSA01=.TRUE.
      CALL TEST(LPHASE,LBUF,TITLE,IC,IL,ILE,ISYM,IBUF,ITYPE,INOO,
     1          IFIX,IPCTR,LERR,ISAF,IPCTR1,.FALSE.,LSA01,IOP.NE.JJ)
  215 CONTINUE
      ISA0=ISAF-ISA1  
      IFAULT=(ISAF*100)/(IPCTR0*2)
      WRITE(*,220) ISA1
C
  220 FORMAT(/,' NUMBER OF STUCK AT ONE (SA1)  FAULTS ARE =' I3)
      WRITE(*,225) ISA0
  225 FORMAT(/,' NUMBER OF STUCK AT ZERO (SA0) FAULTS ARE =' I3)
      WRITE(*,230) IFAULT
  230 FORMAT(/,' PRODUCT  TERM   COVERAGE                 =' I3,'%',//)
      GO TO 135 
C     END OF ADDITION FOR SA1/SA0 TEST 
      END

C**************************************************
C
C
      CHARACTER FUNCTION ICONV(K)
      IMPLICIT INTEGER (A-Z)
      CHARACTER A,B,C,D,E,F,G,H
     1          ,I,J,X,L,M,N,O,P
      DATA A/'0'/,B/'1'/,C/'2'/,D/'3'/,E/'4'/,F/'5'/,G/'6'/,H/'7'/
      DATA I/'8'/,J/'9'/,X/'A'/,L/'B'/,M/'C'/,N/'D'/,O/'E'/,P/'F'/
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
C
C***********************************************************************
C
      SUBROUTINE INITLZ(INOAI,IOT,INOO,ITYPE,LFUSES,LPHANT,IC,IL,IBLOW,
     1                  LFIX,IPCTR) 
C     THIS SUBROUTINE INITIALIZES VARIABLES AND MATCHES PAL PART
C     NUMBER WITH ITYPE
      IMPLICIT INTEGER (A-Z)
      LOGICAL LBLANK,LLEFT,LAND,LOR,LSLASH,LEQUAL,LRIGHT,LXOR,LXNOR,
     1        LFIX,LFUSES(32,64),LPHANT(32,64)
      CHARACTER H,L,C,R,X,A,IOT,INOAI,INOO
     1         ,I0,I2,I4,I6,I8
      CHARACTER TSTVEC(20,50),IPAGE(80,200)
      COMMON  LBLANK,LLEFT,LAND,LOR,LSLASH,LEQUAL,LRIGHT,LXOR,LXNOR
      COMMON /PGE/ IPAGE
      COMMON  /TSTVEC/ NTEST, TSTVEC
      DATA H/'H'/,L/'L'/,C/'C'/,R/'R'/,X/'X'/,A/'A'/,
     1     I0/'0'/,I2/'2'/,I4/'4'/,I6/'6'/,I8/'8'/
C     INITIALIZE LFUSES ARRAY (FUSE ARRAY)
C
      DO 20 J=1,64
         DO 20 I=1,32
            LFUSES(I,J)=.FALSE.
   20       LPHANT(I,J)=.FALSE.
C        INITIALIZE NUMBER OF TEST VECTORS FOR JEDEC FORMAT
         NTEST = 0
C     INITIALIZE IBLOW (NUMBER OF FUSES BLOWN)
      IBLOW=0
C     INITIALIZE IPCTR (NUMBER OF PRODUCT TERMS)
      IPCTR=0
C     INITIALIZE IC AND IL (COLUMN AND LINE POINTERS)
      IC=0
      IL=1
C     INITIALIZE ITYPE (PAL PART TYPE)
      ITYPE=0
C     ITYPE IS ASSIGNED THE FOLLOWING VALUES FOR THESE PAL TYPES:
C          PAL10H8,PAL10L8                          ITYPE=1
C          PAL12H6,PAL12L6                          ITYPE=2
C          PAL14H4,PAL14L4                          ITYPE=3
C          PAL16H2,PAL16L2,PAL16C1                  ITYPE=4
C          PAL16L8                                  ITYPE=5
C          PAL16R4,PAL16R6,PAL16R8,PAL16X4,PAL16A4  ITYPE=6
C     DETERMINE ITYPE
      IF(  INOAI.EQ.I0 )                            ITYPE=1
      IF(  INOAI.EQ.I2 )                            ITYPE=2
      IF(  INOAI.EQ.I4 )                            ITYPE=3
      IF( (INOAI.EQ.I6) )                           ITYPE=4
      IF( (INOAI.EQ.I6).AND.(INOO.EQ.I8) )          ITYPE=5
      IF( (IOT.EQ.R).OR.(IOT.EQ.X).OR.(IOT.EQ.A) )  ITYPE=6
      IF( .NOT.(IOT.EQ.H.OR.IOT.EQ.L.OR.IOT.EQ.C
     1      .OR.IOT.EQ.R.OR.IOT.EQ.X.OR.IOT.EQ.A) ) ITYPE=0
      CALL INCR(IC,IL,LFIX)
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE GETSYM(LPHASE,ISYM,J,IC,IL,LFIX)
C     THIS SUBROUTINE GETS THE PIN NAME, / IF COMPLEMENT LOGIC, AND
C      THE FOLLOWING OPERATION SYMBOL IF ANY
      IMPLICIT INTEGER (A-Z)
      LOGICAL LBLANK,LLEFT,LAND,LOR,LSLASH,LEQUAL,LRIGHT,LXOR,LXNOR,
     1        LFIX,LPHASE(20)
      CHARACTER IPAGE(80,200),ISYM(8,20),IBLANK
      COMMON  LBLANK,LLEFT,LAND,LOR,LSLASH,LEQUAL,LRIGHT,LXOR,LXNOR
      COMMON /PGE/ IPAGE
      DATA IBLANK/' '/
      LFIX=.FALSE.
      IF( .NOT.(LLEFT.OR.LAND.OR.LOR.OR.LEQUAL.OR.LRIGHT) ) GO TO 10
      CALL INCR(IC,IL,LFIX)
      IF(LLEFT) GO TO 60
   10 LPHASE(J)=( .NOT.LSLASH )
      IF(LPHASE(J)) GO TO 15
      CALL INCR(IC,IL,LFIX)
C
   15 DO 20 I=1,8
   20     ISYM(I,J)=IBLANK
   25 DO 30 I=1,7
   30     ISYM(I,J)=ISYM(I+1,J)
      ISYM(8,J)=IPAGE(IC,IL)
      CALL INCR(IC,IL,LFIX)
      IF( LLEFT.OR.LBLANK.OR.LAND.OR.LOR.OR.LRIGHT.OR.LEQUAL ) RETURN
      GO TO 25
   60 LFIX=.TRUE.
      RETURN
      END
C
C**********************************************************************
C
      SUBROUTINE INCR(IC,IL,LFIX)
C     THIS SUBROUTINE INCREMENTS COLUMN AND LINE POINTERS
C      BLANKS AND CHARACTERS AFTER ';' ARE IGNORED
      IMPLICIT INTEGER (A-Z)
      LOGICAL LBLANK,LLEFT,LAND,LOR,LSLASH,LEQUAL,LRIGHT,LXOR,LXNOR,
     1        LFIX,LX1
      CHARACTER IBLANK,ILEFT,IAND,IOR,COMENT
     1         ,ISLASH,IEQUAL,IRIGHT,ICOLON,TAB
      CHARACTER IPAGE(80,200)
      COMMON  LBLANK,LLEFT,LAND,LOR,LSLASH,LEQUAL,LRIGHT,LXOR,LXNOR
      COMMON /PGE/ IPAGE
      DATA IBLANK/' '/,ILEFT/'('/,IAND/'*'/,IOR/'+'/,COMENT/';'/,
     1     ISLASH/'/'/,IEQUAL/'='/,IRIGHT/')'/,ICOLON/':'/,TAB/'  '/
      LBLANK=.FALSE.
      LXOR=.FALSE.
      LXNOR=.FALSE.
      LX1=.FALSE.
      LRIGHT=.FALSE.
   10 IC=IC+1
      IF(IC .LE.79 .AND. IPAGE(IC,IL).NE.COMENT) GO TO 30
      IL=IL+1
      IF(IL.LE.200) GO TO 20
          WRITE(*,15)
   15     FORMAT(/,' SOURCE FILE EXCEEDS 200 LINES OR MISSING',
     1             ' DESCRIPTION OR FUNCTION TABLE KEY WORD')
          CALL EXIT1
   20 IC=0
      GO TO 10
   30 IF( IPAGE(IC,IL).EQ.ICOLON.AND.(LFIX) ) RETURN
      IF(( IPAGE(IC,IL).NE.IBLANK ).AND.( IPAGE(IC,IL).NE.TAB)) GOTO 31
          LBLANK=.TRUE.
          GO TO 10
   31 IF( IPAGE(IC,IL).NE.ICOLON ) GO TO 32
      IF( (LXOR).OR.(LXNOR) )  GO TO 33
      LX1=.TRUE.
      GO TO 10
   33 IF(LXOR)  LOR=.TRUE.
      IF(LXNOR) LAND=.TRUE.
      RETURN
   32 IF( .NOT.(LX1.AND.(IPAGE(IC,IL).EQ.IOR.OR.IPAGE(IC,IL).EQ.IAND)) )
     1     GO TO 34
C
      IF( IPAGE(IC,IL).EQ.IOR  ) LXOR=.TRUE.
      IF( IPAGE(IC,IL).EQ.IAND ) LXNOR=.TRUE.
      GO TO 10
   34 LLEFT =( IPAGE(IC,IL).EQ.ILEFT  )
      LAND  =( IPAGE(IC,IL).EQ.IAND   )
      LOR   =( IPAGE(IC,IL).EQ. IOR   )
      LSLASH=( IPAGE(IC,IL).EQ.ISLASH )
      LEQUAL=( IPAGE(IC,IL).EQ.IEQUAL )
      LRIGHT=( IPAGE(IC,IL).EQ.IRIGHT )
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE MATCH(IMATCH,IBUF,ISYM)
C     THIS SUBROUTINE FINDS A MATCH BETWEEN THE PIN NAME IN THE EQUATION
C      AND THE PIN NAME IN THE PIN LIST OR FUNCTION TABLE PIN LIST
      IMPLICIT INTEGER (A-Z)
      LOGICAL LMATCH
      CHARACTER C,A,R,Y,IBUF(8,20),ISYM(8,20)
      DATA C/'C'/,A/'A'/,R/'R'/,Y/'Y'/
      IMATCH=0
      DO 20 J=1,20
          LMATCH=.TRUE.
          DO 10 I=1,8
   10         LMATCH=LMATCH.AND.(IBUF(I,1).EQ.ISYM(I,J))
          IF(LMATCH) IMATCH=J
   20     CONTINUE
C     MATCH CARRY WHICH IS FOUND IN THE PAL16A4
      IF( IBUF(3,1).EQ.C.AND.IBUF(4,1).EQ.A.AND.IBUF(5,1).EQ.R.AND.
     1    IBUF(6,1).EQ.R.AND.IBUF(7,1).EQ.Y ) IMATCH=99
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE IXLATE(IINPUT,IMATCH,LPHASE,LBUF,ITYPE)
C     THIS SUBROUTINE FINDS A MATCH BETWEEN THE INPUT PIN NUMBER AND
C      THE INPUT LINE NUMBER FOR A SPECIFIC PAL.  ADD 1 TO THE INPUT
C      LINE NUMBER IF THE PIN IS A COMPLEMENT
      IMPLICIT INTEGER (A-Z)
      INTEGER ITABLE(20,6)
      LOGICAL LPHASE(20),LBUF(20)
      DATA    ITABLE/
     1   3, 1, 5, 9,13,17,21,25,29,-10,31,-1,-1,-1,-1,-1,-1,-1,-1,-20,
     2   3, 1, 5, 9,13,17,21,25,29,-10,31,27,-1,-1,-1,-1,-1,-1, 7,-20,
     3   3, 1, 5, 9,13,17,21,25,29,-10,31,27,23,-1,-1,-1,-1,11, 7,-20,
     4   3, 1, 5, 9,13,17,21,25,29,-10,31,27,23,19,-1,-1,15,11, 7,-20,
     5   3, 1, 5, 9,13,17,21,25,29,-10,31,-1,27,23,19,15,11, 7,-1,-20,
     6  -1, 1, 5, 9,13,17,21,25,29,-10,-1,31,27,23,19,15,11, 7, 3,-20/
      IINPUT=0
      IBUBL=0
      IF(((     LPHASE(IMATCH)).AND.(.NOT.LBUF(1))).OR.
     1   ((.NOT.LPHASE(IMATCH)).AND.(     LBUF(1)))) IBUBL=1
      IF( ITABLE(IMATCH,ITYPE).GT.0 ) IINPUT=ITABLE(IMATCH,ITYPE)+IBUBL
C
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE FIXSYM(LBUF,IBUF,IC,IL,LFIRST,LFUSES,IBLOW,IPROD,LFIX)
C     THIS SUBROUTINE EVALUATES THE FIXED SYMBOLS FOUND IN THE
C      PAL16X4 AND PAL16A4
      IMPLICIT INTEGER (A-Z)
      LOGICAL LBUF(20),LFUSES(32,64),LFIRST,LMATCH,LFIX
      CHARACTER A,B,ISLASH,IOR,IBLANK,IRIGHT
     1         ,IAND,N,Q,N0,N1,N2,N3,CI
     2         ,ICOLON
     3         ,TABLE(5,14),IBUF(8,20),FIXBUF(8)
      CHARACTER IPAGE(80,200)
      COMMON /PGE/ IPAGE
      DATA A/'A'/,B/'B'/,ISLASH/'/'/,IOR/'+'/,IBLANK/' '/,IRIGHT/')'/,
     1        IAND/'*'/,N/'N'/,Q/'Q'/,N0/'0'/,N1/'1'/,N2/'2'/,N3/'3'/,
     2      ICOLON/':'/,
     3        TABLE      /      ' ','A','+','/','B',' ',' ','A','+','B',
     4      ' ',' ',' ',' ','A','/','A','+','/','B',' ',' ',' ','/','B',
     5      'A',':','+',':','B',' ','A','*','/','B',' ','/','A','+','B',
     6      'A',':','*',':','B',' ',' ',' ',' ','B',' ',' ','A','*','B',
     7      ' ',' ',' ','/','A','/','A','*','/','B',' ','/','A','*','B'/
      IINPUT=0
      DO 20 I=1,8
          IBUF(I,1)=IBLANK
   20     FIXBUF(I)=IBLANK
   21 CALL INCR(IC,IL,LFIX)
      CI=IPAGE(IC,IL)
      IF(CI.EQ.IRIGHT) GO TO 40
      IF(CI.EQ.N0) IINPUT=8
      IF(CI.EQ.N1) IINPUT=12
      IF(CI.EQ.N2) IINPUT=16
      IF(CI.EQ.N3) IINPUT=20
      DO 24 J=1,7
   24     IBUF(J,1)=IBUF(J+1,1)
      IBUF(8,1)=CI
      IF(.NOT. ( (CI.EQ.A).OR.(CI.EQ.B).OR.(CI.EQ.ISLASH).OR.(CI.EQ.IOR)
     1       .OR.(CI.EQ.IAND).OR.(CI.EQ.ICOLON) ) )  GO TO 21
      DO 30 I=1,4
   30     FIXBUF(I)=FIXBUF(I+1)
      FIXBUF(5)=IPAGE(IC,IL)
      GO TO 21
   40 IMATCH=0
      DO 60 J=1,14
          LMATCH=.TRUE.
          DO 50 I=1,5
   50         LMATCH=LMATCH .AND. ( FIXBUF(I).EQ.TABLE(I,J) )
   60     IF(LMATCH) IMATCH=J
      IF(IMATCH.EQ.0) GO TO 100
      IF(.NOT.LFIRST) GO TO 85
          LFIRST=.FALSE.
          DO 80 I=1,32
              LFUSES(I,IPROD)=.TRUE.
   80         IBLOW = IBLOW + 1
C
   85 DO 90 I=1,4
          IF( (IMATCH-7).LE.0 ) GO TO 90
          ISUM1=IINPUT+I
          LFUSES(ISUM1,IPROD)=.FALSE.
          IBLOW = IBLOW - 1
          IMATCH=IMATCH-8
   90 IMATCH=IMATCH+IMATCH
      LBUF(1)=.TRUE.
      CALL PLOT(LBUF,IBUF,LFUSES,IPROD,TITLE,.FALSE.,ITYPE,
     1          LPROD,IOP,IBLOW,IPCTR)
  100 LFIX=.FALSE.
      CALL INCR(IC,IL,LFIX)
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE ECHO(IPAL,INOAI,IOT,INOO,REST,PATNUM,TITLE,COMP)
C     THIS SUBROUTINE PRINTS THE PAL DESIGN SPECIFICATION INPUT FILE
      IMPLICIT INTEGER (A-Z)
      INTEGER IPAL(4),IOT,INOAI,INOO
     1,         PATNUM(80),TITLE(80),COMP(80),REST(73)
      CHARACTER IPAGE(80,200),IBLANK
      COMMON /PGE/ IPAGE
      COMMON /FTEST/ IFUNCT,IDESC,IEND
      DATA IBLANK/' '/
      WRITE(*,5) IPAL,INOAI,IOT,INOO,REST,PATNUM,TITLE,COMP
    5 FORMAT(/,' ',4A1,A1,A1,A1,73A1,/,' ',80A1,/,' ',80A1,/,' ',80A1)
      DO 20 IL=1,IEND
           IC=81
   10      IC=IC-1
           IF( IPAGE(IC,IL).EQ.IBLANK.AND.IC.GT.1 ) GO TO 10
           WRITE(*,15) (IPAGE(I,IL),I=1,IC)
   15      FORMAT(' ',80A1)
   20 CONTINUE
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE CAT
C     THIS SUBROUTINE PRINTS THE PALASM CATOLOG
      IMPLICIT INTEGER (A-Z)
      WRITE(*,10)
   10 FORMAT(/,'  MONOLITHIC MEMORIES 20-PIN PALASM VERSION 1.6C')
      WRITE(*,15)
   15 FORMAT(' (C) COPYRIGHT 1983 MONOLITHIC MEMORIES')
      WRITE(*,20)
   20 FORMAT(/,'    ECHO (E)     - PRINTS THE PAL DESIGN', 
     4         ' SPECIFICATION',
     5      /,'    PINOUT (O)   - PRINTS THE PINOUT OF THE PAL',
     6      /,'    SIMULATE (T) - EXERCISES THE FUNCTION TABLE',
     7         ' VECTORS IN THE LOGIC',/,'                  ',
     8         ' EQUATIONS AND GENERATES TEST VECTORS',
     9      /,'    PLOT (P)     - PRINTS THE ENTIRE FUSE PLOT')
C
      WRITE(*,30)
   30 FORMAT('    BRIEF (B)    - PRINTS ONLY THE USED PRODUCT LINES',
     1         ' OF THE FUSE PLOT',/,'                   PHANTOM',
     2         ' FUSES ARE OMITTED',
     3      /,'    SHORT (S)    - GENERATES SHORT VERSION'
     3                          ' OF HEX PROGRAMMING FORMAT',
     4      /,'    HEX (H)      - GENERATES HEX PROGRAMMING FORMAT',
     5      /,'    BHLF (L)     - GENERATES BHLF PROGRAMMING FORMAT',
     6      /,'    BNPF (N)     - GENERATES BNPF PROGRAMMING FORMAT',
     7      /,'    CATALOG (C)  - PRINTS THE PALASM CATALOG',
     8      /,'    QUIT (Q)     - EXIT1 PALASM',
     A      /,'    FAULT (F)    - FAULT TESTING ',
     9      /,'    JEDEC (J)    - JEDEC FORMAT FOR DATA I/O PROGRAMMER',
     B      /,'    INTEL (I)    - INTEL HEX PROGRAMMING FORMAT')
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE PINOUT(IPAL,INOAI,IOT,INOO,TITLE)
C     THIS SUBROUTINE PRINTS THE PINOUT OF THE PAL
      IMPLICIT INTEGER (A-Z)
      CHARACTER IBLANK,ISTAR,IPAL(4),IIN(7,2),IOT,INOAI
     1         ,INOO,PIN(12,20)
     2         ,TITLE(80)
      CHARACTER IPAGE(80,200)
      COMMON /PGE/ IPAGE
      DATA IBLANK/' '/,ISTAR/'*'/
      DO 10 J=1,20
          DO 5 I=1,12
    5         PIN(I,J)=IBLANK
   10 CONTINUE
   15 DO 25 J=1,2
          DO 20 I=1,7
   20         IIN(I,J)=IBLANK
   25 CONTINUE
      IIN(2,1)=IPAL(1)
      IIN(4,1)=IPAL(2)
      IIN(6,1)=IPAL(3)
      IIN(1,2)=IPAL(4)
      IIN(3,2)=INOAI
      IIN(5,2)=IOT
      IIN(7,2)=INOO
      J=0
      IL=0
   30 IC=0
      IL=IL+1
   35 IC=IC+1
   40 IF( IC.GT.80 ) GO TO 30
      IF( IPAGE(IC,IL).EQ.IBLANK ) GO TO 35
      J=J+1
      IF(J.GT.20) GO TO 60
      DO 55 I=1,12
          PIN(I,J)=IPAGE(IC,IL)
          IC=IC+1
          IF( IC.GT.80 ) GO TO 40
          IF( IPAGE(IC,IL).EQ.IBLANK ) GO TO 40
C
   55 CONTINUE
   60 DO 75 J=1,10
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
      WRITE(*,76) TITLE
   76 FORMAT(/,' ',80A1)
      WRITE(*,78) ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,
     1              ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,
     2              ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,
     3              ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR
   78 FORMAT(/,' ',18X,14A1,3X,14A1,
     1       /,' ',18X,A1,13X,A1,1X,A1,13X,A1)
      JJ=20
      DO 88 J=1,10
          WRITE(*,80) ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR
   80     FORMAT(' ',15X,4A1,29X,4A1)
          WRITE(*,81) (PIN(I,J),I=1,12),ISTAR,J,ISTAR,
     1         (IIN(I,1),I=1,7),ISTAR,JJ,ISTAR,(PIN(I,JJ),I=1,12)
   81     FORMAT(' ',12A1,3X,A1,I2,A1,11X,7A1,11X,A1,I2,A1,3X,12A1)
          WRITE(*,82) ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR
   82     FORMAT(' ',15X,4A1,29X,4A1)
          WRITE(*,84) ISTAR,(IIN(I,2),I=1,7),ISTAR
   84     FORMAT(' ',18X,A1,11X,7A1,11X,A1)
          DO 86 II=1,2
              DO 85 I=1,7
   85             IIN(I,II)=IBLANK
   86     CONTINUE
          JJ=JJ-1
   88 CONTINUE
      WRITE(*,90) ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,
     1              ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,
     2              ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,
     3              ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR
   90 FORMAT(' ',18X,31A1)
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE PLOT(LBUF,IBUF,LFUSES,IPROD,TITLE,LDUMP,ITYPE,
     1                LPROD,IOP,IBLOW,IPCTR0)
C     THIS THIS SUBROUTINE PRODUCES THE FUSE PLOT
      IMPLICIT INTEGER (A-Z)
      CHARACTER ISAVE(64,32),IAND,IOR,ISLASH,IBUF(8,20),IOUT(64),IOP
     1        ,IDASH,X,IBLANK,P,B
     2        ,HIFANT
C
     3        ,TITLE(80)
      LOGICAL LBUF(20),LFUSES(32,64),LDUMP,LPROD(80)
      DATA ISAVE/2048*' '/,IAND/'*'/,IOR/'+'/,ISLASH/'/'/,
     1     IDASH/'-'/,X/'X'/,IBLANK/' '/,P/'P'/,B/'B'/,
     2     HIFANT/'O'/
      IF(LDUMP) GO TO 60
      IF(ISAVE(IPROD,1).NE.IBLANK) RETURN
      IF( LBUF(1) ) GO TO 5
      DO 30 J=1,31
   30     ISAVE(IPROD,J)=ISAVE(IPROD,J+1)
      ISAVE(IPROD,32)=ISLASH
    5 DO 20 I=1,8
         IF( ISAVE(IPROD,1).NE.IBLANK ) RETURN
          IF( IBUF(I,1).EQ.IBLANK ) GO TO 20
          DO 10 J=1,31
   10         ISAVE(IPROD,J)=ISAVE(IPROD,J+1)
          ISAVE(IPROD,32)=IBUF(I,1)
   20     CONTINUE
      IF(ISAVE(IPROD,1).NE.IBLANK) RETURN
   40 DO 50 J=1,31
   50     ISAVE(IPROD,J)=ISAVE(IPROD,J+1)
      ISAVE(IPROD,32)=IAND
      RETURN
C     PRINT FUSE PLOT
   60 WRITE(*,62) TITLE
   62 FORMAT(/,' ',80A1,//,
     1 '                11 1111 1111 2222 2222 2233',/,
     2 '    0123 4567 8901 2345 6789 0123 4567 8901',/)
      DO 100 I88PRO=1,57,8
          DO 94 I8PRO=1,8
              IPROD=I88PRO+I8PRO-1
              ISAVE(IPROD,32)=IBLANK
              DO 70 I=1,32
                  IF( ISAVE(IPROD,1).NE.IBLANK ) GO TO 70
                  DO 65 J=1,31
                      ISAVE(IPROD,J)=ISAVE(IPROD,J+1)
   65                 CONTINUE
                  ISAVE(IPROD,32)=IBLANK
   70         CONTINUE
              DO 80 I=1,32
                  IOUT(I)=X
                  IF( LFUSES(I,IPROD) ) IOUT(I)=IDASH
                  IOUT(I+32)=ISAVE(IPROD,I)
   80         CONTINUE
              IF(ITYPE.LE.4) CALL FANTOM(ITYPE,IOUT,IPROD,I8PRO)
              IPROD=IPROD-1
              DO 85 J=1,32
                  IF( IOP.EQ.B.AND.IOUT(J).EQ.HIFANT ) IOUT(J)=IBLANK
  85          CONTINUE
              IF( (IOP.EQ.P).OR.(IOP.EQ.B.AND.(LPROD(IPROD+1))) )
     1        WRITE(*,90) IPROD,IOUT
   90         FORMAT(' ',I2,8(' ',4A1),' ',32A1)
   94         CONTINUE
          WRITE(*,96)
C
   96     FORMAT(1X)
  100     CONTINUE
      WRITE(*,110)
  110 FORMAT(/,
     1' LEGEND:  X : FUSE NOT BLOWN (L,N,0)   - : FUSE BLOWN   (H,P,1)')
      IF( IOP.EQ.P.AND.ITYPE.LE.4 ) WRITE(*,111)
  111 FORMAT(
     1'          0 : PHANTOM FUSE   (L,N,0)   O : PHANTOM FUSE (H,P,1)')
      WRITE(*,112) IBLOW
  112 FORMAT(/,' NUMBER OF FUSES BLOWN = ',I4)
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE HEX(LFUSES,IOP)
C     THIS SUBROUTINE GENERATES HEX PROGRAMMING FORMATS
      IMPLICIT INTEGER (A-Z)
      LOGICAL LFUSES(32,64)
      INTEGER SOH,STX,ETX,BEL
      CHARACTER ZTABLE(16),H,S,IBLANK,IOP,ITEMP(64),ZCSUM(4)
      DATA H/'H'/,S/'S'/,IBLANK/' '/,
     1    ZTABLE/'0','1','2','3','4','5','6','7',
     2           '8','9','A','B','C','D','E','F'/
      DATA SOH/001/,STX/002/,ETX/003/,BEL/007/
      CSUM=0
      IF(IOP.EQ.H) WRITE(*,10)
   10 FORMAT(//,'                                         .',//)
C***** NOTE: SOME PROM PROGRAMMERS NEED A START CHARACTER.
C*****       THIS PROGRAM OUTPUTS AN STX FOR THE DATA I/O MODEL 9
C*****         (USE SOH FOR MODEL 5)
      WRITE(*,5) BEL,BEL,BEL,BEL,BEL,BEL,BEL,STX,SOH
    5 FORMAT(' ',9A1)
      DO 40 I=1,33,32
      INC=I-1
        DO 40 IPROD=1,7,2
          DO 20 J=1,2
              DO 20 IINPUT=1,32
              IHEX=0
              ISUM2=IPROD + J-1 + INC
              IF(LFUSES(IINPUT,ISUM2 +  0 )) IHEX=IHEX+1
              IF(LFUSES(IINPUT,ISUM2 +  8 )) IHEX=IHEX+2
              IF(LFUSES(IINPUT,ISUM2 + 16 )) IHEX=IHEX+4
              IF(LFUSES(IINPUT,ISUM2 + 24 )) IHEX=IHEX+8
              CSUM=CSUM+IHEX
              ISUMX=IINPUT+(32*(J-1))
   20         ITEMP(ISUMX)=ZTABLE(IHEX+1)
          IF(IOP.EQ.H) WRITE(*,30) ITEMP
   30     FORMAT(' ',32(A1,' '),'.',/,' ',32(A1,' '),'.')
   40     IF(IOP.EQ.S) WRITE(*,50) ITEMP
   50     FORMAT(' ',64A1)
      IF(IOP.EQ.H) WRITE(*,70)
C
   70 FORMAT(' ',//,'                                       .',//)
      WRITE(*,80) ETX
   80 FORMAT(' ',A1)
C     CONVERT DECIMAL CHECK SUM INTO HEX CHECK SUM
      DO 85 I=1,4
           ZTEMP=CSUM-16*(CSUM/16)
           ZCSUM(5-I)=ZTABLE(ZTEMP+1)
           CSUM=CSUM/16
   85 CONTINUE
      IF(ZCSUM(1).EQ.ZTABLE(1)) ZCSUM(1)=IBLANK
      WRITE(*,90) ZCSUM(1),ZCSUM(2),ZCSUM(3),ZCSUM(4)
   90 FORMAT(/' HEX CHECK SUM = ',4A1)
      RETURN
      END
C
C***********************************************************************
C
C
      SUBROUTINE TWEEK(ITYPE,IOT,LFUSES,LPHANT)
C     THIS SUBROUTINE TWEEKS LFUSES (THE PROGRAMMING FUSE PLOT)
C      FOR HIGH AND LOW PHANTOM FUSES
      IMPLICIT INTEGER (A-Z)
      LOGICAL LFUSES(32,64),LPHANT(32,64)
      CHARACTER L,C,IOT
      DATA L/'L'/,C/'C'/
      IF(ITYPE.GE.4) GO TO 20
      DO 10 IPROD=1,64
          LFUSES(15,IPROD)=.TRUE.
          LFUSES(16,IPROD)=.TRUE.
          LFUSES(19,IPROD)=.TRUE.
          LFUSES(20,IPROD)=.TRUE.
          LPHANT(15,IPROD)=.TRUE.
          LPHANT(16,IPROD)=.TRUE.
          LPHANT(19,IPROD)=.TRUE.
          LPHANT(20,IPROD)=.TRUE.
          IF(ITYPE.GE.3) GO TO 10
          LFUSES(11,IPROD)=.TRUE.
          LFUSES(12,IPROD)=.TRUE.
          LFUSES(23,IPROD)=.TRUE.
          LFUSES(24,IPROD)=.TRUE.
          LPHANT(11,IPROD)=.TRUE.
          LPHANT(12,IPROD)=.TRUE.
          LPHANT(23,IPROD)=.TRUE.
          LPHANT(24,IPROD)=.TRUE.
          IF(ITYPE.GE.2) GO TO 10
          LFUSES( 7,IPROD)=.TRUE.
          LFUSES( 8,IPROD)=.TRUE.
          LFUSES(27,IPROD)=.TRUE.
          LFUSES(28,IPROD)=.TRUE.
          LPHANT( 7,IPROD)=.TRUE.
          LPHANT( 8,IPROD)=.TRUE.
          LPHANT(27,IPROD)=.TRUE.
          LPHANT(28,IPROD)=.TRUE.
   10     CONTINUE
      DO 18 IINPUT=7,28
C
          DO 12 IPROD=1,57,8
              LFUSES(IINPUT,IPROD+4)=.FALSE.
              LFUSES(IINPUT,IPROD+5)=.FALSE.
              LFUSES(IINPUT,IPROD+6)=.FALSE.
              LFUSES(IINPUT,IPROD+7)=.FALSE.
              LPHANT(IINPUT,IPROD+4)=.TRUE.
              LPHANT(IINPUT,IPROD+5)=.TRUE.
              LPHANT(IINPUT,IPROD+6)=.TRUE.
   12         LPHANT(IINPUT,IPROD+7)=.TRUE.
          IF(ITYPE.GE.3) GO TO 18
          DO 14 IPROD=17,41,8
              LFUSES(IINPUT,IPROD+2)=.FALSE.
              LFUSES(IINPUT,IPROD+3)=.FALSE.
              LPHANT(IINPUT,IPROD+2)=.TRUE.
   14         LPHANT(IINPUT,IPROD+3)=.TRUE.
          IF(ITYPE.GE.2) GO TO 18
          DO 16 IPROD=1,57,8
              LFUSES(IINPUT,IPROD+2)=.FALSE.
              LFUSES(IINPUT,IPROD+3)=.FALSE.
              LPHANT(IINPUT,IPROD+2)=.TRUE.
   16         LPHANT(IINPUT,IPROD+3)=.TRUE.
   18 CONTINUE
   20 IF( ITYPE.EQ.1 ) RETURN
      DO 99 IINPUT=1,32
          DO 30 IPROD=1,8
              LFUSES(IINPUT,IPROD+ 0)= (IOT.NE.L)
              LPHANT(IINPUT,IPROD+ 0)= .TRUE.
              IF(IOT.EQ.C) GO TO 30
              LFUSES(IINPUT,IPROD+56)= (IOT.NE.L)
              LPHANT(IINPUT,IPROD+56)= .TRUE.
   30         CONTINUE
          IF(ITYPE.LE.2) GO TO 99
          DO 40 IPROD=1,8
              LFUSES(IINPUT,IPROD+ 8)= (IOT.NE.L)
              LPHANT(IINPUT,IPROD+ 8)= .TRUE.
              IF(IOT.EQ.C) GO TO 40
              LFUSES(IINPUT,IPROD+48)= (IOT.NE.L)
              LPHANT(IINPUT,IPROD+48)= .TRUE.
   40         CONTINUE
          IF(ITYPE.LE.3) GO TO 99
          DO 50 IPROD=1,8
              LFUSES(IINPUT,IPROD+16)= (IOT.NE.L)
              LPHANT(IINPUT,IPROD+16)= .TRUE.
              IF(IOT.EQ.C) GO TO 50
              LFUSES(IINPUT,IPROD+40)= (IOT.NE.L)
              LPHANT(IINPUT,IPROD+40)= .TRUE.
   50         CONTINUE
   99     CONTINUE
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE BINR(LFUSES,H,L)
C     THIS SUBROUTINE GENERATES BINARY PROGRAMMING FORMATS
C
      IMPLICIT INTEGER (A-Z)
      INTEGER ITEMP(4,8),H,L
      LOGICAL LFUSES(32,64)
      WRITE(*,10)
   10 FORMAT(//,'                                         .',//)
      DO 20 I=1,33,32
      INC=I-1
        DO 20 IPROD=1,8
            DO 20 J=1,25,8
              DO 15 K=1,8
                IINPUT=J+K-1
                ITEMP(1,K)=L
                ITEMP(2,K)=L
                ITEMP(3,K)=L
                ITEMP(4,K)=L
                ISUM3=IPROD+INC
                IF(LFUSES(IINPUT,ISUM3 +  0 )) ITEMP(4,K)=H
                IF(LFUSES(IINPUT,ISUM3 +  8 )) ITEMP(3,K)=H
                IF(LFUSES(IINPUT,ISUM3 + 16 )) ITEMP(2,K)=H
                IF(LFUSES(IINPUT,ISUM3 + 24 )) ITEMP(1,K)=H
   15           CONTINUE
   20         WRITE(*,30) ITEMP
   30         FORMAT(' ',8('b',4A1,'f '))
      WRITE(*,10)
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE SLIP(LFUSES,I88PRO,INOAI,IOT,INOO,IBLOW)
C     THIS SUBROUTINE WILL BLOW THE ENTIRE CONDITIONAL THREE-STATE
C      PRODUCT LINE WHEN 'IF(VCC)' CONDITION IS USED FOR THE
C      CORRESPONDING OUTPUT PIN
      IMPLICIT INTEGER (A-Z)
      LOGICAL LFUSES(32,64)
      CHARACTER R,I1,I2,I4,I6,I8,IOT,INOAI,INOO
      DATA R/'R'/,I1/'1'/,I2/'2'/,I4/'4'/,I6/'6'/,I8/'8'/
      IF( (INOAI.NE.I6) .OR. (INOO.EQ.I1) .OR.  (INOO.EQ.I2) .OR.
     1    ( (IOT.EQ.R).AND.(INOO.EQ.I8) ) .OR.
     2    ( (I88PRO.GE. 9).AND.(I88PRO.LE.49).AND.(INOO.EQ.I6) ) .OR.
     3    ( (I88PRO.GE.17).AND.(I88PRO.LE.41).AND.(INOO.EQ.I4)) ) RETURN
      DO 10 I=1,32
      IBLOW = IBLOW + 1
   10 LFUSES(I,I88PRO) = .TRUE.
      I88PRO = I88PRO + 1
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE FANTOM(ITYPE,IOUT,IPROD,I8PRO)
C     THIS SUBROUTINE UPDATES IOUT (THE PRINTED FUSE PLOT)
C      FOR HIGH AND LOW PHANTOM FUSES
      IMPLICIT INTEGER (A-Z)
C
      CHARACTER X,IDASH,LOFANT,HIFANT, IOUT(64)
      DATA X/'X'/,IDASH/'-'/,LOFANT/'0'/,HIFANT/'O'/
      DO 10 I=1,32
          IF( IOUT(I).EQ.IDASH ) IOUT(I)=HIFANT
          IF( IOUT(I).EQ.X )     IOUT(I)=LOFANT
   10 CONTINUE
      IF((ITYPE.EQ.4).AND.((IPROD.LE.24).OR.(IPROD.GE.41))) RETURN
      IF((ITYPE.EQ.3).AND.((IPROD.LE.16).OR.(IPROD.GE.45))) RETURN
      IF((ITYPE.EQ.2).AND.((IPROD.LE. 8).OR.(IPROD.GE.53))) RETURN
      IF((ITYPE.LE.3).AND.(I8PRO.GE.5)) RETURN
      IF((ITYPE.LE.2).AND.(IPROD.GE.19).AND.(IPROD.LE.48).AND.
     1   (I8PRO.GE.3)) RETURN
      IF((ITYPE.EQ.1).AND.(I8PRO.GE.3)) RETURN
      DO 50 I=1,32
        IF(((I.EQ.15).OR.(I.EQ.16).OR.(I.EQ.19).OR.(I.EQ.20)).AND.
     1   (ITYPE.LE.3)) GO TO 50
        IF(((I.EQ.11).OR.(I.EQ.12).OR.(I.EQ.23).OR.(I.EQ.24)).AND.
     1   (ITYPE.LE.2)) GO TO 50
        IF(((I.EQ. 7).OR.(I.EQ. 8).OR.(I.EQ.27).OR.(I.EQ.28)).AND.
     1   (ITYPE.LE.1)) GO TO 50
        IF( IOUT(I).EQ.HIFANT ) IOUT(I)=IDASH
        IF( IOUT(I).EQ.LOFANT ) IOUT(I)=X
   50 CONTINUE
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE IODC2
C***** THIS ROUTINE IS OPTIONAL, IT MAY BE USED TO TURN PERIPHERALS ON
      IMPLICIT INTEGER (A-Z)
      INTEGER BEL,DC2
      DATA BEL/007/,DC2/022/
      WRITE(*,10) DC2,BEL
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
      WRITE(*,10) BEL,DC3,DC4
   10 FORMAT(' ',3A1)
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE TEST(LPHASE,LBUF,TITLE,IC,IL,ILE,ISYM,IBUF,ITYPE,INOO,
C
     1                LFIX,IPCTR,LERR,ISAF,IPCTR1,LSA11,LSA01,LPRINT)
C     THIS SUBROUTINE PERFORMS THE FUNCTION TABLE SIMULATION
C      AND GENERATES TEST VECTORS
      IMPLICIT INTEGER (A-Z)
      INTEGER IPIN(20),IPCTR,NTEST
      LOGICAL LBLANK,LLEFT,LAND,LOR,LSLASH,LEQUAL,LRIGHT,LXOR,LXNOR,
     1        LFIX,LSAME,XORFND,LERR,LPHASE(20),LPHAS1(20),LBUF(20),
     2        LOUT(20),LOUTP(20),LCLOCK,LPTRST,LCTRST,LENABL(20),NREG,
     3        LSA11,LSA12,LSA01,LSA02,LPRINT                
      CHARACTER IVECT(20),ISTATT(20),IPAGE(80,200),ISUM,IPROD
      CHARACTER TSTVEC(20,50),IDASH,L,H,X,C,Z,N0,ISTATE(20)
     2         ,N1,ERR,IBLANK,COMENT,I4,I6,I8,NN,IBUF(8,20)
     3         ,INOO,TITLE(80),IVECTP(20),XORSUM,ITEST,ISYM(8,20)
     4         ,ISYM1(8,20)      
      COMMON  LBLANK,LLEFT,LAND,LOR,LSLASH,LEQUAL,LRIGHT,LXOR,LXNOR
      COMMON /PGE/ IPAGE
      COMMON /FTEST/ IFUNCT,IDESC,IEND
      COMMON /TSTVEC/ NTEST,TSTVEC
      DATA IDASH/'-'/,L/'L'/,H/'H'/,X/'X'/,C/'C'/,Z/'Z'/,N0/'0'/,
     1     N1/'1'/,ERR/'?'/,IBLANK/' '/,COMENT/';'/,I4/'4'/,I6/'6'/,
     2     I8/'8'/,NN/'N'/
C
         NTEST = 0
C     PRINT AN ERROR MESSAGE IF NO FUNCTION TABLE IS SUPPLIED
      IF(IFUNCT.NE.0) GO TO 3
      WRITE(*,2)
    2 FORMAT(/,' FUNCTION TABLE MUST BE SUPPLIED IN ORDER TO PERFORM',
     1         ' SIMULATION')
      RETURN
C     PRINT TITLE
    3 IF((.NOT.LSA11).AND.(.NOT.LSA01).AND.LPRINT)
     1 WRITE(*,4) TITLE
    4 FORMAT(/,' ',80A1,/)
C     INITIALIZE LERR (FUNCTION TABLE ERROR FLAG) TO NO ERROR
      LERR=.FALSE.
C     INITIALIZE NERR (NUMBER OF FUNCTION TABLE ERRORS) TO NO ERRORS
      NERR=0
C     INITIALIZE ITRST (THREE-STATE ENABLE FUNCTION TABLE PIN NUMBER)
      ITRST=0
C     SET THE STARTING POINT OF THE FUNCTION TABLE TO COLUMN 0
C     AND IFUNCT + 1
      IC=0
      IL=IFUNCT + 1
C     INITIALIZE SA1/SA0 PARAMETERS
      IPCTR3=0
      IEQN=0
      IPCTR=0
C      
C     MAKE A DUMMY CALL TO INCR
      CALL INCR(IC,IL,LFIX)
C     GET THE FUNCTION TABLE PIN LIST (UP TO 18)
C      GO ONE MORE THAN MAX TO LOOK FOR DASHED LINE
C
      DO 10 I=1,19
      CALL GETSYM(LPHAS1,ISYM1,I,IC,IL,LFIX)
         DO 5 J=1,8
    5    IBUF(J,1)=ISYM1(J,I)
      IF(IBUF(8,1).EQ.IDASH) GO TO 12
      CALL MATCH(IMATCH,IBUF,ISYM)
      IF(IMATCH.NE.0) GO TO 7 
      WRITE(*,6) (IBUF(J,1),J=1,8)
    6 FORMAT(/,' FUNCTION TABLE PIN LIST ERROR AT', 8A1) 
      RETURN
    7 LOUT(I)=.FALSE.
      ISTATT(I)=X
      IVECTP(I)=X
C     IF APPROPIATE PAL TYPE, REMEMBER LOCATION OF CLOCK AND THREE-STATE    
C      ENABLE PIN IN FUNCTION TABLE PIN LIST
      IF(ITYPE.NE.6) GO TO 10
      IF(IMATCH.EQ.1)  ICLOCK=I
      IF(IMATCH.EQ.11) ITRST=I
   10 IPIN(I)=IMATCH 
C     ALL SIGNAL NAMES FOR THE FUNCTIONAL TEST HAVE BEEN READ IN
C      ADJUST COUNT
   12 IMAX=I-1
      NVECT=0
C
C*****START OF MAIN LOOP FOR SIMULATION*****
C
C     * SA1/SA0  TESTS
C     INITIALLY THERE ARE NO FAULTS. IPCTR2 IS THE POINTER FOR
C     TOTAL NUMBER OF PRODUCT TERMS. IEQN IS EQUATION COUNT.
C     IPCTR3 IS THE PRODUCT TERM POINTER IN A PARTICULAR EQN.
   90 IPCTR2=0
      IEQN=0
      IPCTR3=0
      LSA12=.FALSE.
      LSA02=.FALSE.
C     *************END OF ADDITION**********************
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
C
      IC=1
      IF(IVECT(1).EQ.IDASH) GO TO 95
C     CHECK FOR VALID FUNCTION TABLE VALUES (L,H,X,Z,C)
      DO 11 I=1,IMAX
         IF( IVECT(I).EQ.L.OR.IVECT(I).EQ.H.OR.IVECT(I).EQ.X.OR.
     1       IVECT(I).EQ.Z.OR.IVECT(I).EQ.C) GO TO 11
         WRITE(*,8) IVECT(I),NVECT
    8    FORMAT(/,' ',A1,' IS NOT AN ALLOWED FUNCTION TABLE ENTRY',
     1                   ' IN VECTOR ',I4)
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
C     INITIALIZE ISTATE ARRAY TO ALL X'S
      DO 15 I=1,20
      ISTATE(I)=X
   15 CONTINUE
C     CHECK IF THIS PAL TYPE HAS REGISTERS
      IF(ITYPE.NE.6) GO TO 25
C     CHECK CLOCK AND THREE-STATE ENABLE PINS AND CHANGE FLAG IF NEEDED
      IF(IVECT(ICLOCK).EQ.C) LCLOCK=.TRUE.
      IF(ITRST.EQ.0) GO TO 25
      LSAME=( (     LPHASE(11)).AND.(     LPHAS1(ITRST)).OR.
     1        (.NOT.LPHASE(11)).AND.(.NOT.LPHAS1(ITRST)) )
      IF( IVECT(ITRST).EQ.L.AND.(.NOT.LSAME).OR.
     1    IVECT(ITRST).EQ.H.AND.(     LSAME) ) LPTRST=.FALSE.
      IF(LPTRST) GO TO 25
C     DISABLE REGISTERED OUTPUTS IF APPROPRIATE
      DO 46 I=1,IMAX
         J=IPIN(I)
         IF(J.EQ.14.OR.J.EQ.15.OR.J.EQ.16.OR.J.EQ.17) LENABL(I)=.FALSE.
         IF( INOO.EQ.I6.AND.(J.EQ.13.OR.J.EQ.18) )    LENABL(I)=.FALSE.
         IF( INOO.EQ.I8.AND.(J.EQ.12.OR.J.EQ.13
     1                   .OR.J.EQ.18.OR.J.EQ.19) )    LENABL(I)=.FALSE.
   46 CONTINUE
C
C*****SCAN THROUGH THE LOGIC EQUATIONS*****
C
C     MAKE A DUMMY CALL TO INCR
   25 CALL INCR(IC1,IL1,LFIX)
   26 CALL GETSYM(LBUF,IBUF,1,IC1,IL1,LFIX)
      IF(LLEFT) GO TO 29
   27 IF(.NOT.LEQUAL) GO TO 26
C *************ADDED FOR EQN CONT**********
      IF(LEQUAL) IEQN=IEQN+1
C *****************************************  
C     EVALUATE CONDITIONAL THREE-STATE PRODUCT LINE
   29 IF(LEQUAL) GO TO 35
      NREG=.TRUE.
   33 CALL GETSYM(LBUF,IBUF,1,IC1,IL1,LFIX)
C
      CALL MATCH(IINP,IBUF,ISYM1)
C     CHECK FOR GND, VCC, /GND, OR /VCC IN CONDITIONAL THREE-STATE
C      PRODUCT LINE
      IF(IINP.NE.0) GO TO 32
      CALL MATCH(IMATCH,IBUF,ISYM)
      ILL=IL1
      IF( IINP.EQ.0.AND.IMATCH.NE.10.AND.IMATCH.NE.20 ) GO TO 100
      IF( IMATCH.EQ.10.AND.(LBUF(1)).OR.
     1    IMATCH.EQ.20.AND.(.NOT.LBUF(1)) ) LCTRST=.FALSE.
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
C     FIND THE PIN NUMBER OF THE OUTPUT VECTORS
C     *ADDTION FOR SA0/SA1 TEST
   35  IPCTR3=0
C     *END OF ADDITION
      CALL MATCH(IOUTP,IBUF,ISYM1)
C     FLAG UNREGISTERED OUTPUTS
      CALL MATCH(IOUT,IBUF,ISYM)
      IF(ITYPE.LE.5) NREG=.TRUE.
      IF( (INOO.EQ.I4.OR.INOO.EQ.I6).AND.(IOUT.EQ.12.OR.IOUT.EQ.19) )
     1     NREG=.TRUE.
      IF( (INOO.EQ.I4).AND.(IOUT.EQ.13.OR.IOUT.EQ.18) ) NREG=.TRUE.
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
C     *********THE FOLLOWING IS THE ADDITION FOR SA1/SA0  TESTS***
   28 IPCTR2=IPCTR2+1
      IPCTR3=IPCTR3+1
C     *********END OF ADDITION*********
      IPCTR=IPCTR+1
      IPROD=H
   30 ILL=IL1
      CALL GETSYM(LBUF,IBUF,1,IC1,IL1,LFIX)
      IF( .NOT.LFIX ) GO TO 39
C     EVALUATE THE FIXED SYMBOLS FOUND IN THE PAL16X4 AND PAL16A4
C
          LFIX=.FALSE.
          CALL FIXTST(LPHAS1,LBUF,IC1,IL1,ISYM,ISYM1,IBUF,
     1                IVECT,IVECTP,ITEST,LCLOCK,NREG,LFIX)
          IF(IPROD.EQ.H) IPROD=ITEST
          GO TO 38
   39 CALL MATCH(IINP,IBUF,ISYM1)
      IF(IINP.NE.0) GO TO 47
      CALL MATCH(IMATCH,IBUF,ISYM)
      IF(IMATCH.NE.10.AND.IMATCH.NE.20) GO TO 100
C     TWEEK FOR GND AND VCC IN PRODUCT LINE
      IF(IMATCH.EQ.10) ITEST=L
      IF(IMATCH.EQ.20) ITEST=H
      IINP=19
      LPHAS1(19)=.TRUE.
      GO TO 37
   47 ITEST=IVECT(IINP)
C     GET REGISTERED FEED BACK VALUES
      IF(NREG) GO TO 37
      CALL MATCH(IIFB,IBUF,ISYM)
      IF( (INOO.EQ.I4.OR.INOO.EQ.I6.OR.INOO.EQ.I8).AND.
     1    (IIFB.EQ.14.OR.IIFB.EQ.15.OR.IIFB.EQ.16.OR.IIFB.EQ.17) )
     2     ITEST=IVECTP(IINP)
      IF( (INOO.EQ.I6.OR.INOO.EQ.I8).AND.(IIFB.EQ.13.OR.IIFB.EQ.18) )
     1     ITEST=IVECTP(IINP)
      IF(  INOO.EQ.I8.AND.(IIFB.EQ.12.OR.IIFB.EQ.19) )
     1     ITEST=IVECTP(IINP)
   37 IF( ITEST.EQ.X.OR.ITEST.EQ.Z ) ITEST=L
      IF(  ITEST.EQ.L.AND.(     LPHAS1(IINP)).AND.(     LBUF(1)) 
     1.OR. ITEST.EQ.H.AND.(     LPHAS1(IINP)).AND.(.NOT.LBUF(1))
     2.OR. ITEST.EQ.H.AND.(.NOT.LPHAS1(IINP)).AND.(     LBUF(1))
     3.OR. ITEST.EQ.L.AND.(.NOT.LPHAS1(IINP)).AND.(.NOT.LBUF(1)) 
     4  )  IPROD=L
C     *THE FOLLOWING ADDITION IS FOR SA1 TEST
C     CHECK FOR A PARTICULAR PRODUCT TERM AND GO FOR SA1 TEST
      IF((IPCTR2.EQ.IPCTR1).AND.(LSA11))GO TO 110
C     *END OF ADDITION
   38 IF(LRIGHT) CALL INCR(IC1,IL1,LFIX)
      IF(LAND) GO TO 30
C     *SA0 ADDITION
C     CHECK FOR A PARTICULAR PRODUCT TERM AND GO FOR SA0 TEST
      IF((IPCTR2.EQ.IPCTR1).AND.(LSA01))GO TO 120
C     *END OF ADDITION 
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
C
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
      IF( .NOT.LOUT(I) ) GO TO 50
      IF( ISTATT(I).EQ.X.AND.IVECT(I).EQ.X ) GO TO 50
      LSAME = ( (     LOUTP(I)).AND.(     LPHAS1(I)).OR.
     1          (.NOT.LOUTP(I)).AND.(.NOT.LPHAS1(I)) )
      IMESS=40
      IF(ISTATT(I).EQ.L.AND.IVECT(I).EQ.L.AND.(.NOT.LSAME)) IMESS=41
      IF(ISTATT(I).EQ.H.AND.IVECT(I).EQ.H.AND.(.NOT.LSAME)) IMESS=42
      IF(ISTATT(I).EQ.L.AND.IVECT(I).EQ.H.AND.(     LSAME)) IMESS=42
      IF(ISTATT(I).EQ.H.AND.IVECT(I).EQ.L.AND.(     LSAME)) IMESS=41
      IF( (     LENABL(I)).AND.IVECT(I).EQ.Z )              IMESS=43
      IF( (.NOT.LENABL(I)).AND.(LOUT(I)).AND.IVECT(I).NE.Z) IMESS=44
      IF(IMESS.NE.40) LERR=.TRUE.
C     *THIS IS AN ADDITION FOR SA1/SA0  TESTS
C     IF NO FAULT GO FOR NEXT VECTOR ELSE GET OUT OF SIMULATION AND
C     START SIMULATION FOR THE NEXT PRODUCT TERM. 
      IF((.NOT.LERR).AND.((LSA11).OR.(LSA01))) GO TO 50
      IF((LERR).AND.((LSA11).OR.(LSA01))) GO TO 115
C     ***************************************************************
      IF(IMESS.EQ.41) WRITE(*,41) NVECT,(ISYM1(J,I),J=1,8)
   41 FORMAT(/,' FUNCTION TABLE ERROR IN VECTOR',I4,'  PIN =',8A1,
     1         '  EXPECT = H  ACTUAL = L')
      IF(IMESS.EQ.42) WRITE(*,42) NVECT,(ISYM1(J,I),J=1,8)
   42 FORMAT(/,' FUNCTION TABLE ERROR IN VECTOR',I4,'  PIN =',8A1,
     1         '  EXPECT = L  ACTUAL = H')
      IF(IMESS.EQ.43) WRITE(*,43) NVECT,(ISYM1(J,I),J=1,8)
   43 FORMAT(/,' FUNCTION TABLE ERROR IN VECTOR',I4,'  PIN =',8A1, 
     1       /,'  EXPECT  = OUTPUT ENABLE  ACTUAL = Z')
      IF(IMESS.EQ.44) WRITE(*,44) NVECT,(ISYM1(J,I),J=1,8),IVECT(I)
   44 FORMAT(/,' FUNCTION TABLE ERROR IN VECTOR',I4,'  PIN =',8A1,
     1         '  EXPECT = Z  ACTUAL = ',A1)
      IF(IMESS.NE.40) IVECT(I)=ERR
      IF(IMESS.NE.40) NERR=NERR+1
   50 CONTINUE
C
C     CHANGE THE ORDER OF VECTORS FROM THE ORDER OF APPEARANCE IN THE
C      FUNCTION TABLE TO THAT OF THE PIN LIST AND TWEEK FOR OUTPUT
      DO 65 I=1,20
         DO 55 J=1,IMAX
         IF(IPIN(J).NE.I) GO TO 55
         IF( IVECT(J).EQ.L.OR.IVECT(J).EQ.H ) GO TO 51
         ISTATE(I)=IVECT(J)
         GO TO 65
   51    LSAME=( (     LPHASE(I)).AND.(     LPHAS1(J)).OR.    
     1           (.NOT.LPHASE(I)).AND.(.NOT.LPHAS1(J)) )
         IF( INOO.EQ.N1.AND.(I.EQ.15.OR.I.EQ.16) )  LOUT(J)=.TRUE.
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
C     SAVE PRESENT VECTORS FOR FEED BACK USED WITH NEXT SET OF VECTORS
C      IF CLOCK PULSE AND NOT Z (HI-Z IS ASYNCHRONOUS)
   65 IF( (LCLOCK).AND.IVECT(J).NE.Z ) IVECTP(J)=IVECT(J)
C     ASSIGN X TO GROUND PIN AND 1 TO VCC PIN
      ISTATE(10)=X
      ISTATE(20)=N1
C     PRINT TEST VECTORS
      IF((.NOT.LSA11).AND.(.NOT.LSA01).AND.LPRINT)
     1 WRITE(*,60) NVECT,(ISTATE(I),I=1,20)
   60 FORMAT(' ',I4,' ',20A1)
C
C         GENERATE TEST VECTORS FOR
C         JEDEC FORMAT OUTPUT
C
         IF (NVECT.EQ.51) WRITE(*,1000)
 1000    FORMAT(' WARNING: MORE THAN 50 TEST VECTORS.  JEDEC DATA',/
     1         ,' WILL CONTAIN ONLY THE FIRST 50 VECTORS.')
         IF (NVECT.GT.50) GO TO 90
         NTEST = NTEST + 1
         DO 1010 I = 1,20
            IF (ISTATE(I).EQ.L) TSTVEC(I,NTEST) = L
            IF (ISTATE(I).EQ.H) TSTVEC(I,NTEST) = H
            IF (ISTATE(I).EQ.X) TSTVEC(I,NTEST) = X
            IF (ISTATE(I).EQ.C) TSTVEC(I,NTEST) = C
            IF (ISTATE(I).EQ.Z) TSTVEC(I,NTEST) = Z
C
            IF (ISTATE(I).EQ.N0) TSTVEC(I,NTEST) =N0
            IF (ISTATE(I).EQ.N1) TSTVEC(I,NTEST) =N1
            TSTVEC(10,NTEST) = NN
            TSTVEC(20,NTEST) = NN
            IF (TSTVEC(11,NTEST).EQ.X) TSTVEC(11,NTEST) = N0
 1010    CONTINUE
C
C                  END OF ADDITIONS 
C
      GO TO 90
C     TERMINATE SIMULATION
C     *ADITION FOR SA0/SA1 TESTS
   95 IF((.NOT.LERR).AND.(LSA11).AND.LPRINT)
     1 WRITE(*,150) IPCTR4,IEQN1
  150 FORMAT(' ',' PRODUCT: ',I3,' OF ','EQUATION.',I3,'
     1 UNTESTED(SA1)FAULT')
      IF((.NOT.LERR).AND.(LSA01).AND.LPRINT)
     1 WRITE(*,155) IPCTR4,IEQN1
  155 FORMAT(' ',' PRODUCT: ',I3,' OF ','EQUATION.',I3,'
     1 UNTESTED(SA0)FAULT')  
C     *END OF ADDITION
      IF((.NOT.LERR).AND.((.NOT.LSA11).AND.(.NOT.LSA01)).AND.LPRINT)
     1 WRITE(*,67)
   67 FORMAT(/,' PASS SIMULATION')
      IPCTR=IPCTR/(NVECT-1)
      IF((     LERR).AND.((.NOT.LSA11).AND.(.NOT.LSA01)).AND.LPRINT)
     1 WRITE(*,68) NERR
   68 FORMAT(/,' NUMBER OF FUNCTION TABLE ERRORS =',I4)
      RETURN
C     PRINT AN ERROR MESSAGE FOR AN UNDEFINED PIN NAME
  100 ILERR=ILL+4
      WRITE(*,101) (IBUF(I,1),I=1,8),ILERR,(IPAGE(I,ILL),I=1,80)
  101 FORMAT(/,' ERROR SYMBOL = ',8A1,'      IN LINE NUMBER ',I4,
     1       /,' ',80A1,/,' THIS PIN NAME IS NOT DEFINED IN THE',
     2                    ' FUNCTION TABLE PIN LIST')
      RETURN
C     *THIS IS AN ADDITION FOR SA1 TEST
C     THE PRODUCT TERM IS PULLED HIGH AND THE PRODUCT NUMBER
C     AND EQN NUMBER IS REMEMBERED
  110 IPROD=H
      LSA12=.TRUE.
      IEQN1=IEQN
      IPCTR4=IPCTR3
      GO TO 38
C     *END OF ADDITION
C
C     *SA0 ADDITION
C     THE PRODUCT TERM IS TESTED FOR SA0 FAULT AND ALSO REMEMBERED
  120 IPROD=L
      LSA02=.TRUE.
      IEQN1=IEQN
      IPCTR4=IPCTR3
      GO TO 121
C     *END OF ADDITION
C
C     *ADDITION FOR SA1/SA0  TESTS
C     IF NO FAULT THEN NEXT PRODUCT TERM
  115 ISAF=ISAF+1
C
      LERR=.FALSE.
      RETURN
C     *END OF ADDITION
      END
C
C***********************************************************************
C
      SUBROUTINE FIXTST(LPHAS1,LBUF,IC1,IL1,ISYM,ISYM1,IBUF,
     1                  IVECT,IVECTP,ITEST,LCLOCK,NREG,LFIX)
C     THIS SUBROUTINE EVALUATES THE FIXED SYMBOLS FOUND IN THE
C      PAL16X4 AND PAL16A4 FOR THE FUNCTION TABLE
      IMPLICIT INTEGER (A-Z)
      LOGICAL LBLANK,LLEFT,LAND,LOR,LSLASH,LEQUAL,LRIGHT,LXOR,LXNOR,
     1        LFIX,LPHAS1(20),LBUF(20),LCLOCK,NREG,TOR,TXOR,TXNOR,TAND,
     2        LPHASA,LPHASB
      CHARACTER L,H,X,Z,ISYM(8,20),ISYM1(8,20),IBUF(8,20)
     1         ,IVECT(20),IVECTP(20),ITEST,ITESTA,ITESTB
      CHARACTER IPAGE(80,200)
      COMMON  LBLANK,LLEFT,LAND,LOR,LSLASH,LEQUAL,LRIGHT,LXOR,LXNOR
      COMMON /PGE/ IPAGE
      DATA L/'L'/,H/'H'/,X/'X'/,Z/'Z'/
C     GET OUTPUT PIN AN (WHERE N=0,1,2,3)
      CALL GETSYM(LBUF,IBUF,1,IC1,IL1,LFIX)
      CALL MATCH(IINP,IBUF,ISYM1)
      ITESTA=IVECT(IINP)
      LPHASA = ( (     LBUF(1)).AND.(     LPHAS1(IINP)).OR.
     1           (.NOT.LBUF(1)).AND.(.NOT.LPHAS1(IINP)) )
C     GET REGISTERED FEED BACK VALUES
      IF(NREG) GO TO 5
      CALL MATCH(IIFB,IBUF,ISYM)
      IF( IIFB.EQ.14.OR.IIFB.EQ.15.OR.IIFB.EQ.16.OR.IIFB.EQ.17 )
     1    ITESTA=IVECTP(IINP)
    5 IF( (.NOT.LPHASA).AND.ITESTA.EQ.L ) GO TO 10
      IF( (.NOT.LPHASA).AND.ITESTA.EQ.H ) GO TO 15
      GO TO 20
   10 ITESTA=H
      GO TO 20
   15 ITESTA=L
   20 IF( .NOT.LRIGHT ) GO TO 25
           ITEST=ITESTA
           RETURN
C     SAVE THE FIXED SYMBOL OPERATORS
   25 TOR   = (LOR.AND.(.NOT.LXOR))
      TXOR  = (LXOR)
      TXNOR = (LXNOR)
      TAND  = (LAND.AND.(.NOT.LXNOR))
C     GET INPUT BN (WHERE N=0,1,2,3)
      CALL GETSYM(LBUF,IBUF,1,IC1,IL1,LFIX)
      CALL MATCH(IINP,IBUF,ISYM1)
      ITESTB=IVECT(IINP)
      LPHASB = ( (     LBUF(1)).AND.(     LPHAS1(IINP)).OR.
     1           (.NOT.LBUF(1)).AND.(.NOT.LPHAS1(IINP)) )
      IF( (.NOT.LPHASB).AND.ITESTB.EQ.L ) GO TO 30
      IF( (.NOT.LPHASB).AND.ITESTB.EQ.H ) GO TO 35
      GO TO 40
   30 ITESTB=H
C
      GO TO 40
   35 ITESTB=L
C     EVALUATE THE FIXED SYMBOL EXPRESSION
   40 ITEST=L
      IF(   (TOR).AND.(ITESTA.EQ.H.OR. ITESTB.EQ.H) )      ITEST=H
      IF(  (TXOR).AND.((ITESTA.EQ.H.AND.ITESTB.NE.H).OR.
     1                 (ITESTA.NE.H.AND.ITESTB.EQ.H) ))    ITEST=H
      IF( (TXNOR).AND.((ITESTA.EQ.ITESTB).OR.
     1                 (ITESTA.EQ.X.OR.ITESTB.EQ.X) ))     ITEST=H
      IF(  (TAND).AND.(ITESTA.NE.L.AND.ITESTB.NE.L) )      ITEST=H
      IF( (ITESTA.EQ.X.OR.ITESTA.EQ.Z).AND.(ITESTB.EQ.X) ) ITEST=X
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE EXIT1
      STOP
      END
C
C  ************THIS SUBROUTINE IS ADDED FOR JEDEC FORMAT*********
C  THE FOLLOWING SUBROUTINE GIVES JEDEC FORMATTED OUTPUT FOR 
C  PROGRAMMING COMPATIBILITY WITH DATA I/O PROGRAMMERS
      SUBROUTINE PLOTF(ITYPE,IOT)
      IMPLICIT INTEGER (A-Z)
      LOGICAL  LFUSES (32,64),LPHANT(32,64)
      CHARACTER ZERO,ONE,IOT,STAR,L,H,X,C,Z,N0,N1,NN,IPBUF(32)
      CHARACTER TSTVEC(20,50),JC  
      INTEGER ISUM(4),IADR,STX,ETX,IDEC(4),IPT,IINP,J1,J2,J3,PINOUT
      INTEGER IDECIO(4),ISUMIO(4),BUFIO(32),NFUSE,NTEST
      COMMON  /IPT/ IPT
      COMMON  /LFUSES/ LFUSES,LPHANT
      COMMON  /SUM/ ISUM,IDEC,IPBUF,BUFIO,NFUSE
      COMMON  /TSTVEC/ NTEST, TSTVEC
      DATA    ZERO/'0'/, ONE/'1'/, STAR/'*'/,L/'L'/,H/'H'/,X/'X'/
     1        C/'C'/,Z/'Z'/,N0/'0'/,N1/'1'/,NN/'N'/
      IADR=0
      STX=2
      ETX=3
      J3 =0
C
C           ADDITIONS  TO GENERATE PINOUT AND
C           FAMILY CODE.
C
      IF (ITYPE.NE.6) GOTO 24
      PINOUT = 24
      GOTO 30
   24 IF(IOT.NE.L) GOTO 28
      PINOUT = ITYPE + 12
      GOTO 30
   28 PINOUT = ITYPE + 17
      IF ((ITYPE.EQ.4).AND.(IOT.EQ.H)) PINOUT = PINOUT + 1
   30 ISUM(4)=536 + PINOUT/10 + MOD(PINOUT,10)
      ISUM(2)=MOD(ISUM(4)/256,256)
      ISUM(4)=MOD(ISUM(4),256)
      WRITE(*,10) STX,PINOUT
   10 FORMAT(' ',A1,'*D22',I2,'*F0*')
C
C           
      DO 300 IPT=1,64
      IF(LPHANT(9,IPT)) GO TO 300
      NFUSE = 0
      DO 50  IINP=1,32
      IF(LPHANT(IINP,IPT)) GO TO 50
      NFUSE = NFUSE + 1
      IF(LFUSES(IINP,IPT)) IPBUF(NFUSE)=ONE
      IF(.NOT.(LFUSES(IINP,IPT))) IPBUF(NFUSE)=ZERO
   50 CONTINUE
      IF(LFUSES(1,IPT)) GO TO 100
      IF(.NOT.LFUSES(2,IPT)) GO TO 250
  100 IDEC(4)=IADR
      DO 150 J=1,3
      J1=5-J
      J2=4-J
      IDEC(J2)=IDEC(J1)/10
      IDEC(J1)=IDEC(J1)-10*IDEC(J2)
      IDECIO(J1)=ICONV(IDEC(J1))
  150 CONTINUE
      IDECIO(1)=ICONV(IDEC(1))
      CALL SUMCHK
      WRITE(*,201)IDECIO,(IPBUF(I),I=1,NFUSE),STAR
  201 FORMAT(' L',4A1,' ',32A1,A1)
  250 IADR=IADR+NFUSE
  300 CONTINUE
C
C            GENERATE TEST VECTORS.
C
            IF (NTEST.LE.0) GOTO 380
               IF (NTEST.GT.50) NTEST = 50
               DO 370 J = 1,NTEST
               ISUM(4) = ISUM(4) + 215
               ISUM(2) = MOD(ISUM(2)+ISUM(4)/256,256)
               ISUM(4) = MOD(ISUM(4),256)
               DO 350 I = 1,20
               JC = (TSTVEC(I,J))
               IF (JC.EQ.L) ISUM(4) = ISUM(4) + 76
               IF (JC.EQ.H) ISUM(4) = ISUM(4) + 72
               IF (JC.EQ.C) ISUM(4) = ISUM(4) + 67
               IF (JC.EQ.X) ISUM(4) = ISUM(4) + 88
               IF (JC.EQ.Z) ISUM(4) = ISUM(4) + 90
               IF (JC.EQ.NN) ISUM(4) = ISUM(4) + 78
               IF (JC.EQ.N1) ISUM(4) = ISUM(4) + 49
               IF (JC.EQ.N0) ISUM(4) = ISUM(4) + 48
               ISUM(2) = MOD(ISUM(2)+ISUM(4)/256,256)
               ISUM(4) = MOD(ISUM(4),256)
  350          CONTINUE
               DO 360 I = 1,4
               IDECIO(I) = MOD(J/10**(4-I),10)
               ISUM(4) = ISUM(4) + IDECIO(I) + 48
               ISUM(2) = MOD(ISUM(2)+ISUM(4)/256,256)
               ISUM(4) = MOD(ISUM(4),256)
  360          CONTINUE
               WRITE(*,410) IDECIO,(TSTVEC(I,J),I=1,20)
  410          FORMAT(' V',4I1,1X,20A1,' *')
  370          CONTINUE
C
C           ENDIF       
C
  380     ISUMIO(1)=ICONV(ISUM(2)/16)
C
      ISUM(2)=MOD(ISUM(2),16)
      ISUMIO(2)=ICONV(ISUM(2))
      ISUMIO(3)=ICONV(ISUM(4)/16)
      ISUM(4)=MOD(ISUM(4),16)
      ISUMIO(4)=ICONV(ISUM(4))
      WRITE(*,400) ETX,ISUMIO
  400 FORMAT(' ',A1,4A1,/)
      RETURN
      END
C
C*******************************************************
C
C THIS  SUBROUTINE CALCULATES THE SUMCHECK
      SUBROUTINE SUMCHK
      IMPLICIT INTEGER (A-Z)
      LOGICAL LFUSES(32,64),LPHANT(32,64)
      INTEGER BUFIO(32),NFUSE
      INTEGER ISUM(4),IDEC(4)
      CHARACTER IPBUF(32)
      COMMON /IPT/ IPT
      COMMON /LFUSES/ LFUSES,LPHANT
      COMMON/SUM/ ISUM,IDEC,IPBUF,BUFIO,NFUSE
      DO 50 J=1,32
      IF(LPHANT(J,IPT)) GO TO 50
      IF(LFUSES(J,IPT)) BUFIO(J)=49
      IF(.NOT.LFUSES(J,IPT)) BUFIO(J)=48
      ISUM(4)=ISUM(4)+BUFIO(J)
      ISUM(2)=MOD(ISUM(2)+ISUM(4)/256,256)
      ISUM(4)=MOD(ISUM(4),256)
   50 CONTINUE
      DO 100 J=1,4
      ISUM(4)=ISUM(4)+IDEC(J)+48
      ISUM(2)=MOD(ISUM(2)+ISUM(4)/256,256)
      ISUM(4)=MOD(ISUM(4),256)
  100 CONTINUE
      ISUM(4)=ISUM(4)+173
      ISUM(2)=MOD(ISUM(2)+ISUM(4)/256,256)
      ISUM(4)=MOD(ISUM(4),256)
      RETURN
      END
C
C****************************************************************
C
      SUBROUTINE INTEL(LFUSES,IOP)
C     THIS SUBROUTINE GENERATES INTEL HEX PROGRAMMING FORMATS
      IMPLICIT INTEGER (A-Z)
      LOGICAL LFUSES(32,64)
      CHARACTER   ZTABLE(16),IOP,ITEMP(32)
      DATA
     1    ZTABLE/'0','1','2','3','4','5','6','7',
     2           '8','9','A','B','C','D','E','F'/
      ADDR = 0
C
      DO 40 I=1,33,32
      INC=I-1
        DO 40 IPROD1=1,8
          CSUM = MOD(ADDR/256+MOD(ADDR,256)+32,256)
          DO 20 IINPUT=1,32
              IHEX=0
              ISUM2=IPROD1 + INC
              IF(LFUSES(IINPUT,ISUM2 +  0 )) IHEX=IHEX+1
              IF(LFUSES(IINPUT,ISUM2 +  8 )) IHEX=IHEX+2
              IF(LFUSES(IINPUT,ISUM2 + 16 )) IHEX=IHEX+4
              IF(LFUSES(IINPUT,ISUM2 + 24 )) IHEX=IHEX+8
              CSUM=MOD(CSUM+IHEX,256)
              ITEMP(IINPUT)=ZTABLE(IHEX+1)
   20     CONTINUE
          IF(CSUM.NE.0) CSUM=256-CSUM
          WRITE(*,60) ZTABLE(ADDR/4096+1),ZTABLE(MOD(ADDR/256,16)+1),
     1                  ZTABLE(MOD(ADDR/16,16)+1),
     2                  ZTABLE(MOD(ADDR,16)+1),ITEMP,
     3                  ZTABLE(CSUM/16+1),ZTABLE(MOD(CSUM,16)+1)
          ADDR = ADDR + 32
   40   CONTINUE
   60 FORMAT(1X,' :20',4A1,'00',32('0',A1),2A1)
      WRITE(*,70)
   70 FORMAT(1X,' :00000001FF')
      RETURN
      END
C                  **************END************
