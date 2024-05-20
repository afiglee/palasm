/*
palasm 20 pin:
C     ITYPE IS ASSIGNED THE FOLLOWING VALUES FOR THESE PAL TYPES:
C          PAL10H8,PAL10L8                          ITYPE=1
C          PAL12H6,PAL12L6                          ITYPE=2
C          PAL14H4,PAL14L4                          ITYPE=3
C          PAL16H2,PAL16L2,PAL16C1                  ITYPE=4
C          PAL16L8                                  ITYPE=5
C          PAL16R4,PAL16R6,PAL16R8,PAL16X4,PAL16A4  ITYPE=6

palasm 24 pin:
C     ITYPE IS ASSIGNED THE FOLLOWING VALUES FOR THESE PAL PART TYPES:
C     PAL12L10 =  1   PAL14L8  =  2   PAL16L6  =  3   PAL18L4  =  4
C     PAL20L2  =  5   PAL20C1  =  6   PAL20L10 =  7   PAL20X10 =  8
C     PAL20X8  =  9   PAL20X4  = 10   PAL20L8  = 11   PAL20R8  = 12
C     PAL20R6  = 13   PAL20R4  = 14
*/

/*
C                       SUBROUTINES: INITLZ,GETSYM,INCR,MATCH,FIXSYM,
C                                    IXLATE,ECHO,CAT,PINOUT,PLOT,TWEEK,
C                                    BINR,HEX,SLIP,FANTOM,IODC4,
C                                    TEST,FIXTST,PLOTF,INTEL
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
C*/

#include "palasm.h"
#include <string>
#include <iostream>
#include <array>
#include <vector>
#include <fstream>
#include <sstream>

using std::cout;
using std::string;
using std::endl;
using std::ifstream;
using std::vector;
using std::runtime_error;
using std::stringstream;
using std::exception;
using std::cerr;

#if 0
void readFile(const char *filename, vector<string>& content) {

      ifstream file(filename); 
  
      string line; 
  
      if (file.is_open()) { 
            while (getline(file, line)) { 
                  if (line.size() > 80) {
                        stringstream ss;
                        ss << "Warning: Line " << (content.size() + 1) << " is longer than 80 chars"; 
                        cerr << ss.str() << endl;
                  }
                  content.emplace_back(line); 
            } 
            file.close(); 
      } else { 
            stringstream ss;
            ss << "Unable to open file" << filename;   
        throw  runtime_error(ss.str()); 
    } 
}
#endif




#define ISUM_ADD(VAL)   _isum[3]+=(VAL); \
                        _isum[1] = (_isum[1] + (_isum[3]%256))%256;\
                        _isum[3] = _isum[1]%256;

Palasm::Palasm():
    _ipt(0), _lblank(false), _lleft(false), 
    _land(false), _lor(false),
    _lslash(false), _lequal(false), _lright(false),
    _lxor(false), _lxnor(false),
    _ifunct(0),_idesc(0),_iend(0)
{
    for (size_t col=0; col<80; col++) {
        for (size_t rr=0; rr < 200; rr++) {
            _ipage[col][rr] = 0;
        }
    }
    for (size_t col=0; col<32; col++) {
        for (size_t rr=0; rr < 64; rr++) {
            _lphant[col][rr] = _lfuses[col][rr] = false;
            
        }
        _bufio[col] = 0;
    }
    for (size_t tt = 0; tt < 4; tt++) {
        _idec[tt] = _isum[tt] = 0;
    }
}

void Palasm::tweek(int chipType) {
    if(chipType < 4) {
        for (size_t tt = 0; tt < 64; tt++) {
            _lfuses[14][tt]=true;
            _lfuses[15][tt]=true;
            _lfuses[18][tt]=true;
            _lfuses[19][tt]=true;
            _lphant[14][tt]=true;
            _lphant[15][tt]=true;
            _lphant[18][tt]=true;
            _lphant[19][tt]=true;
            if(chipType< 3) {
                _lfuses[10][tt]=true;
                _lfuses[11][tt]=true;
                _lfuses[22][tt]=true;
                _lfuses[23][tt]=true;
                _lphant[10][tt]=true;
                _lphant[11][tt]=true;
                _lphant[22][tt]=true;
                _lphant[23][tt]=true;
                if(chipType< 2){
                    _lfuses[6][tt]=true;
                    _lfuses[7][tt]=true;
                    _lfuses[26][tt]=true;
                    _lfuses[27][tt]=true;
                    _lphant[6][tt]=true;
                    _lphant[7][tt]=true;
                    _lphant[26][tt]=true;
                    _lphant[27][tt]=true;
                }
            }
        }
        for (size_t tt = 6; tt< 28; tt++) {
            for (size_t cc=0; cc<57; cc+=8) {
                _lfuses[tt][cc+4]=false;
                _lfuses[tt][cc+5]=false;
                _lfuses[tt][cc+6]=false;
                _lfuses[tt][cc+7]=false;
                _lphant[tt][cc+4]=true;
                _lphant[tt][cc+5]=true;
                _lphant[tt][cc+6]=true;
                _lphant[tt][cc+7]=true;
            }
            if(chipType < 3) {
                for (size_t cc = 16; cc < 41; cc+= 8) {
                    _lfuses[tt][cc+2]=false;
                    _lfuses[tt][cc+3]=false;
                    _lphant[tt][cc+2]=true;
                    _lphant[tt][cc+3]=true;
                }
                if(chipType <2 ) {
                    for (size_t cc = 0; cc < 57; cc+=8 ) {
                        _lfuses[tt][cc+2]=false;
                        _lfuses[tt][cc+3]=false;
                        _lphant[tt][cc+2]=true;
                        _lphant[tt][cc+3]=true;
                    }
                }
            }
        }
    }
    if( chipType == 1 ){
        return;
    }
    for (size_t rr = 0; rr < 32; rr++) {
        for (size_t tt=0; tt < 8; tt++) {
            _lfuses[rr][tt+ 0]= _iot != 'L';
            _lphant[rr][tt+ 0]= true;
            if (_iot != 'C') {
                _lfuses[rr][tt+56]= _iot != 'L';
                 _lphant[rr][tt+56]= true;
            }
        }
        if (chipType>2) {
            for (size_t tt = 0; tt < 8; tt++) {
                _lfuses[rr][tt+ 8]= _iot != 'L';
                _lphant[rr][tt+ 8]= true;
                if(_iot != 'C') {
                    _lfuses[rr][tt+48]= _iot != 'L';
                    _lphant[rr][tt+48]= true;
                }
            }
            if(chipType > 3) {
                for (size_t tt = 0; tt < 8; tt++) {
                    _lfuses[rr][tt+16]= _iot != 'L';
                    _lphant[rr][tt+16]= true;
                    if(_iot != 'C') {
                        _lfuses[rr][tt+40]= _iot != 'L';
                        _lphant[rr][tt+40]= true;
                    }
                }
            }
        }
    }
}

void Palasm::sumchk(){

      for (size_t tt = 0; tt < 32; tt++) {
        if (!(_lphant[tt][_ipt])){
            if(_lfuses[tt][_ipt]) {
                _bufio[tt]='1';
            }
            ISUM_ADD(_bufio[tt]);
        }
      }
    for (size_t tt = 0; tt < 4; tt++) {
      ISUM_ADD(_idec[tt] + 48);
    }
    ISUM_ADD(173);
}

void Palasm::plotf()
{

}

#if 0
int main(int argc, char *argv[]) {

      //IMPLICIT INTEGER (A-Z)
      bool LFIX{false},LFIRST{false},
            LPHASE[20],LBUF[20],
             LPROD[80],LSAME{false},LACT{false},
             LOPERR{false},LINP{false},LPRD{false},
             LERR{false},LSA11{false},LSA01{false};
             
      int   /*IPAL[4],*/IOT,REST[73],IOP,INOAI,INOO,
              E,O,T,P,B,H,S,L,N,C,Q,U,F,R,A,Y,X,IBUF[8][20],
              BB,CC,DD,EE,FF,II,NN,OO,PP,RR,SS,TT,UU,hhh,lll,ppp,nnn,
              ISYM[8][20],
              PATNUM[80],TITLE[80],COMP[80];
      const int BEL = 7;
      
      vector<string> content;

      //COMMON /LUNIT/ PMS,POF,PDF
      
     // DATA E/'E'/,O/'O'/,T/'T'/,P/'P'/,B/'B'/,H/'H'/,S/'S'/,L/'L'/,
     //1     N/'N'/,C/'C'/,Q/'Q'/,U/'U'/,F/'F'/,R/'R'/,A/'A'/,JJ
     //2     /'J'/,X/'X'/
     // DATA BB/'B'/,CC/'C'/,DD/'D'/,EE/'E'/,FF/'F'/,II/'I'/,NN/'N'/,
     //1     OO/'O'/,PP/'P'/,RR/'R'/,SS/'S'/,TT/'T'/,UU/'U'/
     // DATA BEL /007/
     /*
C
C
C     ASSIGNMENT OF DATA SET REFERENCES
C     RPD - PAL DESIGN SPECIFICATION (INPUT)
C     ROC - OPERATION CODE (INPUT)
C     POF - ECHO, PINOUT, TEST, AND PLOT (OUTPUT)
C     PDF - HEX AND BINARY FORMAT PROGRAM TAPES (OUTPUT)
C     PMS - PROMPTS AND ERROR MESSAGES (OUTPUT)*/

      cout << endl << " MONOLITHIC MEMORIES INC. PALASM VERSION 1.6C"
           << endl << " (C) COPYRIGHT 1983 MONOLITHIC MEMORIES')"
           << endl; // << " WHAT IS THE LOGICAL UNIT NUMBER FOR OUTPUT(6)?: '$)

     // READ(5,2) LUN
    /*  RPD=1
      ROC=5
      POF=LUN
      PDF=LUN
      PMS=LUN*/
      if (argc != 2) {
            cerr << "Filename required as an argument" << endl;
            exit(1);
      }
      try {
            readFile(argv[1], content);
      } catch (exception& ex) {
            cerr << ex.what() << endl;
            exit(1);
      }
      size_t linesCount = content.size();
      if (linesCount > 200) {
            stringstream ss;
            ss << "Warning, number of lines in the file "
               << argv[1]
               << " more then 200";
            cerr << ss.str() << endl;
      } else if (linesCount < 5) {
            stringstream ss;
            cerr << "Error: file is too short" << endl;
            exit(1);
      }
      
      const string& ident = content[0];
      if (ident.size() < 7) {
            cerr << "Error, first line must start with chip identification, minimum 7 chars" << endl;
            exit(1);
      }
      const string ipal{ident.substr(0,4)};//INT(4) IPAL
      const char inoai{ident[4]}, 
                 iot{ident[5]}, 
                 inoo{ident[6]},
                 rest{ident[7]};
      const string& patnum = content[1];
      const string& title = content[2];
      const string& comp = content[3];

      for (size_t seek = 4; seek < content.size(); seek++) {
            const string& line = content[seek];
            if (!IFUNCT && "FUNCTION" == line) {
                  IFUNCT = seek;
            } else if (!IDESC && "DESCRIPTION" == line) {
                  IDESC = seek;
            }
            if (IFUNCT && IDESC) {
                  break;
            }
      }

      IEND = content.size() - 1;
      
      int ntest = 0;
      int tstvec[20][50];

      int IBLOW=0, IPCTR=0, IC=0, IL=1;
      int chipType = 0;

      switch (inoai) {
            case '0':
                  chipType = 1; //PAL10H8,PAL10L8
                  break;
            case '2':
                  chipType = 2; //PAL12H6,PAL12L6
                  break;
            case '4':
                  chipType = 3; //PAL14H4,PAL14L4
                  break;
            case '6':
                  chipType = 4; //PAL16H2,PAL16L2,PAL16C1
                  if (inoo == '8') {
                        chipType = 5; //PAL16L8 
                  }
      }
      switch (iot) {//PAL16R4,PAL16R6,PAL16R8,PAL16X4,PAL16A4
            case 'R':
            case 'X':
            case 'A':
                  chipType = 6;
                  break;
            case 'H':
            case 'L':
            case 'C':
                  break;
            default:
                  chipType = 0;
      }
      if (!chipType) {
            stringstream ss;
            ss << "Unknown chip type: " << ipal << inoai 
               << iot;
            cerr << ss.str() << endl;
            exit(1);   
      }
      if (ipal[0] == 'H') {
            cerr << "HAL devices are not supported" << endl;
            exit(1);
      }

      incr(IC,IL,LFIX);

      int ILE=IL+1;

C     GET 20 PIN NAMES
   17 DO 20 J=1,20
   20     CALL GETSYM(LPHASE,ISYM,J,IC,IL,LFIX)
          IF(.NOT.(LEQUAL.OR.LLEFT.OR.LAND.OR.LOR.OR.LRIGHT)) GO TO 24
              WRITE(PMS,23)
   23         FORMAT(/,' LESS THAN 20 PIN NAMES IN PIN LIST')
              CALL EXIT
   24 ILE=IL

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
          IF( (chipType.EQ.1.OR.chipType.EQ.5.OR.chipType.EQ.6).AND.IOT.NE.A.
     1    AND.(IMATCH.LT.12.OR.IMATCH.GT.19) ) LOPERR=.TRUE.
          IF(  chipType.EQ.2.AND.(IMATCH.LT.13.OR.IMATCH.GT.18) )
     1                                         LOPERR=.TRUE.
          IF(  chipType.EQ.3.AND.(IMATCH.LT.14.OR.IMATCH.GT.17) )
     1                                         LOPERR=.TRUE.
          IF(  chipType.EQ.4.AND.(IMATCH.LT.15.OR.IMATCH.GT.16) )
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
              IF( (chipType.EQ.1.OR.chipType.EQ.2.AND.IPRD.GT.13
     1             .AND.IPRD.LT.18).AND.COUNT.GT.2 ) LPRD=.TRUE.
              IF( (chipType.EQ.3.OR.chipType.EQ.2.AND.(IPRD.EQ.13.OR.
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
                IF( chipType.EQ.1.AND.(IMATCH.GE.12.AND.IMATCH.LE.19) )
     1              LINP=.TRUE.
                IF( chipType.EQ.2.AND.(IMATCH.GE.13.AND.IMATCH.LE.18) )
     1              LINP=.TRUE.
                IF( chipType.EQ.3.AND.(IMATCH.GE.14.AND.IMATCH.LE.17) )
     1              LINP=.TRUE.
                IF( chipType.EQ.4.AND.(IMATCH.EQ.15.OR.IMATCH.EQ.16) )
     1              LINP=.TRUE.
                IF( chipType.EQ.5.AND.(IMATCH.EQ.12.OR.IMATCH.EQ.19) )
     1              LINP=.TRUE.
                IF( chipType.EQ.6.AND.(IMATCH.EQ.1.OR.IMATCH.EQ.11) )
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
   58           CALL IXLATE(IINPUT,IMATCH,LPHASE,LBUF,chipType)
                IF(IINPUT.LE.0) GO TO 60
                IBLOW = IBLOW - 1
                LFUSES(IINPUT,IPROD)=.FALSE.
                CALL PLOT(LBUF,IBUF,LFUSES,IPROD,TITLE,.FALSE.,chipType,
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
      WRITE(PMS,99) BEL
   99 FORMAT(' ',A1)
      WRITE(PMS,101) (IBUF(I,1),I=1,8),ILERR,(IPAGE(I,ILL),I=1,80)
  101 FORMAT(/,' ERROR SYMBOL = ',8A1,'      IN LINE NUMBER ',I4,
     1       /,' ',80A1)
C
C     PRINT AN ERROR MESSAGE FOR ACTIVE HIGH/LOW PART
      IF( (LACT).AND.(     LSAME).AND.(.NOT.LOPERR) )
     1         WRITE(PMS,103) IPAL,INOAI,IOT,INOO
  103 FORMAT(' OUTPUT MUST BE INVERTED SINCE ',4A1,A1,A1,A1,
     1       ' IS AN ACTIVE LOW DEVICE')
      IF( (LACT).AND.(.NOT.LSAME).AND.(.NOT.LOPERR) )
     1         WRITE(PMS,109) IPAL,INOAI,IOT,INOO
  109 FORMAT(' OUTPUT CANNOT BE INVERTED SINCE ',4A1,A1,A1,A1,
     1       ' IS AN ACTIVE HIGH DEVICE')
C     PRINT AN ERROR MESSAGE FOR AN INVALID OUTPUT PIN
      IF( (LOPERR).AND.IMATCH.NE.0 )
     1         WRITE(PMS,105) IMATCH,IPAL,INOAI,IOT,INOO
  105 FORMAT(' THIS PIN NUMBER ',I2,' IS AN INVALID OUTPUT PIN',
     1       ' FOR ',4A1,A1,A1,A1)
C     PRINT AN ERROR MESSAGE FOR AN INVALID INPUT PIN
      IF(LINP) WRITE(PMS,115) IMATCH,IPAL,INOAI,IOT,INOO
  115 FORMAT(' THIS PIN NUMBER ',I2,' IS AN INVALID INPUT PIN',
     1       ' FOR ',4A1,A1,A1,A1)
C     PRINT AN ERROR MESSAGE FOR INVALID PRODUCT LINE
  118 ILERR=ILL+4
      IF(LPRD) WRITE(PMS,119)
     1 (ISYM(I,IPRD),I=1,8),IPRD,ILERR,(IPAGE(I,ILL),I=1,80)
  119 FORMAT(/,' OUTPUT PIN NAME = ',8A1,'  OUTPUT PIN NUMBER = ',I2,
     1       /,' MINTERM IN LINE NUMBER ',I4,/,' ',80A1)
      IF( LPRD.AND.COUNT.LT.8 )
     1         WRITE(PMS,116) IPROD,IPAL,INOAI,IOT,INOO
  116 FORMAT(' THIS PRODUCT LINE NUMBER ',I2,' IS NOT VALID',
     1       ' FOR ',4A1,A1,A1,A1)
      IF( LPRD.AND.COUNT.GT.8 )
     1         WRITE(PMS,117) IPAL,INOAI,IOT,INOO
  117 FORMAT(' MAXIMUM OF 8 PRODUCT LINES ARE VALID FOR ',4A1,A1,A1,A1,
     1     /,' TOO MANY MINTERMS ARE SPECIFIED IN THIS EQUATION')
      CALL EXIT
  102 IF(chipType.LE.4) CALL TWEEK(chipType,IOT,LFUSES,LPHANT)
  108 WRITE(6,106)
  106 FORMAT(/,' OPERATION CODES:')
      WRITE(6,107)
  107 FORMAT(/,' E=ECHO INPUT  O=PINOUT  T=SIMULATE  P=PLOT ',
     1       /,' B=BRIEF H=HEX  S=SHORT  L=BHLF  N=BNPF   ',
     2       /,' C=CATALOG      Q=QUIT      F=FAULT TESTING  ',
     3       /,'      J=JEDEC FORMAT   I= INTEL HEX')
      WRITE(6,110)
  110 FORMAT(/,' ENTER OPERATION CODE: ',$)
      READ(ROC,120) IOP
  120 FORMAT(A1)
C     IF(POF.NE.6) WRITE(POF,125)
C 125 FORMAT('1')
      IF(IOP.EQ.E) CALL ECHO(IPAL,INOAI,IOT,INOO,REST,PATNUM,TITLE,
     1                       COMP)
      IF(IOP.EQ.O) CALL PINOUT(IPAL,INOAI,IOT,INOO,TITLE)
      IF(IOP.EQ.T) CALL TEST(LPHASE,LBUF,TITLE,IC,IL,ILE,ISYM,IBUF,
     1                       chipType,INOO,LFIX,IPCTR,LERR,ISAF,IPCTR1,
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
     1                   chipType,INOO,LFIX,IPCTR,LERR,ISAF,IPCTR1,
     2                   .FALSE.,.FALSE.,IOP.NE.JJ)
                  CALL PLOTF(chipType,IOT)
C
C        
C
  135 IF(IOP.EQ.P) CALL PLOT(LBUF,IBUF,LFUSES,IPROD,TITLE,.TRUE.,chipType,
     1                       LPROD,IOP,IBLOW,IPCTR0)
      IF(IOP.EQ.B) CALL PLOT(LBUF,IBUF,LFUSES,IPROD,TITLE,.TRUE.,chipType,
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
            IF(G.EQ.II) CALL INTEL(LFUSES,II)
      IF(IOP.NE.Q ) GO TO 108
      CALL EXIT
C     ADDITION FOR SA1/SA0 TESTS
C     SETTING THE PARAMETERS FOR SA1/SA0 TESTS
  200 IPCTR=0
      CALL TEST(LPHASE,LBUF,TITLE,IC,IL,ILE,ISYM,IBUF,chipType,INOO,
     1          IFIX,IPCTR,LERR,ISAF,IPCTR1,.FALSE.,.FALSE.,IOP.NE.JJ)
      IF(IFUNCT.EQ.0) GO TO 135
      IPCTR0=IPCTR
C     LOOPING FOR SA1 TEST
      DO 210 IPCTR1=1,IPCTR0
      LSA11=.TRUE.
      CALL TEST(LPHASE,LBUF,TITLE,IC,IL,ILE,ISYM,IBUF,chipType,INOO,
     1          IFIX,IPCTR,LERR,ISAF,IPCTR1,LSA11,.FALSE.,IOP.NE.JJ)
  210 CONTINUE
      ISA1=ISAF
C     LOOPING FOR SA0 TEST
      DO 215 IPCTR1=1,IPCTR0
      LSA01=.TRUE.
      CALL TEST(LPHASE,LBUF,TITLE,IC,IL,ILE,ISYM,IBUF,chipType,INOO,
     1          IFIX,IPCTR,LERR,ISAF,IPCTR1,.FALSE.,LSA01,IOP.NE.JJ)
  215 CONTINUE
      ISA0=ISAF-ISA1  
      IFAULT=(ISAF*100)/(IPCTR0*2)
      WRITE(POF,220) ISA1
C
  220 FORMAT(/,' NUMBER OF STUCK AT ONE (SA1)  FAULTS ARE =' I3)
      WRITE(POF,225) ISA0
  225 FORMAT(/,' NUMBER OF STUCK AT ZERO (SA0) FAULTS ARE =' I3)
      WRITE(POF,230) IFAULT
  230 FORMAT(/,' PRODUCT  TERM   COVERAGE                 =' I3,'%',//)
      GO TO 135 
C     END OF ADDITION FOR SA1/SA0 TEST 
      END

C**************************************************
C
C
      INTEGER FUNCTION ICONV(K)
      IMPLICIT INTEGER (A-Z)
C      INTEGER A,B,C,D,E,F,G,H
C     1,I,J,X,L,M,N,O,P
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


void getsym(int &J) {
      
      LFIX=false;
      if (LLEFT || LAND || LOR || LEQUAL || LRIGHT) {
            incr();
            if (LLEFT) {
                  LFIX = true;
                  return;
            }
      }
      LPHASE[J] = !LSLASH;
      if (!LPHASE[J]) {
            incr();
      }
      for (size_t tt = 0; tt < 8; tt++) {
            ISYM[tt][J] = ' ';
      }

      do {
            for (size_t tt = 0; tt < 7; tt++) {
                  ISYM[tt][J] = ISYM[tt+1][J];
            }
   
            ISYM[7][J]=IPAGE[IC][IL];
            incr(IC,IL,LFIX);
            if( LLEFT || LBLANK || LAND || LOR || LRIGHT || LEQUAL ) {
                  return;
            }
      } while (1);
}

void incr(int &ic, int &il, int& lfix ){

      LBLANK= false;
      LXOR= false;
      LXNOR= false;
      bool LX1{false};
      LRIGHT= false;

      do {
            ++IC;
            char ch = content[IL][IC];
            if (IC >= 80 || ch == ';') {
                  IL++;
                  IC = 0;
                  if (IL == content.size()) { //EOF
                        return;
                  }
                  continue;
            }
            if (ch == ':' && LFIX) {
                  return;
            }
            if (ch == ' ' || ch == '\t') {
                  LBLANK = true;
                  continue;
            }
            if (ch == ':') {
                  if (!LXOR && !LXNOR) {
                        LX1 = true;
                        continue;
                  }
                  if (LXOR) {
                        LOR = true;
                  }
                  if (LXNOR) {
                        LXNOR = true;
                  }
                  return;
            }
            // 32
            if (LX1 && (ch == '+' || ch == '*')) {
                  if (ch == '+') {
                        LXOR = true;
                  }
                  if (ch == '*') {
                        LXNOR = true;
                  }
                  continue;
            }
            LLEFT = ch == '(';
            LAND  = ch == '*';
            LOR   = ch == '+';
            LSLASH= ch == '/';
            LEQUAL=ch == '=';
            LRIGHT=ch == ')';
            return;
      }   
}

void match(int &IMATCH,IBUF,ISYM){
 
      IMATCH=0
      for (size_t tt = 0; tt < 20; tt++) {
          LMATCH= true;
          for (size_t ii = 0; ii < 8; ii++) {
            LMATCH=LMATCH && (IBUF[ii][1] == ISYM[ii][tt];
          }
          if(LMATCH) {
            IMATCH=J;
          }
      }

      //     MATCH CARRY WHICH IS FOUND IN THE PAL16A4
      if( IBUF[3][1] == 'C' && IBUF[4][1] == 'A' &&
          IBUF[5][1] == 'R' && IBUF[6][1] == 'R' &&
          IBUF[7][1] == 'Y' &&) {
            IMATCH=99;
      }

}

      SUBROUTINE IXLATE(IINPUT,IMATCH,LPHASE,LBUF,chipType)
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
      IF( ITABLE(IMATCH,chipType).GT.0 ) IINPUT=ITABLE(IMATCH,chipType)+IBUBL
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
      INTEGER A,B,ISLASH,IOR,IBLANK,IRIGHT,IPAGE(80,200)
     1         ,IAND,N,Q,N0,N1,N2,N3,CI
     2         ,ICOLON
     3         ,TABLE(5,14),IBUF(8,20),FIXBUF(8)
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
      CALL PLOT(LBUF,IBUF,LFUSES,IPROD,TITLE,.FALSE.,chipType,
     1          LPROD,IOP,IBLOW,IPCTR)
  100 LFIX=.FALSE.
      CALL INCR(IC,IL,LFIX)
      RETURN
      END
C
C***********************************************************************
C
void echo(const vector<string>& content) {
      for (const auto &line: content) {
            cout << line << endl;
      }
}


void cat() {
      cout << endl
           << "  MONOLITHIC MEMORIES 20-PIN PALASM VERSION 1.6C" << endl
           << "  (C) COPYRIGHT 1983 MONOLITHIC MEMORIES" << endl << endl
           << "   ECHO (E)     - PRINTS THE PAL DESIGN  SPECIFICATION" << endl
           << "   PINOUT (O)   - PRINTS THE PINOUT OF THE PAL" << endl
           << "   SIMULATE (T) - EXERCISES THE FUNCTION TABLE"
           << "  VECTORS IN THE LOGIC" << endl                 
           << "  EQUATIONS AND GENERATES TEST VECTORS" << endl
           << "   PLOT (P)     - PRINTS THE ENTIRE FUSE PLOT" << endl
           << "    BRIEF (B)    - PRINTS ONLY THE USED PRODUCT LINES" << endl
           << " OF THE FUSE PLOT" << endl
           << "                   PHANTOM FUSES ARE OMITTED" << endl
           <<"    SHORT (S)    - GENERATES SHORT VERSION OF HEX PROGRAMMING FORMAT" << endl
           << "    HEX (H)      - GENERATES HEX PROGRAMMING FORMAT" << endl
           << "    BHLF (L)     - GENERATES BHLF PROGRAMMING FORMAT" << endl
           << "    BNPF (N)     - GENERATES BNPF PROGRAMMING FORMAT" << endl
           << "    CATALOG (C)  - PRINTS THE PALASM CATALOG"
           << "    QUIT (Q)     - EXIT PALASM" << endl
           << "    FAULT (F)    - FAULT TESTING " << endl
           << "    JEDEC (J)    - JEDEC FORMAT FOR DATA I/O PROGRAMMER" << endl
           << "    INTEL (I)    - INTEL HEX PROGRAMMING FORMAT" << endl;
}

/*
C
C***********************************************************************
C
      SUBROUTINE PINOUT(IPAL,INOAI,IOT,INOO,TITLE)
C     THIS SUBROUTINE PRINTS THE PINOUT OF THE PAL
      IMPLICIT INTEGER (A-Z)
      INTEGER IBLANK,ISTAR,IPAGE(80,200),IPAL(4),IIN(7,2),IOT,INOAI
     1         ,INOO,PIN(12,20)
     2         ,TITLE(80)
      COMMON /PGE/ IPAGE
      COMMON /LUNIT/ PMS,POF,PDF
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
      WRITE(POF,76) TITLE
   76 FORMAT(/,' ',80A1)
      WRITE(POF,78) ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,
     1              ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,
     2              ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,
     3              ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR
   78 FORMAT(/,' ',18X,14A1,3X,14A1,
     1       /,' ',18X,A1,13X,A1,1X,A1,13X,A1)
      JJ=20
      DO 88 J=1,10
          WRITE(POF,80) ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR
   80     FORMAT(' ',15X,4A1,29X,4A1)
          WRITE(POF,81) (PIN(I,J),I=1,12),ISTAR,J,ISTAR,
     1         (IIN(I,1),I=1,7),ISTAR,JJ,ISTAR,(PIN(I,JJ),I=1,12)
   81     FORMAT(' ',12A1,3X,A1,I2,A1,11X,7A1,11X,A1,I2,A1,3X,12A1)
          WRITE(POF,82) ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR,ISTAR
   82     FORMAT(' ',15X,4A1,29X,4A1)
          WRITE(POF,84) ISTAR,(IIN(I,2),I=1,7),ISTAR
   84     FORMAT(' ',18X,A1,11X,7A1,11X,A1)
          DO 86 II=1,2
              DO 85 I=1,7
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
      SUBROUTINE PLOT(LBUF,IBUF,LFUSES,IPROD,TITLE,LDUMP,chipType,
     1                LPROD,IOP,IBLOW,IPCTR0)
C     THIS THIS SUBROUTINE PRODUCES THE FUSE PLOT
      IMPLICIT INTEGER (A-Z)
      INTEGER ISAVE(64,32),IAND,IOR,ISLASH,IBUF(8,20),IOUT(64),IOP
     1        ,IDASH,X,IBLANK,P,B
     2        ,HIFANT
C
     3        ,TITLE(80)
      LOGICAL LBUF(20),LFUSES(32,64),LDUMP,LPROD(80)
      COMMON /LUNIT/ PMS,POF,PDF
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
   60 WRITE(POF,62) TITLE
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
              IF(chipType.LE.4) CALL FANTOM(chipType,IOUT,IPROD,I8PRO)
              IPROD=IPROD-1
              DO 85 J=1,32
                  IF( IOP.EQ.B.AND.IOUT(J).EQ.HIFANT ) IOUT(J)=IBLANK
  85          CONTINUE
              IF( (IOP.EQ.P).OR.(IOP.EQ.B.AND.(LPROD(IPROD+1))) )
     1        WRITE(POF,90) IPROD,IOUT
   90         FORMAT(' ',I2,8(' ',4A1),' ',32A1)
   94         CONTINUE
          WRITE(POF,96)
C
   96     FORMAT(1X)
  100     CONTINUE
      WRITE(POF,110)
  110 FORMAT(/,
     1' LEGEND:  X : FUSE NOT BLOWN (L,N,0)   - : FUSE BLOWN   (H,P,1)')
      IF( IOP.EQ.P.AND.chipType.LE.4 ) WRITE(POF,111)
  111 FORMAT(
     1'          0 : PHANTOM FUSE   (L,N,0)   O : PHANTOM FUSE (H,P,1)')
      WRITE(POF,112) IBLOW
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
      INTEGER ZTABLE(16),ZCSUM(4),IOP,ITEMP(64)
      COMMON /LUNIT/ PMS,POF,PDF
      DATA H/'H'/,S/'S'/,IBLANK/' '/,
     1    ZTABLE/'0','1','2','3','4','5','6','7',
     2           '8','9','A','B','C','D','E','F'/
      DATA SOH/001/,STX/002/,ETX/003/,BEL/007/
      CSUM=0
      IF(IOP.EQ.H) WRITE(PDF,10)
   10 FORMAT(//,'                                         .',//)
C***** NOTE: SOME PROM PROGRAMMERS NEED A START CHARACTER.
C*****       THIS PROGRAM OUTPUTS AN STX FOR THE DATA I/O MODEL 9
C*****         (USE SOH FOR MODEL 5)
      WRITE(PDF,5) BEL,BEL,BEL,BEL,BEL,BEL,BEL,STX,SOH
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
          IF(IOP.EQ.H) WRITE(PDF,30) ITEMP
   30     FORMAT(' ',32(A1,' '),'.',/,' ',32(A1,' '),'.')
   40     IF(IOP.EQ.S) WRITE(PDF,50) ITEMP
   50     FORMAT(' ',64A1)
      IF(IOP.EQ.H) WRITE(PDF,70)
C
   70 FORMAT(' ',//,'                                       .',//)
      WRITE(PDF,80) ETX
   80 FORMAT(' ',A1)
C     CONVERT DECIMAL CHECK SUM INTO HEX CHECK SUM
      DO 85 I=1,4
           ZTEMP=CSUM-16*(CSUM/16)
           ZCSUM(5-I)=ZTABLE(ZTEMP+1)
           CSUM=CSUM/16
   85 CONTINUE
      IF(ZCSUM(1).EQ.ZTABLE(1)) ZCSUM(1)=IBLANK
      WRITE(PMS,90) ZCSUM(1),ZCSUM(2),ZCSUM(3),ZCSUM(4)
   90 FORMAT(/' HEX CHECK SUM = ',4A1)
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
      COMMON /LUNIT/ PMS,POF,PDF
      WRITE(PDF,10)
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
   20         WRITE(PDF,30) ITEMP
   30         FORMAT(' ',8('b',4A1,'f '))
      WRITE(PDF,10)
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
      INTEGER R,I1,I2,I4,I6,I8,IOT,INOAI,INOO
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
      SUBROUTINE FANTOM(chipType,IOUT,IPROD,I8PRO)
C     THIS SUBROUTINE UPDATES IOUT (THE PRINTED FUSE PLOT)
C      FOR HIGH AND LOW PHANTOM FUSES
      IMPLICIT INTEGER (A-Z)
C
      INTEGER X,IDASH,LOFANT,HIFANT, IOUT(64)
      DATA X/'X'/,IDASH/'-'/,LOFANT/'0'/,HIFANT/'O'/
      DO 10 I=1,32
          IF( IOUT(I).EQ.IDASH ) IOUT(I)=HIFANT
          IF( IOUT(I).EQ.X )     IOUT(I)=LOFANT
   10 CONTINUE
      IF((chipType.EQ.4).AND.((IPROD.LE.24).OR.(IPROD.GE.41))) RETURN
      IF((chipType.EQ.3).AND.((IPROD.LE.16).OR.(IPROD.GE.45))) RETURN
      IF((chipType.EQ.2).AND.((IPROD.LE. 8).OR.(IPROD.GE.53))) RETURN
      IF((chipType.LE.3).AND.(I8PRO.GE.5)) RETURN
      IF((chipType.LE.2).AND.(IPROD.GE.19).AND.(IPROD.LE.48).AND.
     1   (I8PRO.GE.3)) RETURN
      IF((chipType.EQ.1).AND.(I8PRO.GE.3)) RETURN
      DO 50 I=1,32
        IF(((I.EQ.15).OR.(I.EQ.16).OR.(I.EQ.19).OR.(I.EQ.20)).AND.
     1   (chipType.LE.3)) GO TO 50
        IF(((I.EQ.11).OR.(I.EQ.12).OR.(I.EQ.23).OR.(I.EQ.24)).AND.
     1   (chipType.LE.2)) GO TO 50
        IF(((I.EQ. 7).OR.(I.EQ. 8).OR.(I.EQ.27).OR.(I.EQ.28)).AND.
     1   (chipType.LE.1)) GO TO 50
        IF( IOUT(I).EQ.HIFANT ) IOUT(I)=IDASH
        IF( IOUT(I).EQ.LOFANT ) IOUT(I)=X
   50 CONTINUE
      RETURN
      END

C
C***********************************************************************
C
      SUBROUTINE IODC4
C***** THIS ROUTINE IS OPTIONAL, IT MAY BE USED TO TURN PERIPHERALS OFF
      IMPLICIT INTEGER (A-Z)
      INTEGER BEL,DC3,DC4
      COMMON /LUNIT/ PMS,POF,PDF
      DATA BEL/007/,DC3/023/,DC4/024/
      WRITE(PDF,10) BEL,DC3,DC4
   10 FORMAT(' ',3A1)
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE TEST(LPHASE,LBUF,TITLE,IC,IL,ILE,ISYM,IBUF,chipType,INOO,
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
      INTEGER  IPAGE(80,200),INOO,ISYM(8,20),ISYM1(8,20)
     1         ,IDASH,L,H,X,C,Z,N0,ITEST,IPROD,ISUM,XORSUM
     2         ,N1,ERR,IBLANK,COMENT,I4,I6
     3         ,I8
     4         ,IVECTP(20),IBUF(8,20),TITLE(80)
      INTEGER TSTVEC(20,50),ISTATE(20),IVECT(20),ISTATT(20)
      INTEGER BEL
      COMMON  LBLANK,LLEFT,LAND,LOR,LSLASH,LEQUAL,LRIGHT,LXOR,LXNOR
      COMMON /PGE/ IPAGE
      COMMON /LUNIT/ PMS,POF,PDF
      COMMON /FTEST/ IFUNCT,IDESC,IEND
      COMMON /TSTVEC/ NTEST,TSTVEC
      DATA IDASH/'-'/,L/'L'/,H/'H'/,X/'X'/,C/'C'/,Z/'Z'/,N0/'0'/,
     1     N1/'1'/,ERR/'?'/,IBLANK/' '/,COMENT/';'/,I4/'4'/,I6/'6'/,
     2     I8/'8'/,NN/'N'/
      DATA BEL/007/
C
         NTEST = 0
C     PRINT AN ERROR MESSAGE IF NO FUNCTION TABLE IS SUPPLIED
      IF(IFUNCT.NE.0) GO TO 3
      WRITE(PMS,2)
    2 FORMAT(/,' FUNCTION TABLE MUST BE SUPPLIED IN ORDER TO PERFORM',
     1         ' SIMULATION')
      RETURN
C     PRINT TITLE
    3 IF((.NOT.LSA11).AND.(.NOT.LSA01).AND.LPRINT)
     1 WRITE(POF,4) TITLE
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
      WRITE(PMS,6) (IBUF(J,1),J=1,8)
    6 FORMAT(/,' FUNCTION TABLE PIN LIST ERROR AT', 8A1) 
      RETURN
    7 LOUT(I)=.FALSE.
      ISTATT(I)=X
      IVECTP(I)=X
C     IF APPROPIATE PAL TYPE, REMEMBER LOCATION OF CLOCK AND THREE-STATE    
C      ENABLE PIN IN FUNCTION TABLE PIN LIST
      IF(chipType.NE.6) GO TO 10
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
         WRITE(PMS,8) IVECT(I),NVECT
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
      IF(chipType.NE.6) GO TO 25
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
      IF(chipType.LE.5) NREG=.TRUE.
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
      IF(IMESS.EQ.41) WRITE(PMS,41) NVECT,(ISYM1(J,I),J=1,8)
   41 FORMAT(/,' FUNCTION TABLE ERROR IN VECTOR',I4,'  PIN =',8A1,
     1         '  EXPECT = H  ACTUAL = L')
      IF(IMESS.EQ.42) WRITE(PMS,42) NVECT,(ISYM1(J,I),J=1,8)
   42 FORMAT(/,' FUNCTION TABLE ERROR IN VECTOR',I4,'  PIN =',8A1,
     1         '  EXPECT = L  ACTUAL = H')
      IF(IMESS.EQ.43) WRITE(PMS,43) NVECT,(ISYM1(J,I),J=1,8)
   43 FORMAT(/,' FUNCTION TABLE ERROR IN VECTOR',I4,'  PIN =',8A1, 
     1       /,'  EXPECT  = OUTPUT ENABLE  ACTUAL = Z')
      IF(IMESS.EQ.44) WRITE(PMS,44) NVECT,(ISYM1(J,I),J=1,8),IVECT(I)
   44 FORMAT(/,' FUNCTION TABLE ERROR IN VECTOR',I4,'  PIN =',8A1,
     1         '  EXPECT = Z  ACTUAL = ',A1)
      IF( (IMESS.NE.40).AND.(PMS.EQ.6) ) WRITE(PMS,45) BEL
   45 FORMAT(' ',A1)
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
     1 WRITE(POF,60) NVECT,(ISTATE(I),I=1,20)
   60 FORMAT(' ',I4,' ',20A1)
C
C         GENERATE TEST VECTORS FOR
C         JEDEC FORMAT OUTPUT
C
         IF (NVECT.EQ.51) WRITE(PMS,1000)
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
     1 WRITE(POF,150) IPCTR4,IEQN1
  150 FORMAT(' ',' PRODUCT: ',I3,' OF ','EQUATION.',I3,'
     1 UNTESTED(SA1)FAULT')
      IF((.NOT.LERR).AND.(LSA01).AND.LPRINT)
     1 WRITE(POF,155) IPCTR4,IEQN1
  155 FORMAT(' ',' PRODUCT: ',I3,' OF ','EQUATION.',I3,'
     1 UNTESTED(SA0)FAULT')  
C     *END OF ADDITION
      IF((.NOT.LERR).AND.((.NOT.LSA11).AND.(.NOT.LSA01)).AND.LPRINT)
     1 WRITE(POF,67)
   67 FORMAT(/,' PASS SIMULATION')
      IPCTR=IPCTR/(NVECT-1)
      IF((     LERR).AND.((.NOT.LSA11).AND.(.NOT.LSA01)).AND.LPRINT)
     1 WRITE(POF,68) NERR
   68 FORMAT(/,' NUMBER OF FUNCTION TABLE ERRORS =',I4)
      RETURN
C     PRINT AN ERROR MESSAGE FOR AN UNDEFINED PIN NAME
  100 ILERR=ILL+4
      WRITE(PMS,101) (IBUF(I,1),I=1,8),ILERR,(IPAGE(I,ILL),I=1,80)
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
      INTEGER L,H,X,Z,IPAGE(80,200),ISYM(8,20),ISYM1(8,20),IBUF(8,20)
     1         ,IVECT(20),IVECTP(20),ITEST,ITESTA,ITESTB
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
*/

char iconv(int k) {
    if (k < 10) {
        return '0' + k;
    }
    return 'A' + (k - 10);
}
/*
C
C  ************THIS SUBROUTINE IS ADDED FOR JEDEC FORMAT*********
C  THE FOLLOWING SUBROUTINE GIVES JEDEC FORMATTED OUTPUT FOR 
C  PROGRAMMING COMPATIBILITY WITH DATA I/O PROGRAMMERS

      SUBROUTINE PLOTF(chipType,IOT)
      IMPLICIT INTEGER (A-Z)
      LOGICAL  LFUSES (32,64),LPHANT(32,64)
      INTEGER ZERO,ONE,IPBUF(32),IOT,STAR
      INTEGER  TSTVEC(20,50)
      INTEGER ISUM(4),IADR,STX,ETX,IDEC(4),IPT,IINP,J1,J2,J3,PINOUT
      INTEGER IDECIO(4),ISUMIO(4),BUFIO(32),NFUSE,NTEST
      COMMON  /LUNIT/ PMS,POF,PDF
      COMMON  /IPT/ IPT
      COMMON  /LFUSES/ LFUSES,LPHANT
      COMMON  /SUM/ ISUM,IDEC,IPBUF,BUFIO,NFUSE
      COMMON  /TSTVEC/ NTEST, TSTVEC
      DATA    ZERO/'0'/, ONE/'1'/, STAR/'*'/,L/'L'/,H/'H'/,X/'X'/
     1        C/'C'/,Z/'Z'/,N0/'0'/,N1/'1'/,NN/'N'/
     */

vector<string> plotf(int chipType, char IOT, int isum[],
                    int nfuse, int idec[], char tstvec[20][50]) {
    int IADR=0;
    static const char STX=2;
    static const char ETX=3;
    int j1, j2, j3;
    char idecio[4];
    stringstream ss;

    int pinout;
    if (chipType == 6) {
        pinout = 24;
    } else if (IOT == 'L') {
        pinout = chipType + 12;
    } else {
        pinout = chipType + 17;
        if (chipType == 4 && IOT == 'H') {
            pinout++;
        } 
    }
   
    isum[4]=536 + pinout/10 + pinout%10;
    isum[2]=(isum[4]/256)%256;
    isum[4]=isum[4]%256;
    
    ss << " " << STX << "*D22" << pinout << "*F0*";

    for (int ipt = 0; ipt < 64; ipt++) {   
        if (lphant[9][ipt]) {
            confinue;
        }
        nfuse = 0;

        for (size_t iinp = 0; iinp < 32; iinp++) {
            if(!lphant[iinp][ipt]){
                  nfuse++;
                  if(lfuses[iinp][ipt]) {
                    ipbuf[nfuse]='1';
                  } else {
                    ipbuf[nfuse]='0';
                  }
            }
        }
        
      //IF(LFUSES(1,IPT)) GO TO 100
      //IF(.NOT.LFUSES(2,IPT)) GO TO 250
  //100 IDEC(4)=IADR
    if (lfuses[0][ipt] || lfuses[1][ipt]){
        for (size_t j = 0; j < 3; j++) {
      //DO 150 J=1,3
            j1 = 3-j;//J1=5-J
            j2 = 2-j;//J2=4-J
            idec[j2]=idec[j1]/10;
            idec[j1]=idec[j1]-10*idec[j2];
            idecio[j1]=iconv(idec[j1]);
  //150 CONTINUE
        }

        idecio[0]=iconv(idec[0])
        ss << " L" << idecio[0] << idecio[1] 
           << idecio[2] << idecio[3];
        for (size_t tt = 0; tt < nfuse; tt++) {
            ss << ipbuf[tt];
        }   
        ss << "*";
    }
  //    WRITE(PDF,201)IDECIO,(IPBUF(I),I=1,NFUSE),STAR
  //201 FORMAT(' L',4A1,' ',32A1,A1)
        iaddr += nfuse;
  //250 IADR=IADR+NFUSE
  //300 CONTINUE
      }
  
C
C            GENERATE TEST VECTORS.
C
            IF (NTEST.LE.0) GOTO 380
               IF (NTEST.GT.50) NTEST = 50
               DO 370 J = 1,NTEST
               ISUM(4) = ISUM(4) + 215
               ISUM(2) = MOD(ISUM(2)+ISUM(4)/256,256)
               ISUM(4) = MOD(ISUM(4),256)

               //DO 350 I = 1,20
               for (size_t tt = 0; tt < 20; tt++) {
                isum[3] += tstvec[tt][j];
                
               ISUM(2) = MOD(ISUM(2)+ISUM(4)/256,256)
               ISUM(4) = MOD(ISUM(4),256)
               }
  //350          CONTINUE

                for (int i = 0; i < 4; i++) {
                    idecio[i] = (j/10**(3-i))%10; //power of 3-i
                    isum[3] = isum[3] + idecio[i] + 48;
                    isum[1] = (isum[1]+isum[3])/256)%256;
                    isum[3] = isum[3]%256;
                }
                ss << " V" << idecio[0] << idecio[1]
                   << idecio[2] << idecio[3];
                //endl
                for (size_t tt = 0; tt < 20; tt++) {
                    ss << tstvec[tt][j];
                }  
                ss << " *";
  //             WRITE(PDF,410) IDECIO,(TSTVEC(I,J),I=1,20)
  //410          FORMAT(' V',4I1,1X,20A1,' *')
  //370          CONTINUE
//C
//C           ENDIF       
//C
  //380     
      isumio[0]=iconv(isum[1]/16;
      isum[1]=isum[1]%16;
      isumio[1]=iconv(isum[1]);
      isumio[2]=iconv(isum[3]/16;
      isum[3]=isum[3]%16;
      isumio[3]=iconv(isum[3]);
      ss << etx << isumio[0] << isumio[1]
         << isumio[2] << isumio[3];
      //WRITE(PDF,400) ETX,ISUMIO
  //400 FORMAT(' ',A1,4A1,/)

}
#endif