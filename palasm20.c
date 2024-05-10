/* palasm20.f -- translated by f2c (version 12.02.01).
   You must link the resulting object file with libf2c:
	on Microsoft Windows system, link with libf2c.lib;
	on Linux or Unix systems, link with .../path/to/libf2c.a -lm
	or, if you install libf2c.a in a standard place, with -lf2c -lm
	-- in that order, at the end of the command line, as in
		cc *.o -lf2c -lm
	Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

		http://www.netlib.org/f2c/libf2c.zip
*/

#include <stdlib.h> /* For exit() */
#include <f2c.h>

/* Common Block Declarations */

struct {
    logical lblank, lleft, land, lor, lslash, lequal, lright, lxor, lxnor;
} _BLNK__;

#define _BLNK__1 _BLNK__

struct {
    integer ipage[16000]	/* was [80][200] */;
} pge_;

#define pge_1 pge_

struct {
    logical lfuses[2048]	/* was [32][64] */, lphant[2048]	/* 
	    was [32][64] */;
} lfuses_;

#define lfuses_1 lfuses_

struct {
    integer pms, pof, pdf;
} lunit_;

#define lunit_1 lunit_

struct {
    integer ifunct, idesc, iend;
} ftest_;

#define ftest_1 ftest_

struct {
    integer ntest, tstvec[1000]	/* was [20][50] */;
} tstvec_;

#define tstvec_1 tstvec_

struct {
    integer ipt;
} ipt_;

#define ipt_1 ipt_

struct {
    integer isum[4], idec[4], ipbuf[32], bufio[32], nfuse;
} sum_;

#define sum_1 sum_

/* Table of constant values */

static integer c__1 = 1;
static integer c__4 = 4;
static integer c__73 = 73;
static integer c__80 = 80;
static logical c_false = FALSE_;
static logical c_true = TRUE_;
static integer c__64 = 64;
static integer c__32 = 32;
static integer c__10 = 10;

/* **PALASM20**PALASM20**PALASM20**PALASM20**PALASM20**PALASM20**PALASM20 */

/*  P A L A S M  2 0  -  TRANSLATES SYMBOLIC EQUATIONS INTO PAL OBJECT */
/*                       CODE  FORMATTED FOR DIRECT INPUT TO STANDARD */
/*                       PROM PROGRAMMERS. */

/*                       REV LEVEL:   VERSION 1.6C (08/16/83) */
/*                       (C) COPYRIGHT 1983 MONOLITHIC MEMORIES */


/*                       ********************************************* */
/*                       *                                           * */
/*                       *            APPROVAL                       * */
/*                       *                                           * */
/*                       * 1:JOHN BIRKNER                            * */
/*                       *                                           * */
/*                       *  PROGRAMMABLE LOGIC PLANNER               * */
/*                       *                                           * */
/*                       *                                           * */
/*                       * 2:VINCENT COLI                            * */
/*                       *                                           * */
/*                       * APPLICATIONS ENGINEER                     * */
/*                       *                                           * */
/*                       *                                           * */
/*                       * 3:MANOUCHEHR VAFAI                        * */
/*                       *                                           * */
/*                       * APPLICATIONS ENGINEER                     * */
/*                       *                                           * */
/*                       *                                           * */
/*                       ********************************************* */



/*                       INPUT:       PAL DESIGN SPECIFICATION ASSIGNED */
/*                                    TO RPD(1).  OPERATION CODES ARE */
/*                                    ASSIGNED TO ROP(5). */

/*                       OUTPUT:      ECHO, SIMULATION, AND FUSE PATTERN */
/*                                    ARE ASSIGNED TO POF(6).  HEX AND */
/*                                    BINARY PROGRAMMING FORMATS ARE */
/*                                    ASSIGNED TO PDF(6).  PROMPTS AND */
/*                                    ERROR MESSAGES ARE ASSIGNED TO */
/*                                    PMS(6). */

/*                       PART NUMBER: THE PAL PART NUMBER MUST APPEAR */
/*                                    IN COLUMN ONE OF LINE ONE. */

/*                       PIN LIST:    20 SYMBOLIC PIN NAMES MUST APPEAR */
/*                                    STARTING ON LINE FIVE. */

/*                       EQUATIONS:   STARTING FIRST LINE AFTER THE */
/*                                    PIN LIST IN THE FOLLOWING FORMS: */

/*                                       A = B*C + D */

/*                                       A := B*C + D */

/*                                       IF( A*B )  C = D + E */

/*                                       A2 := (A1:*:B1) + /C */


/*                                    ALL CHARACTERS FOLLOWING ';' ARE */
/*                                    IGNORED UNTIL THE NEXT LINE. */


/*                                    BLANKS ARE IGNORED. */

/*                       OPERATORS:   ( IN HIERARCHY OF EVALUATION ) */

/*                                     ;    COMMENT FOLLOWS */
/*                                     /    COMPLEMENT */
/*                                     *    AND (PRODUCT) */
/*                                     +    OR (SUM) */
/*                                    :+:   XOR (EXCLUSIVE OR) */
/*                                    :*:   XNOR (EXCLUSIVE NOR) */
/*                                    ( )   CONDITIONAL THREE-STATE */
/*                                          OR FIXED SYMBOL */
/*                                     =    EQUALITY */
/*                                    :=    REPLACED BY (AFTER CLOCK) */

/*                       FIXED SYMBOLS */
/*                       FOR PAL16X4 */
/*                       AND PAL16A4 */
/*                       ONLY:        (AN+/BN)     WHERE N = 0,1,2,3 */
/*                                    (AN+BN)      FOR OUTPUT PINS */
/*                                    (AN)         17,16,15,14, RESP */
/*                                    (/AN+/BN)    A IS OUTPUT */
/*                                    (/BN)        B IS INPUT */
/*                                    (AN:+:BN) */
/*                                    (AN+/BN) */
/*                                    (/AN+BN) */
/*                                    (AN:*:BN) */
/*                                    (BN) */
/*                                    (AN*BN) */
/*                                    (/AN) */
/*                                    (/AN+/BN) */
/*                                    (/AN*BN) */

/*                       FUNCTION     L, H, X, Z, AND C ARE VALID */
/*                         TABLE:     FUNCTION TABLE VECTOR ENTRIES. */

/*                       REFERENCE:   A COMPLETE USERS GUIDE TO */
/*                                    DESIGNING WITH PALS USING PALASM */
/*                                    IS PROVIDED IN THE MONOLITHIC */
/*                                    MEMORIES PAL HANDBOOK. */

/*                       SUBROUTINES: INITLZ,GETSYM,INCR,MATCH,FIXSYM, */
/*                                    IXLATE,ECHO,CAT,PINOUT,PLOT,TWEEK, */
/*                                    BINR,HEX,SLIP,FANTOM,IODC2,IODC4, */
/*                                    TEST,FIXTST,PLOTF,SUMCHK,INTEL */

/*                       AUTHORS:     JOHN BIRKNER AND VINCENT COLI */
/*                                    FAULT TESTING BY IMTIAZ BENGALI */
/*                                    JEDEC FORMAT  BY MANO VAFAI */
/*                                    MONOLITHIC MEMORIES INC. */
/*                                    1165 EAST ARQUES AVENUE */

/*                                    SUNNYVALE, CALIFORNIA 94043 */
/*                                    (408) 739-3535 */

/*                       FINE PRINT:  MONOLITHIC MEMORIES TAKES NO */
/*                                    RESPONSIBILITY FOR THE OPERATION */
/*                                    OR MAINTENANCE OF THIS PROGRAM. */
/*                                    THE SOURCE CODE AS PRINTED HERE */
/*                                    PRODUCED THE OBJECT CODE OF THE */
/*                                    EXAMPLES IN THE APPLICATIONS */
/*                                    SECTION ON A VAX/VMS 11/780 */
/*                                    COMPUTER AND A NATIONAL CSS IBM */
/*                                    SYSTEM/370 FORTRAN IV(G). */

/* *********************************************************************** */


/* *********************************************************************** */


/*     MAIN PROGRAM */

/* Actual main program */
int main(int argc, char **argv)
{
	extern int MAIN__();
	libf2c_init(argc, argv);
	MAIN__();
	libf2c_close();
	exit(0);
	return 0;
}

/* Main program */
int MAIN__(void)
{
    /* Initialized data */

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_108 = { "E   ", 0 };

#define e (*(integer *)&equiv_108)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_109 = { "C   ", 0 };

#define c__ (*(integer *)&equiv_109)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_110 = { "Q   ", 0 };

#define q (*(integer *)&equiv_110)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_111 = { "F   ", 0 };

#define f (*(integer *)&equiv_111)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_112 = { "A   ", 0 };

#define a (*(integer *)&equiv_112)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_113 = { "J   ", 0 };

#define jj (*(integer *)&equiv_113)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_114 = { "B   ", 0 };

#define bb (*(integer *)&equiv_114)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_115 = { "C   ", 0 };

#define cc (*(integer *)&equiv_115)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_116 = { "O   ", 0 };

#define o (*(integer *)&equiv_116)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_117 = { "D   ", 0 };

#define dd (*(integer *)&equiv_117)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_118 = { "E   ", 0 };

#define ee (*(integer *)&equiv_118)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_119 = { "F   ", 0 };

#define ff (*(integer *)&equiv_119)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_120 = { "I   ", 0 };

#define ii (*(integer *)&equiv_120)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_121 = { "N   ", 0 };

#define nn (*(integer *)&equiv_121)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_122 = { "O   ", 0 };

#define oo (*(integer *)&equiv_122)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_123 = { "P   ", 0 };

#define pp (*(integer *)&equiv_123)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_124 = { "R   ", 0 };

#define rr (*(integer *)&equiv_124)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_125 = { "S   ", 0 };

#define ss (*(integer *)&equiv_125)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_126 = { "T   ", 0 };

#define tt (*(integer *)&equiv_126)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_127 = { "T   ", 0 };

#define t (*(integer *)&equiv_127)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_128 = { "U   ", 0 };

#define uu (*(integer *)&equiv_128)

    static integer bel = 7;
    static struct {
	char e_1[4];
	integer e_2;
	} equiv_129 = { "P   ", 0 };

#define p (*(integer *)&equiv_129)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_130 = { "B   ", 0 };

#define b (*(integer *)&equiv_130)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_131 = { "H   ", 0 };

#define h__ (*(integer *)&equiv_131)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_132 = { "S   ", 0 };

#define s (*(integer *)&equiv_132)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_133 = { "L   ", 0 };

#define l (*(integer *)&equiv_133)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_134 = { "N   ", 0 };

#define n (*(integer *)&equiv_134)


    /* Format strings */
    static char fmt_3[] = "(/,\002 MONOLITHIC MEMORIES INC. PALASM VERSION 1"
	    ".6C\002)";
    static char fmt_33[] = "(\002 (C) COPYRIGHT 1983 MONOLITHIC MEMORIES\002)"
	    ;
    static char fmt_1[] = "(/,\002 WHAT IS THE LOGICAL UNIT NUMBER FOR OUTPU"
	    "T(6)?: \002$)";
    static char fmt_2[] = "(i4)";
    static char fmt_10[] = "(4a1,a1,a1,a1,73a1,/,80a1,/,80a1,/,80a1)";
    static char fmt_11[] = "(80a1)";
    static char fmt_18[] = "(/,\002 PAL PART TYPE \002,4a1,a1,a1,a1,\002 IS "
	    "INCORRECT\002)";
    static char fmt_23[] = "(/,\002 LESS THAN 20 PIN NAMES IN PIN LIST\002)";
    static char fmt_99[] = "(\002 \002,a1)";
    static char fmt_101[] = "(/,\002 ERROR SYMBOL = \002,8a1,\002      IN LI"
	    "NE NUMBER \002,i4,/,\002 \002,80a1)";
    static char fmt_103[] = "(\002 OUTPUT MUST BE INVERTED SINCE \002,4a1,a1"
	    ",a1,a1,\002 IS AN ACTIVE LOW DEVICE\002)";
    static char fmt_109[] = "(\002 OUTPUT CANNOT BE INVERTED SINCE \002,4a1,"
	    "a1,a1,a1,\002 IS AN ACTIVE HIGH DEVICE\002)";
    static char fmt_105[] = "(\002 THIS PIN NUMBER \002,i2,\002 IS AN INVALI"
	    "D OUTPUT PIN\002,\002 FOR \002,4a1,a1,a1,a1)";
    static char fmt_115[] = "(\002 THIS PIN NUMBER \002,i2,\002 IS AN INVALI"
	    "D INPUT PIN\002,\002 FOR \002,4a1,a1,a1,a1)";
    static char fmt_119[] = "(/,\002 OUTPUT PIN NAME = \002,8a1,\002  OUTPUT"
	    " PIN NUMBER = \002,i2,/,\002 MINTERM IN LINE NUMBER \002,i4,/"
	    ",\002 \002,80a1)";
    static char fmt_116[] = "(\002 THIS PRODUCT LINE NUMBER \002,i2,\002 IS "
	    "NOT VALID\002,\002 FOR \002,4a1,a1,a1,a1)";
    static char fmt_117[] = "(\002 MAXIMUM OF 8 PRODUCT LINES ARE VALID FOR"
	    " \002,4a1,a1,a1,a1,/,\002 TOO MANY MINTERMS ARE SPECIFIED IN THI"
	    "S EQUATION\002)";
    static char fmt_106[] = "(/,\002 OPERATION CODES:\002)";
    static char fmt_107[] = "(/,\002 E=ECHO INPUT  O=PINOUT  T=SIMULATE  P=P"
	    "LOT \002,/,\002 B=BRIEF H=HEX  S=SHORT  L=BHLF  N=BNPF   \002,/"
	    ",\002 C=CATALOG      Q=QUIT      F=FAULT TESTING  \002,/,\002   "
	    "   J=JEDEC FORMAT   I= INTEL HEX\002)";
    static char fmt_110[] = "(/,\002 ENTER OPERATION CODE: \002,$)";
    static char fmt_120[] = "(a1)";
    static char fmt_220[] = "(/,\002 NUMBER OF STUCK AT ONE (SA1)  FAULTS AR"
	    "E =\002i3)";
    static char fmt_225[] = "(/,\002 NUMBER OF STUCK AT ZERO (SA0) FAULTS AR"
	    "E =\002i3)";
    static char fmt_230[] = "(/,\002 PRODUCT  TERM   COVERAGE               "
	    "  =\002i3,\002%\002,//)";

    /* System generated locals */
    integer i__1;
    logical L__1;

    /* Local variables */
    static integer g, i__, j, ic, il;
    extern /* Subroutine */ int cat_(void);
    static integer ile, ill, rpd, iop, roc;
    extern /* Subroutine */ int hex_(logical *, integer *);
    static integer iot, lun, isa0, isa1;
    static logical lsa01, lsa11;
    extern /* Subroutine */ int echo_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *);
    static logical lact;
    static integer isaf, ipal[4], ibuf[160]	/* was [8][20] */;
    static logical lbuf[20];
    extern /* Subroutine */ int binr_(logical *, integer *, integer *), incr_(
	    integer *, integer *, logical *);
    static integer comp[80], iprd, ifix;
    static logical lprd, lfix, linp, lerr;
    static integer inoo;
    extern /* Subroutine */ int slip_(logical *, integer *, integer *, 
	    integer *, integer *, integer *), exit_(void);
    static integer rest[73];
    extern /* Subroutine */ int plot_(logical *, integer *, logical *, 
	    integer *, integer *, logical *, integer *, logical *, integer *, 
	    integer *, integer *), test_();
    static integer isym[160]	/* was [8][20] */, i8pro;
    extern /* Subroutine */ int match_(integer *, integer *, integer *);
    static integer inoai;
    static logical lsame;
    extern /* Subroutine */ int intel_(logical *, integer *);
    static integer iblow, iprod, ilerr;
    extern /* Subroutine */ int tweek_(integer *, integer *, logical *, 
	    logical *);
    static logical lprod[80];
    static integer title[80], ipctr;
    extern /* Subroutine */ int plotf_(integer *, integer *);
    static integer count, i88pro, itype, ipctr0, ipctr1, imatch;
    static logical lphase[20];
    static integer ifault;
    extern /* Subroutine */ int ixlate_(integer *, integer *, logical *, 
	    logical *, integer *);
    static logical lfirst, loperr;
    static integer patnum[80];
    extern /* Subroutine */ int getsym_(logical *, integer *, integer *, 
	    integer *, integer *, logical *), initlz_(integer *, integer *, 
	    integer *, integer *, logical *, logical *, integer *, integer *, 
	    integer *, logical *, integer *);
    static integer iinput;
    extern /* Subroutine */ int pinout_(integer *, integer *, integer *, 
	    integer *, integer *), fixsym_(logical *, integer *, integer *, 
	    integer *, logical *, logical *, integer *, integer *, logical *);

    /* Fortran I/O blocks */
    static cilist io___32 = { 0, 6, 0, fmt_3, 0 };
    static cilist io___33 = { 0, 6, 0, fmt_33, 0 };
    static cilist io___34 = { 0, 6, 0, fmt_1, 0 };
    static cilist io___35 = { 0, 5, 0, fmt_2, 0 };
    static cilist io___44 = { 0, 0, 0, fmt_10, 0 };
    static cilist io___54 = { 0, 0, 1, fmt_11, 0 };
    static cilist io___63 = { 0, 0, 0, fmt_18, 0 };
    static cilist io___66 = { 0, 0, 0, fmt_23, 0 };
    static cilist io___81 = { 0, 0, 0, fmt_99, 0 };
    static cilist io___82 = { 0, 0, 0, fmt_101, 0 };
    static cilist io___83 = { 0, 0, 0, fmt_103, 0 };
    static cilist io___84 = { 0, 0, 0, fmt_109, 0 };
    static cilist io___85 = { 0, 0, 0, fmt_105, 0 };
    static cilist io___86 = { 0, 0, 0, fmt_115, 0 };
    static cilist io___87 = { 0, 0, 0, fmt_119, 0 };
    static cilist io___88 = { 0, 0, 0, fmt_116, 0 };
    static cilist io___89 = { 0, 0, 0, fmt_117, 0 };
    static cilist io___90 = { 0, 6, 0, fmt_106, 0 };
    static cilist io___91 = { 0, 6, 0, fmt_107, 0 };
    static cilist io___92 = { 0, 6, 0, fmt_110, 0 };
    static cilist io___93 = { 0, 0, 0, fmt_120, 0 };
    static cilist io___105 = { 0, 0, 0, fmt_220, 0 };
    static cilist io___106 = { 0, 0, 0, fmt_225, 0 };
    static cilist io___107 = { 0, 0, 0, fmt_230, 0 };




/*     ASSIGNMENT OF DATA SET REFERENCES */
/*     RPD - PAL DESIGN SPECIFICATION (INPUT) */
/*     ROC - OPERATION CODE (INPUT) */
/*     POF - ECHO, PINOUT, TEST, AND PLOT (OUTPUT) */
/*     PDF - HEX AND BINARY FORMAT PROGRAM TAPES (OUTPUT) */
/*     PMS - PROMPTS AND ERROR MESSAGES (OUTPUT) */
    s_wsfe(&io___32);
    e_wsfe();
    s_wsfe(&io___33);
    e_wsfe();
    s_wsfe(&io___34);
    e_wsfe();

    s_rsfe(&io___35);
    do_fio(&c__1, (char *)&lun, (ftnlen)sizeof(integer));
    e_rsfe();


    rpd = 1;
    roc = 5;
    lunit_1.pof = lun;
    lunit_1.pdf = lun;
    lunit_1.pms = lun;
    ftest_1.ifunct = 0;
    ftest_1.idesc = 0;
/*     INITIALIZE LSAME AND LACT TO FALSE (ACTIVE HIGH/LOW ERROR) */
    lsame = FALSE_;
    lact = FALSE_;
/*     INITIALIZE LOPERR TO FALSE (OUTPUT PIN ERROR) */
    loperr = FALSE_;
/*     INITIALIZE LINP TO FALSE (INPUT PIN ERROR) */
    linp = FALSE_;
/*     INITIALIZE LPRD TO FALSE (PRODUCT LINE ERROR) */
    lprd = FALSE_;




/*     READ IN FIRST 4 LINES OF THE PAL DESIGN SPECIFICATION */
    io___44.ciunit = rpd;
    s_rsfe(&io___44);
    do_fio(&c__4, (char *)&ipal[0], (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&inoai, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&iot, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&inoo, (ftnlen)sizeof(integer));
    do_fio(&c__73, (char *)&rest[0], (ftnlen)sizeof(integer));
    do_fio(&c__80, (char *)&patnum[0], (ftnlen)sizeof(integer));
    do_fio(&c__80, (char *)&title[0], (ftnlen)sizeof(integer));
    do_fio(&c__80, (char *)&comp[0], (ftnlen)sizeof(integer));
    e_rsfe();
/*     READ IN PIN LIST (LINE 5) THROUGH THE END OF THE PAL DESIGN */
/*      SPECIFICATION */
    for (j = 1; j <= 200; ++j) {
	io___54.ciunit = rpd;
	i__1 = s_rsfe(&io___54);
	if (i__1 != 0) {
	    goto L16;
	}
	for (i__ = 1; i__ <= 80; ++i__) {
	    i__1 = do_fio(&c__1, (char *)&pge_1.ipage[i__ + j * 80 - 81], (
		    ftnlen)sizeof(integer));
	    if (i__1 != 0) {
		goto L16;
	    }
	}
	i__1 = e_rsfe();
	if (i__1 != 0) {
	    goto L16;
	}
/*     CHECK FOR 'FUNCTION TABLE' AND SAVE ITS LINE NUMBER */
	if (ftest_1.ifunct == 0 && pge_1.ipage[j * 80 - 80] == ff && 
		pge_1.ipage[j * 80 - 79] == uu && pge_1.ipage[j * 80 - 78] == 
		nn && pge_1.ipage[j * 80 - 77] == cc && pge_1.ipage[j * 80 - 
		76] == tt && pge_1.ipage[j * 80 - 75] == ii && pge_1.ipage[j *
		 80 - 74] == oo && pge_1.ipage[j * 80 - 73] == nn && 
		pge_1.ipage[j * 80 - 71] == tt && pge_1.ipage[j * 80 - 69] == 
		bb && pge_1.ipage[j * 80 - 67] == ee) {
	    ftest_1.ifunct = j;
	}
/*     CHECK FOR 'DESCRIPTION' AND SAVE ITS LINE NUMBER */
	if (ftest_1.idesc == 0 && pge_1.ipage[j * 80 - 80] == dd && 
		pge_1.ipage[j * 80 - 79] == ee && pge_1.ipage[j * 80 - 78] == 
		ss && pge_1.ipage[j * 80 - 77] == cc && pge_1.ipage[j * 80 - 
		76] == rr && pge_1.ipage[j * 80 - 75] == ii && pge_1.ipage[j *
		 80 - 74] == pp && pge_1.ipage[j * 80 - 73] == tt && 
		pge_1.ipage[j * 80 - 72] == ii && pge_1.ipage[j * 80 - 71] == 
		oo && pge_1.ipage[j * 80 - 70] == nn) {
	    ftest_1.idesc = j;
	}
/* L15: */
    }
/*     SAVE THE LAST LINE NUMBER OF THE PAL DESIGN SPECIFICATION */
L16:
    ftest_1.iend = j - 1;
    initlz_(&inoai, &iot, &inoo, &itype, lfuses_1.lfuses, lfuses_1.lphant, &
	    ic, &il, &iblow, &lfix, &ipctr);
    ile = il + 1;
/*     PRINT ERROR MESSAGE FOR INVALID PAL PART TYPE */
    if (itype != 0) {
	goto L17;
    }
    io___63.ciunit = lunit_1.pms;
    s_wsfe(&io___63);
    do_fio(&c__4, (char *)&ipal[0], (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&inoai, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&iot, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&inoo, (ftnlen)sizeof(integer));
    e_wsfe();

    exit_();
/*     GET 20 PIN NAMES */
L17:
    for (j = 1; j <= 20; ++j) {
/* L20: */
	getsym_(lphase, isym, &j, &ic, &il, &lfix);
    }
    if (! (_BLNK__1.lequal || _BLNK__1.lleft || _BLNK__1.land || _BLNK__1.lor 
	    || _BLNK__1.lright)) {
	goto L24;
    }
    io___66.ciunit = lunit_1.pms;
    s_wsfe(&io___66);
    e_wsfe();
    exit_();
L24:
    ile = il;
/*     BYPASS FUSE PLOT ASSEMBLY IF HAL (H IN COLUMN 1, LINE 1) */
    if (ipal[0] == h__) {
	goto L108;
    }
L25:
    getsym_(lbuf, ibuf, &c__1, &ic, &il, &lfix);
L28:
    if (! _BLNK__1.lequal) {
	goto L25;
    }
    count = 0;
    ill = il;
    match_(&imatch, ibuf, isym);
    if (imatch == 0) {
	goto L100;
    }
    iprd = imatch;
/*         CHECK FOR VALID POLARITY */
    lsame = lphase[imatch - 1] && lbuf[0] || ! lphase[imatch - 1] && ! lbuf[0]
	    ;
    if (iot == h__ && ! lsame) {
	lact = TRUE_;
    }
    if (! (iot == h__ || iot == c__) && lsame) {
	lact = TRUE_;
    }
/*         CHECK FOR VALID OUTPUT PIN */
    if ((itype == 1 || itype == 5 || itype == 6) && iot != a && (imatch < 12 
	    || imatch > 19)) {
	loperr = TRUE_;
    }
    if (itype == 2 && (imatch < 13 || imatch > 18)) {
	loperr = TRUE_;
    }
    if (itype == 3 && (imatch < 14 || imatch > 17)) {
	loperr = TRUE_;
    }
    if (itype == 4 && (imatch < 15 || imatch > 16)) {
	loperr = TRUE_;
    }
    if (lact || loperr) {
	goto L100;
    }
    i88pro = (19 - imatch << 3) + 1;
/*         START PAL16C1 ON PRODUCT LINE 24 (I88PRO=25) */
    if (iot == c__) {
	i88pro = 25;
    }
    ic = 0;
L30:
    incr_(&ic, &il, &lfix);
    if (! (_BLNK__1.lequal || _BLNK__1.lleft)) {
	goto L30;
    }
    lprod[i88pro - 1] = TRUE_;
    if (! _BLNK__1.lleft && rest[2] != pp) {
	slip_(lfuses_1.lfuses, &i88pro, &inoai, &iot, &inoo, &iblow);
    }
    for (i8pro = 1; i8pro <= 16; ++i8pro) {
	++count;
	if (_BLNK__1.lxor && i8pro != 5) {
	    goto L70;
	}
	iprod = i88pro + i8pro - 1;
	lprod[iprod - 1] = TRUE_;
	lfirst = TRUE_;
L50:
	ill = il;
	getsym_(lbuf, ibuf, &c__1, &ic, &il, &lfix);
	if ((itype == 1 || itype == 2 && iprd > 13 && iprd < 18) && count > 2)
		 {
	    lprd = TRUE_;
	}
	if ((itype == 3 || itype == 2 && (iprd == 13 || iprd == 18)) && count 
		> 4) {
	    lprd = TRUE_;
	}

	if (iot != a && iot != c__ && count > 8) {
	    lprd = TRUE_;
	}
	if (! lprd) {
	    goto L69;
	}
	if (il != ftest_1.ifunct && il != ftest_1.idesc) {
	    ill = il;
	}
	--iprod;
	goto L118;
L69:
	if (lfix) {
	    goto L59;
	}
	match_(&imatch, ibuf, isym);
/*               CHECK FOR INVALID INPUT PIN */
	if (itype == 1 && (imatch >= 12 && imatch <= 19)) {
	    linp = TRUE_;
	}
	if (itype == 2 && (imatch >= 13 && imatch <= 18)) {
	    linp = TRUE_;
	}
	if (itype == 3 && (imatch >= 14 && imatch <= 17)) {
	    linp = TRUE_;
	}
	if (itype == 4 && (imatch == 15 || imatch == 16)) {
	    linp = TRUE_;
	}
	if (itype == 5 && (imatch == 12 || imatch == 19)) {
	    linp = TRUE_;
	}
	if (itype == 6 && (imatch == 1 || imatch == 11)) {
	    linp = TRUE_;
	}
	ill = il;
	if (linp) {
	    goto L100;
	}
	if (imatch == 0) {
	    goto L100;
	}
	if (imatch == 10 || imatch == 99) {
	    goto L64;
	}
	if (! lfirst) {
	    goto L58;
	}
	lfirst = FALSE_;
	for (i__ = 1; i__ <= 32; ++i__) {
	    ++iblow;
/* L56: */
	    lfuses_1.lfuses[i__ + (iprod << 5) - 33] = TRUE_;
	}
L58:
	ixlate_(&iinput, &imatch, lphase, lbuf, &itype);
	if (iinput <= 0) {
	    goto L60;
	}
	--iblow;
	lfuses_1.lfuses[iinput + (iprod << 5) - 33] = FALSE_;
	plot_(lbuf, ibuf, lfuses_1.lfuses, &iprod, title, &c_false, &itype, 
		lprod, &iop, &iblow, &ipctr);
	goto L60;
L59:
	fixsym_(lbuf, ibuf, &ic, &il, &lfirst, lfuses_1.lfuses, &iblow, &
		iprod, &lfix);
L60:
	if (_BLNK__1.land) {
	    goto L50;
	}
L64:
	if (! _BLNK__1.lright) {
	    goto L68;
	}
L66:
	incr_(&ic, &il, &lfix);
	if (! _BLNK__1.lequal) {
	    goto L66;
	}
L68:
	if (! (_BLNK__1.lor || _BLNK__1.lequal)) {
	    goto L74;
	}
L70:
	;
    }
L74:
    ill = il;
    getsym_(lbuf, ibuf, &c__1, &ic, &il, &lfix);
    if (_BLNK__1.lleft || _BLNK__1.lequal) {
	goto L28;
    }
L100:
    if (ill == ftest_1.ifunct || ill == ftest_1.idesc) {
	goto L102;
    }
/*     PRINT AN ERROR MESSAGE IF UNRECOGNIZABLE SYMBOL */
    ilerr = ill + 4;
    io___81.ciunit = lunit_1.pms;
    s_wsfe(&io___81);
    do_fio(&c__1, (char *)&bel, (ftnlen)sizeof(integer));
    e_wsfe();
    io___82.ciunit = lunit_1.pms;
    s_wsfe(&io___82);
    for (i__ = 1; i__ <= 8; ++i__) {
	do_fio(&c__1, (char *)&ibuf[i__ - 1], (ftnlen)sizeof(integer));
    }
    do_fio(&c__1, (char *)&ilerr, (ftnlen)sizeof(integer));
    for (i__ = 1; i__ <= 80; ++i__) {
	do_fio(&c__1, (char *)&pge_1.ipage[i__ + ill * 80 - 81], (ftnlen)
		sizeof(integer));
    }
    e_wsfe();

/*     PRINT AN ERROR MESSAGE FOR ACTIVE HIGH/LOW PART */
    if (lact && lsame && ! loperr) {
	io___83.ciunit = lunit_1.pms;
	s_wsfe(&io___83);
	do_fio(&c__4, (char *)&ipal[0], (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&inoai, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&iot, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&inoo, (ftnlen)sizeof(integer));
	e_wsfe();
    }
    if (lact && ! lsame && ! loperr) {
	io___84.ciunit = lunit_1.pms;
	s_wsfe(&io___84);
	do_fio(&c__4, (char *)&ipal[0], (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&inoai, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&iot, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&inoo, (ftnlen)sizeof(integer));
	e_wsfe();
    }
/*     PRINT AN ERROR MESSAGE FOR AN INVALID OUTPUT PIN */
    if (loperr && imatch != 0) {
	io___85.ciunit = lunit_1.pms;
	s_wsfe(&io___85);
	do_fio(&c__1, (char *)&imatch, (ftnlen)sizeof(integer));
	do_fio(&c__4, (char *)&ipal[0], (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&inoai, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&iot, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&inoo, (ftnlen)sizeof(integer));
	e_wsfe();
    }
/*     PRINT AN ERROR MESSAGE FOR AN INVALID INPUT PIN */
    if (linp) {
	io___86.ciunit = lunit_1.pms;
	s_wsfe(&io___86);
	do_fio(&c__1, (char *)&imatch, (ftnlen)sizeof(integer));
	do_fio(&c__4, (char *)&ipal[0], (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&inoai, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&iot, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&inoo, (ftnlen)sizeof(integer));
	e_wsfe();
    }
/*     PRINT AN ERROR MESSAGE FOR INVALID PRODUCT LINE */
L118:
    ilerr = ill + 4;
    if (lprd) {
	io___87.ciunit = lunit_1.pms;
	s_wsfe(&io___87);
	for (i__ = 1; i__ <= 8; ++i__) {
	    do_fio(&c__1, (char *)&isym[i__ + (iprd << 3) - 9], (ftnlen)
		    sizeof(integer));
	}
	do_fio(&c__1, (char *)&iprd, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ilerr, (ftnlen)sizeof(integer));
	for (i__ = 1; i__ <= 80; ++i__) {
	    do_fio(&c__1, (char *)&pge_1.ipage[i__ + ill * 80 - 81], (ftnlen)
		    sizeof(integer));
	}
	e_wsfe();
    }
    if (lprd && count < 8) {
	io___88.ciunit = lunit_1.pms;
	s_wsfe(&io___88);
	do_fio(&c__1, (char *)&iprod, (ftnlen)sizeof(integer));
	do_fio(&c__4, (char *)&ipal[0], (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&inoai, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&iot, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&inoo, (ftnlen)sizeof(integer));
	e_wsfe();
    }
    if (lprd && count > 8) {
	io___89.ciunit = lunit_1.pms;
	s_wsfe(&io___89);
	do_fio(&c__4, (char *)&ipal[0], (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&inoai, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&iot, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&inoo, (ftnlen)sizeof(integer));
	e_wsfe();
    }
    exit_();
L102:
    if (itype <= 4) {
	tweek_(&itype, &iot, lfuses_1.lfuses, lfuses_1.lphant);
    }
L108:
    s_wsfe(&io___90);
    e_wsfe();
    s_wsfe(&io___91);
    e_wsfe();
    s_wsfe(&io___92);
    e_wsfe();
    io___93.ciunit = roc;
    s_rsfe(&io___93);
    do_fio(&c__1, (char *)&iop, (ftnlen)sizeof(integer));
    e_rsfe();
/*     CALL IODC2 */
/*     IF(POF.NE.6) WRITE(POF,125) */
/* 125 FORMAT('1') */
    if (iop == e) {
	echo_(ipal, &inoai, &iot, &inoo, rest, patnum, title, comp);
    }
    if (iop == o) {
	pinout_(ipal, &inoai, &iot, &inoo, title);
    }
    if (iop == t) {
	L__1 = iop != jj;
	test_(lphase, lbuf, title, &ic, &il, &ile, isym, ibuf, &itype, &inoo, 
		&lfix, &ipctr, &lerr, &isaf, &ipctr1, &c_false, &c_false, &
		L__1);
    }
/*     THE FOLLOWING IS ADDED FOR SA1 TEST */

/*     INITIALIZING THE TOTAL FAULTS. CALLING FOR SA1/SA0 TEST */
    isaf = 0;
    if (iop == f) {
	goto L200;
    }
/*     END OF ADDITION */

/*        ADDITIONS MADE  TO GENERATE TEST VECTORS */
/*        FOR JEDEC FORMAT */

    if (iop != jj) {
	goto L135;
    }
    L__1 = iop != jj;
    test_(lphase, lbuf, title, &ic, &il, &ile, isym, ibuf, &itype, &inoo, &
	    lfix, &ipctr, &lerr, &isaf, &ipctr1, &c_false, &c_false, &L__1);
    plotf_(&itype, &iot);



L135:
    if (iop == p) {
	plot_(lbuf, ibuf, lfuses_1.lfuses, &iprod, title, &c_true, &itype, 
		lprod, &iop, &iblow, &ipctr0);
    }
    if (iop == b) {
	plot_(lbuf, ibuf, lfuses_1.lfuses, &iprod, title, &c_true, &itype, 
		lprod, &iop, &iblow, &ipctr0);
    }
    if (iop == h__) {
	hex_(lfuses_1.lfuses, &h__);
    }
    if (iop == s) {
	hex_(lfuses_1.lfuses, &s);
    }
    if (iop == l) {
	binr_(lfuses_1.lfuses, &h__, &l);
    }
    if (iop == n) {
	binr_(lfuses_1.lfuses, &p, &n);
    }
    if (iop == c__) {
	cat_();
    }
/*     CALL IODC4 */



    if (g == ii) {
	intel_(lfuses_1.lfuses, &ii);
    }
    if (iop != q) {
	goto L108;
    }
    exit_();
/*     ADDITION FOR SA1/SA0 TESTS */
/*     SETTING THE PARAMETERS FOR SA1/SA0 TESTS */
L200:
    ipctr = 0;
    L__1 = iop != jj;
    test_(lphase, lbuf, title, &ic, &il, &ile, isym, ibuf, &itype, &inoo, &
	    ifix, &ipctr, &lerr, &isaf, &ipctr1, &c_false, &c_false, &L__1);
    if (ftest_1.ifunct == 0) {
	goto L135;
    }
    ipctr0 = ipctr;
/*     LOOPING FOR SA1 TEST */
    i__1 = ipctr0;
    for (ipctr1 = 1; ipctr1 <= i__1; ++ipctr1) {
	lsa11 = TRUE_;
	L__1 = iop != jj;
	test_(lphase, lbuf, title, &ic, &il, &ile, isym, ibuf, &itype, &inoo, 
		&ifix, &ipctr, &lerr, &isaf, &ipctr1, &lsa11, &c_false, &L__1)
		;
/* L210: */
    }
    isa1 = isaf;
/*     LOOPING FOR SA0 TEST */
    i__1 = ipctr0;
    for (ipctr1 = 1; ipctr1 <= i__1; ++ipctr1) {
	lsa01 = TRUE_;
	L__1 = iop != jj;
	test_(lphase, lbuf, title, &ic, &il, &ile, isym, ibuf, &itype, &inoo, 
		&ifix, &ipctr, &lerr, &isaf, &ipctr1, &c_false, &lsa01, &L__1)
		;
/* L215: */
    }
    isa0 = isaf - isa1;
    ifault = isaf * 100 / (ipctr0 << 1);
    io___105.ciunit = lunit_1.pof;
    s_wsfe(&io___105);
    do_fio(&c__1, (char *)&isa1, (ftnlen)sizeof(integer));
    e_wsfe();

    io___106.ciunit = lunit_1.pof;
    s_wsfe(&io___106);
    do_fio(&c__1, (char *)&isa0, (ftnlen)sizeof(integer));
    e_wsfe();
    io___107.ciunit = lunit_1.pof;
    s_wsfe(&io___107);
    do_fio(&c__1, (char *)&ifault, (ftnlen)sizeof(integer));
    e_wsfe();
    goto L135;
/*     END OF ADDITION FOR SA1/SA0 TEST */
} /* MAIN__ */

#undef n
#undef l
#undef s
#undef h__
#undef b
#undef p
#undef uu
#undef t
#undef tt
#undef ss
#undef rr
#undef pp
#undef oo
#undef nn
#undef ii
#undef ff
#undef ee
#undef dd
#undef o
#undef cc
#undef bb
#undef jj
#undef a
#undef f
#undef q
#undef c__
#undef e


/* ************************************************** */


integer iconv_(integer *k)
{
    /* Initialized data */

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_151 = { "0   ", 0 };

#define a (*(integer *)&equiv_151)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_152 = { "1   ", 0 };

#define b (*(integer *)&equiv_152)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_153 = { "2   ", 0 };

#define c__ (*(integer *)&equiv_153)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_154 = { "3   ", 0 };

#define d__ (*(integer *)&equiv_154)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_155 = { "4   ", 0 };

#define e (*(integer *)&equiv_155)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_156 = { "5   ", 0 };

#define f (*(integer *)&equiv_156)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_157 = { "6   ", 0 };

#define g (*(integer *)&equiv_157)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_158 = { "7   ", 0 };

#define h__ (*(integer *)&equiv_158)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_159 = { "8   ", 0 };

#define i__ (*(integer *)&equiv_159)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_160 = { "9   ", 0 };

#define j (*(integer *)&equiv_160)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_161 = { "A   ", 0 };

#define x (*(integer *)&equiv_161)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_162 = { "B   ", 0 };

#define l (*(integer *)&equiv_162)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_163 = { "C   ", 0 };

#define m (*(integer *)&equiv_163)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_164 = { "D   ", 0 };

#define n (*(integer *)&equiv_164)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_165 = { "E   ", 0 };

#define o (*(integer *)&equiv_165)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_166 = { "F   ", 0 };

#define p (*(integer *)&equiv_166)


    /* System generated locals */
    integer ret_val;

/*      INTEGER A,B,C,D,E,F,G,H */
/*     1,I,J,X,L,M,N,O,P */
    if (*k == 0) {
	ret_val = a;
    }
    if (*k == 1) {
	ret_val = b;
    }
    if (*k == 2) {
	ret_val = c__;
    }
    if (*k == 3) {
	ret_val = d__;
    }
    if (*k == 4) {
	ret_val = e;
    }
    if (*k == 5) {
	ret_val = f;
    }
    if (*k == 6) {
	ret_val = g;
    }
    if (*k == 7) {
	ret_val = h__;
    }
    if (*k == 8) {
	ret_val = i__;
    }
    if (*k == 9) {
	ret_val = j;
    }
    if (*k == 10) {
	ret_val = x;
    }
    if (*k == 11) {
	ret_val = l;
    }
    if (*k == 12) {
	ret_val = m;
    }
    if (*k == 13) {
	ret_val = n;
    }
    if (*k == 14) {
	ret_val = o;
    }
    if (*k == 15) {
	ret_val = p;
    }
    return ret_val;
} /* iconv_ */

#undef p
#undef o
#undef n
#undef m
#undef l
#undef x
#undef j
#undef i__
#undef h__
#undef g
#undef f
#undef e
#undef d__
#undef c__
#undef b
#undef a




/* *********************************************************************** */

/* Subroutine */ int initlz_(integer *inoai, integer *iot, integer *inoo, 
	integer *itype, logical *lfuses, logical *lphant, integer *ic, 
	integer *il, integer *iblow, logical *lfix, integer *ipctr)
{
    /* Initialized data */

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_180 = { "H   ", 0 };

#define h__ (*(integer *)&equiv_180)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_181 = { "L   ", 0 };

#define l (*(integer *)&equiv_181)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_182 = { "C   ", 0 };

#define c__ (*(integer *)&equiv_182)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_183 = { "R   ", 0 };

#define r__ (*(integer *)&equiv_183)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_184 = { "X   ", 0 };

#define x (*(integer *)&equiv_184)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_185 = { "A   ", 0 };

#define a (*(integer *)&equiv_185)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_186 = { "0   ", 0 };

#define i0 (*(integer *)&equiv_186)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_187 = { "2   ", 0 };

#define i2 (*(integer *)&equiv_187)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_188 = { "4   ", 0 };

#define i4 (*(integer *)&equiv_188)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_189 = { "6   ", 0 };

#define i6 (*(integer *)&equiv_189)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_190 = { "8   ", 0 };

#define i8 (*(integer *)&equiv_190)


    static integer i__, j;
    extern /* Subroutine */ int incr_(integer *, integer *, logical *);

/*     THIS SUBROUTINE INITIALIZES VARIABLES AND MATCHES PAL PART */
/*     NUMBER WITH ITYPE */
    /* Parameter adjustments */
    lphant -= 33;
    lfuses -= 33;

    /* Function Body */
/*     INITIALIZE LFUSES ARRAY (FUSE ARRAY) */

    for (j = 1; j <= 64; ++j) {
	for (i__ = 1; i__ <= 32; ++i__) {
	    lfuses[i__ + (j << 5)] = FALSE_;
/* L20: */
	    lphant[i__ + (j << 5)] = FALSE_;
	}
    }
/*        INITIALIZE NUMBER OF TEST VECTORS FOR JEDEC FORMAT */
    tstvec_1.ntest = 0;
/*     INITIALIZE IBLOW (NUMBER OF FUSES BLOWN) */
    *iblow = 0;
/*     INITIALIZE IPCTR (NUMBER OF PRODUCT TERMS) */
    *ipctr = 0;
/*     INITIALIZE IC AND IL (COLUMN AND LINE POINTERS) */
    *ic = 0;
    *il = 1;
/*     INITIALIZE ITYPE (PAL PART TYPE) */
    *itype = 0;
/*     ITYPE IS ASSIGNED THE FOLLOWING VALUES FOR THESE PAL TYPES: */
/*          PAL10H8,PAL10L8                          ITYPE=1 */
/*          PAL12H6,PAL12L6                          ITYPE=2 */
/*          PAL14H4,PAL14L4                          ITYPE=3 */
/*          PAL16H2,PAL16L2,PAL16C1                  ITYPE=4 */
/*          PAL16L8                                  ITYPE=5 */
/*          PAL16R4,PAL16R6,PAL16R8,PAL16X4,PAL16A4  ITYPE=6 */
/*     DETERMINE ITYPE */
    if (*inoai == i0) {
	*itype = 1;
    }
    if (*inoai == i2) {
	*itype = 2;
    }
    if (*inoai == i4) {
	*itype = 3;
    }
    if (*inoai == i6) {
	*itype = 4;
    }
    if (*inoai == i6 && *inoo == i8) {
	*itype = 5;
    }
    if (*iot == r__ || *iot == x || *iot == a) {
	*itype = 6;
    }
    if (! (*iot == h__ || *iot == l || *iot == c__ || *iot == r__ || *iot == 
	    x || *iot == a)) {
	*itype = 0;
    }
    incr_(ic, il, lfix);
    return 0;
} /* initlz_ */

#undef i8
#undef i6
#undef i4
#undef i2
#undef i0
#undef a
#undef x
#undef r__
#undef c__
#undef l
#undef h__



/* *********************************************************************** */

/* Subroutine */ int getsym_(logical *lphase, integer *isym, integer *j, 
	integer *ic, integer *il, logical *lfix)
{
    /* Initialized data */

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_193 = { "    ", 0 };

#define iblank (*(integer *)&equiv_193)


    static integer i__;
    extern /* Subroutine */ int incr_(integer *, integer *, logical *);

/*     THIS SUBROUTINE GETS THE PIN NAME, / IF COMPLEMENT LOGIC, AND */
/*      THE FOLLOWING OPERATION SYMBOL IF ANY */
    /* Parameter adjustments */
    isym -= 9;
    --lphase;

    /* Function Body */
    *lfix = FALSE_;
    if (! (_BLNK__1.lleft || _BLNK__1.land || _BLNK__1.lor || _BLNK__1.lequal 
	    || _BLNK__1.lright)) {
	goto L10;
    }
    incr_(ic, il, lfix);
    if (_BLNK__1.lleft) {
	goto L60;
    }
L10:
    lphase[*j] = ! _BLNK__1.lslash;
    if (lphase[*j]) {
	goto L15;
    }
    incr_(ic, il, lfix);

L15:
    for (i__ = 1; i__ <= 8; ++i__) {
/* L20: */
	isym[i__ + (*j << 3)] = iblank;
    }
L25:
    for (i__ = 1; i__ <= 7; ++i__) {
/* L30: */
	isym[i__ + (*j << 3)] = isym[i__ + 1 + (*j << 3)];
    }
    isym[(*j << 3) + 8] = pge_1.ipage[*ic + *il * 80 - 81];
    incr_(ic, il, lfix);
    if (_BLNK__1.lleft || _BLNK__1.lblank || _BLNK__1.land || _BLNK__1.lor || 
	    _BLNK__1.lright || _BLNK__1.lequal) {
	return 0;
    }
    goto L25;
L60:
    *lfix = TRUE_;
    return 0;
} /* getsym_ */

#undef iblank



/* ********************************************************************** */

/* Subroutine */ int incr_(integer *ic, integer *il, logical *lfix)
{
    /* Initialized data */

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_206 = { "    ", 0 };

#define iblank (*(integer *)&equiv_206)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_207 = { "(   ", 0 };

#define ileft (*(integer *)&equiv_207)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_208 = { "*   ", 0 };

#define iand (*(integer *)&equiv_208)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_209 = { "+   ", 0 };

#define ior (*(integer *)&equiv_209)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_210 = { ";   ", 0 };

#define coment (*(integer *)&equiv_210)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_211 = { "/   ", 0 };

#define islash (*(integer *)&equiv_211)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_212 = { "=   ", 0 };

#define iequal (*(integer *)&equiv_212)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_213 = { ")   ", 0 };

#define iright (*(integer *)&equiv_213)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_214 = { ":   ", 0 };

#define icolon (*(integer *)&equiv_214)

    static integer tab = 9;

    /* Format strings */
    static char fmt_15[] = "(/,\002 SOURCE FILE EXCEEDS 200 LINES OR MISSIN"
	    "G\002,\002 DESCRIPTION OR FUNCTION TABLE KEY WORD\002)";

    /* Local variables */
    static logical lx1;
    extern /* Subroutine */ int exit_(void);

    /* Fortran I/O blocks */
    static cilist io___205 = { 0, 0, 0, fmt_15, 0 };


/*     THIS SUBROUTINE INCREMENTS COLUMN AND LINE POINTERS */
/*      BLANKS AND CHARACTERS AFTER ';' ARE IGNORED */
    _BLNK__1.lblank = FALSE_;
    _BLNK__1.lxor = FALSE_;
    _BLNK__1.lxnor = FALSE_;
    lx1 = FALSE_;
    _BLNK__1.lright = FALSE_;
L10:
    ++(*ic);
    if (*ic <= 79 && pge_1.ipage[*ic + *il * 80 - 81] != coment) {
	goto L30;
    }
    ++(*il);
    if (*il <= 200) {
	goto L20;
    }
    io___205.ciunit = lunit_1.pms;
    s_wsfe(&io___205);
    e_wsfe();
    exit_();
L20:
    *ic = 0;
    goto L10;
L30:
    if (pge_1.ipage[*ic + *il * 80 - 81] == icolon && *lfix) {
	return 0;
    }
    if (pge_1.ipage[*ic + *il * 80 - 81] != iblank && pge_1.ipage[*ic + *il * 
	    80 - 81] != tab) {
	goto L31;
    }
    _BLNK__1.lblank = TRUE_;
    goto L10;
L31:
    if (pge_1.ipage[*ic + *il * 80 - 81] != icolon) {
	goto L32;
    }
    if (_BLNK__1.lxor || _BLNK__1.lxnor) {
	goto L33;
    }
    lx1 = TRUE_;
    goto L10;
L33:
    if (_BLNK__1.lxor) {
	_BLNK__1.lor = TRUE_;
    }
    if (_BLNK__1.lxnor) {
	_BLNK__1.land = TRUE_;
    }
    return 0;
L32:
    if (! (lx1 && (pge_1.ipage[*ic + *il * 80 - 81] == ior || pge_1.ipage[*ic 
	    + *il * 80 - 81] == iand))) {
	goto L34;
    }

    if (pge_1.ipage[*ic + *il * 80 - 81] == ior) {
	_BLNK__1.lxor = TRUE_;
    }
    if (pge_1.ipage[*ic + *il * 80 - 81] == iand) {
	_BLNK__1.lxnor = TRUE_;
    }
    goto L10;
L34:
    _BLNK__1.lleft = pge_1.ipage[*ic + *il * 80 - 81] == ileft;
    _BLNK__1.land = pge_1.ipage[*ic + *il * 80 - 81] == iand;
    _BLNK__1.lor = pge_1.ipage[*ic + *il * 80 - 81] == ior;
    _BLNK__1.lslash = pge_1.ipage[*ic + *il * 80 - 81] == islash;
    _BLNK__1.lequal = pge_1.ipage[*ic + *il * 80 - 81] == iequal;
    _BLNK__1.lright = pge_1.ipage[*ic + *il * 80 - 81] == iright;
    return 0;
} /* incr_ */

#undef icolon
#undef iright
#undef iequal
#undef islash
#undef coment
#undef ior
#undef iand
#undef ileft
#undef iblank



/* *********************************************************************** */

/* Subroutine */ int match_(integer *imatch, integer *ibuf, integer *isym)
{
    /* Initialized data */

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_222 = { "C   ", 0 };

#define c__ (*(integer *)&equiv_222)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_223 = { "A   ", 0 };

#define a (*(integer *)&equiv_223)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_224 = { "R   ", 0 };

#define r__ (*(integer *)&equiv_224)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_225 = { "Y   ", 0 };

#define y (*(integer *)&equiv_225)


    static integer i__, j;
    static logical lmatch;

/*     THIS SUBROUTINE FINDS A MATCH BETWEEN THE PIN NAME IN THE EQUATION */
/*      AND THE PIN NAME IN THE PIN LIST OR FUNCTION TABLE PIN LIST */
    /* Parameter adjustments */
    isym -= 9;
    ibuf -= 9;

    /* Function Body */
    *imatch = 0;
    for (j = 1; j <= 20; ++j) {
	lmatch = TRUE_;
	for (i__ = 1; i__ <= 8; ++i__) {
/* L10: */
	    lmatch = lmatch && ibuf[i__ + 8] == isym[i__ + (j << 3)];
	}
	if (lmatch) {
	    *imatch = j;
	}
/* L20: */
    }
/*     MATCH CARRY WHICH IS FOUND IN THE PAL16A4 */
    if (ibuf[11] == c__ && ibuf[12] == a && ibuf[13] == r__ && ibuf[14] == 
	    r__ && ibuf[15] == y) {
	*imatch = 99;
    }
    return 0;
} /* match_ */

#undef y
#undef r__
#undef a
#undef c__



/* *********************************************************************** */

/* Subroutine */ int ixlate_(integer *iinput, integer *imatch, logical *
	lphase, logical *lbuf, integer *itype)
{
    /* Initialized data */

    static integer itable[120]	/* was [20][6] */ = { 3,1,5,9,13,17,21,25,29,
	    -10,31,-1,-1,-1,-1,-1,-1,-1,-1,-20,3,1,5,9,13,17,21,25,29,-10,31,
	    27,-1,-1,-1,-1,-1,-1,7,-20,3,1,5,9,13,17,21,25,29,-10,31,27,23,-1,
	    -1,-1,-1,11,7,-20,3,1,5,9,13,17,21,25,29,-10,31,27,23,19,-1,-1,15,
	    11,7,-20,3,1,5,9,13,17,21,25,29,-10,31,-1,27,23,19,15,11,7,-1,-20,
	    -1,1,5,9,13,17,21,25,29,-10,-1,31,27,23,19,15,11,7,3,-20 };

    static integer ibubl;

/*     THIS SUBROUTINE FINDS A MATCH BETWEEN THE INPUT PIN NUMBER AND */
/*      THE INPUT LINE NUMBER FOR A SPECIFIC PAL.  ADD 1 TO THE INPUT */
/*      LINE NUMBER IF THE PIN IS A COMPLEMENT */
    /* Parameter adjustments */
    --lbuf;
    --lphase;

    /* Function Body */
    *iinput = 0;
    ibubl = 0;
    if (lphase[*imatch] && ! lbuf[1] || ! lphase[*imatch] && lbuf[1]) {
	ibubl = 1;
    }
    if (itable[*imatch + *itype * 20 - 21] > 0) {
	*iinput = itable[*imatch + *itype * 20 - 21] + ibubl;
    }

    return 0;
} /* ixlate_ */


/* *********************************************************************** */

/* Subroutine */ int fixsym_(logical *lbuf, integer *ibuf, integer *ic, 
	integer *il, logical *lfirst, logical *lfuses, integer *iblow, 
	integer *iprod, logical *lfix)
{
    /* Initialized data */

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_256 = { "A   ", 0 };

#define a (*(integer *)&equiv_256)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_257 = { "B   ", 0 };

#define b (*(integer *)&equiv_257)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_258 = { "/   ", 0 };

#define islash (*(integer *)&equiv_258)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_259 = { "+   ", 0 };

#define ior (*(integer *)&equiv_259)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_260 = { "    ", 0 };

#define iblank (*(integer *)&equiv_260)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_261 = { ")   ", 0 };

#define iright (*(integer *)&equiv_261)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_262 = { "*   ", 0 };

#define iand (*(integer *)&equiv_262)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_263 = { "0   ", 0 };

#define n0 (*(integer *)&equiv_263)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_264 = { "1   ", 0 };

#define n1 (*(integer *)&equiv_264)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_265 = { "2   ", 0 };

#define n2 (*(integer *)&equiv_265)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_266 = { "3   ", 0 };

#define n3 (*(integer *)&equiv_266)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_267 = { ":   ", 0 };

#define icolon (*(integer *)&equiv_267)

    static struct {
	char e_1[280];
	integer e_2;
	} equiv_268 = { "    A   +   /   B           A   +   B              "
		"     A   /   A   +   /   B               /   B   A   :   +  "
		" :   B       A   *   /   B       /   A   +   B   A   :   *  "
		" :   B                   B           A   *   B              "
		" /   A   /   A   *   /   B       /   A   *   B   ", 0 };

#define table ((integer *)&equiv_268)


    static integer i__, j, ci, iop;
    extern /* Subroutine */ int incr_(integer *, integer *, logical *), plot_(
	    );
    static integer isum1, lprod, title, ipctr, itype, imatch;
    static logical lmatch;
    static integer fixbuf[8], iinput;

/*     THIS SUBROUTINE EVALUATES THE FIXED SYMBOLS FOUND IN THE */
/*      PAL16X4 AND PAL16A4 */
    /* Parameter adjustments */
    lfuses -= 33;
    ibuf -= 9;
    --lbuf;

    /* Function Body */
    iinput = 0;
    for (i__ = 1; i__ <= 8; ++i__) {
	ibuf[i__ + 8] = iblank;
/* L20: */
	fixbuf[i__ - 1] = iblank;
    }
L21:
    incr_(ic, il, lfix);
    ci = pge_1.ipage[*ic + *il * 80 - 81];
    if (ci == iright) {
	goto L40;
    }
    if (ci == n0) {
	iinput = 8;
    }
    if (ci == n1) {
	iinput = 12;
    }
    if (ci == n2) {
	iinput = 16;
    }
    if (ci == n3) {
	iinput = 20;
    }
    for (j = 1; j <= 7; ++j) {
/* L24: */
	ibuf[j + 8] = ibuf[j + 9];
    }
    ibuf[16] = ci;
    if (! (ci == a || ci == b || ci == islash || ci == ior || ci == iand || 
	    ci == icolon)) {
	goto L21;
    }
    for (i__ = 1; i__ <= 4; ++i__) {
/* L30: */
	fixbuf[i__ - 1] = fixbuf[i__];
    }
    fixbuf[4] = pge_1.ipage[*ic + *il * 80 - 81];
    goto L21;
L40:
    imatch = 0;
    for (j = 1; j <= 14; ++j) {
	lmatch = TRUE_;
	for (i__ = 1; i__ <= 5; ++i__) {
/* L50: */
	    lmatch = lmatch && fixbuf[i__ - 1] == table[i__ + j * 5 - 6];
	}
/* L60: */
	if (lmatch) {
	    imatch = j;
	}
    }
    if (imatch == 0) {
	goto L100;
    }
    if (! (*lfirst)) {
	goto L85;
    }
    *lfirst = FALSE_;
    for (i__ = 1; i__ <= 32; ++i__) {
	lfuses[i__ + (*iprod << 5)] = TRUE_;
/* L80: */
	++(*iblow);
    }

L85:
    for (i__ = 1; i__ <= 4; ++i__) {
	if (imatch - 7 <= 0) {
	    goto L90;
	}
	isum1 = iinput + i__;
	lfuses[isum1 + (*iprod << 5)] = FALSE_;
	--(*iblow);
	imatch += -8;
L90:
	imatch += imatch;
    }
    lbuf[1] = TRUE_;
    plot_(&lbuf[1], &ibuf[9], &lfuses[33], iprod, &title, &c_false, &itype, &
	    lprod, &iop, iblow, &ipctr);
L100:
    *lfix = FALSE_;
    incr_(ic, il, lfix);
    return 0;
} /* fixsym_ */

#undef table
#undef icolon
#undef n3
#undef n2
#undef n1
#undef n0
#undef iand
#undef iright
#undef iblank
#undef ior
#undef islash
#undef b
#undef a



/* *********************************************************************** */

/* Subroutine */ int echo_(integer *ipal, integer *inoai, integer *iot, 
	integer *inoo, integer *rest, integer *patnum, integer *title, 
	integer *comp)
{
    /* Initialized data */

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_275 = { "    ", 0 };

#define iblank (*(integer *)&equiv_275)


    /* Format strings */
    static char fmt_5[] = "(/,\002 \002,4a1,a1,a1,a1,73a1,/,\002 \002,80a1"
	    ",/,\002 \002,80a1,/,\002 \002,80a1)";
    static char fmt_15[] = "(\002 \002,80a1)";

    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer i__, ic, il;

    /* Fortran I/O blocks */
    static cilist io___270 = { 0, 0, 0, fmt_5, 0 };
    static cilist io___273 = { 0, 0, 0, fmt_15, 0 };


/*     THIS SUBROUTINE PRINTS THE PAL DESIGN SPECIFICATION INPUT FILE */
    /* Parameter adjustments */
    --comp;
    --title;
    --patnum;
    --rest;
    --ipal;

    /* Function Body */
    io___270.ciunit = lunit_1.pof;
    s_wsfe(&io___270);
    do_fio(&c__4, (char *)&ipal[1], (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&(*inoai), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&(*iot), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&(*inoo), (ftnlen)sizeof(integer));
    do_fio(&c__73, (char *)&rest[1], (ftnlen)sizeof(integer));
    do_fio(&c__80, (char *)&patnum[1], (ftnlen)sizeof(integer));
    do_fio(&c__80, (char *)&title[1], (ftnlen)sizeof(integer));
    do_fio(&c__80, (char *)&comp[1], (ftnlen)sizeof(integer));
    e_wsfe();
    i__1 = ftest_1.iend;
    for (il = 1; il <= i__1; ++il) {
	ic = 81;
L10:
	--ic;
	if (pge_1.ipage[ic + il * 80 - 81] == iblank && ic > 1) {
	    goto L10;
	}
	io___273.ciunit = lunit_1.pof;
	s_wsfe(&io___273);
	i__2 = ic;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    do_fio(&c__1, (char *)&pge_1.ipage[i__ + il * 80 - 81], (ftnlen)
		    sizeof(integer));
	}
	e_wsfe();
/* L20: */
    }
    return 0;
} /* echo_ */

#undef iblank



/* *********************************************************************** */

/* Subroutine */ int cat_(void)
{
    /* Format strings */
    static char fmt_10[] = "(/,\002  MONOLITHIC MEMORIES 20-PIN PALASM VERSI"
	    "ON 1.6C\002)";
    static char fmt_15[] = "(\002 (C) COPYRIGHT 1983 MONOLITHIC MEMORIES\002)"
	    ;
    static char fmt_20[] = "(/,\002    ECHO (E)     - PRINTS THE PAL DESIG"
	    "N\002,\002 SPECIFICATION\002,/,\002    PINOUT (O)   - PRINTS THE"
	    " PINOUT OF THE PAL\002,/,\002    SIMULATE (T) - EXERCISES THE FU"
	    "NCTION TABLE\002,\002 VECTORS IN THE LOGIC\002,/,\002           "
	    "       \002,\002 EQUATIONS AND GENERATES TEST VECTORS\002,/,\002"
	    "    PLOT (P)     - PRINTS THE ENTIRE FUSE PLOT\002)";
    static char fmt_30[] = "(\002    BRIEF (B)    - PRINTS ONLY THE USED PRO"
	    "DUCT LINES\002,\002 OF THE FUSE PLOT\002,/,\002                 "
	    "  PHANTOM\002,\002 FUSES ARE OMITTED\002,/,\002    SHORT (S)    "
	    "- GENERATES SHORT VERSION OF HEX PROGRAMMING FORMAT\002,/,\002  "
	    "  HEX (H)      - GENERATES HEX PROGRAMMING FORMAT\002,/,\002    "
	    "BHLF (L)     - GENERATES BHLF PROGRAMMING FORMAT\002,/,\002    B"
	    "NPF (N)     - GENERATES BNPF PROGRAMMING FORMAT\002,/,\002    CA"
	    "TALOG (C)  - PRINTS THE PALASM CATALOG\002,/,\002    QUIT (Q)   "
	    "  - EXIT PALASM\002,/,\002    FAULT (F)    - FAULT TESTING \002,"
	    "/,\002    JEDEC (J)    - JEDEC FORMAT FOR DATA I/O PROGRAMMER"
	    "\002,/,\002    INTEL (I)    - INTEL HEX PROGRAMMING FORMAT\002)";

    /* Fortran I/O blocks */
    static cilist io___276 = { 0, 0, 0, fmt_10, 0 };
    static cilist io___277 = { 0, 0, 0, fmt_15, 0 };
    static cilist io___278 = { 0, 0, 0, fmt_20, 0 };
    static cilist io___279 = { 0, 0, 0, fmt_30, 0 };


/*     THIS SUBROUTINE PRINTS THE PALASM CATOLOG */
    io___276.ciunit = lunit_1.pms;
    s_wsfe(&io___276);
    e_wsfe();
    io___277.ciunit = lunit_1.pms;
    s_wsfe(&io___277);
    e_wsfe();
    io___278.ciunit = lunit_1.pms;
    s_wsfe(&io___278);
    e_wsfe();

    io___279.ciunit = lunit_1.pms;
    s_wsfe(&io___279);
    e_wsfe();
    return 0;
} /* cat_ */


/* *********************************************************************** */

/* Subroutine */ int pinout_(integer *ipal, integer *inoai, integer *iot, 
	integer *inoo, integer *title)
{
    /* Initialized data */

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_297 = { "    ", 0 };

#define iblank (*(integer *)&equiv_297)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_298 = { "*   ", 0 };

#define istar (*(integer *)&equiv_298)


    /* Format strings */
    static char fmt_76[] = "(/,\002 \002,80a1)";
    static char fmt_78[] = "(/,\002 \002,18x,14a1,3x,14a1,/,\002 \002,18x,a1"
	    ",13x,a1,1x,a1,13x,a1)";
    static char fmt_80[] = "(\002 \002,15x,4a1,29x,4a1)";
    static char fmt_81[] = "(\002 \002,12a1,3x,a1,i2,a1,11x,7a1,11x,a1,i2,a1"
	    ",3x,12a1)";
    static char fmt_82[] = "(\002 \002,15x,4a1,29x,4a1)";
    static char fmt_84[] = "(\002 \002,18x,a1,11x,7a1,11x,a1)";
    static char fmt_90[] = "(\002 \002,18x,31a1)";

    /* Local variables */
    static integer i__, j, ic, ii, jj, il, iin[14]	/* was [7][2] */, pin[
	    240]	/* was [12][20] */;

    /* Fortran I/O blocks */
    static cilist io___289 = { 0, 0, 0, fmt_76, 0 };
    static cilist io___290 = { 0, 0, 0, fmt_78, 0 };
    static cilist io___292 = { 0, 0, 0, fmt_80, 0 };
    static cilist io___293 = { 0, 0, 0, fmt_81, 0 };
    static cilist io___294 = { 0, 0, 0, fmt_82, 0 };
    static cilist io___295 = { 0, 0, 0, fmt_84, 0 };
    static cilist io___296 = { 0, 0, 0, fmt_90, 0 };


/*     THIS SUBROUTINE PRINTS THE PINOUT OF THE PAL */
    /* Parameter adjustments */
    --title;
    --ipal;

    /* Function Body */
    for (j = 1; j <= 20; ++j) {
	for (i__ = 1; i__ <= 12; ++i__) {
/* L5: */
	    pin[i__ + j * 12 - 13] = iblank;
	}
/* L10: */
    }
/* L15: */
    for (j = 1; j <= 2; ++j) {
	for (i__ = 1; i__ <= 7; ++i__) {
/* L20: */
	    iin[i__ + j * 7 - 8] = iblank;
	}
/* L25: */
    }
    iin[1] = ipal[1];
    iin[3] = ipal[2];
    iin[5] = ipal[3];
    iin[7] = ipal[4];
    iin[9] = *inoai;
    iin[11] = *iot;
    iin[13] = *inoo;
    j = 0;
    il = 0;
L30:
    ic = 0;
    ++il;
L35:
    ++ic;
L40:
    if (ic > 80) {
	goto L30;
    }
    if (pge_1.ipage[ic + il * 80 - 81] == iblank) {
	goto L35;
    }
    ++j;
    if (j > 20) {
	goto L60;
    }
    for (i__ = 1; i__ <= 12; ++i__) {
	pin[i__ + j * 12 - 13] = pge_1.ipage[ic + il * 80 - 81];
	++ic;
	if (ic > 80) {
	    goto L40;
	}
	if (pge_1.ipage[ic + il * 80 - 81] == iblank) {
	    goto L40;
	}

/* L55: */
    }
L60:
    for (j = 1; j <= 10; ++j) {
	ii = 0;
L65:
	++ii;
	if (ii == 13) {
	    goto L75;
	}
	if (pin[ii + j * 12 - 13] != iblank) {
	    goto L65;
	}
	i__ = 13;
L70:
	--i__;
	--ii;
	pin[i__ + j * 12 - 13] = pin[ii + j * 12 - 13];
	pin[ii + j * 12 - 13] = iblank;
	if (ii != 1) {
	    goto L70;
	}
L75:
	;
    }
    io___289.ciunit = lunit_1.pof;
    s_wsfe(&io___289);
    do_fio(&c__80, (char *)&title[1], (ftnlen)sizeof(integer));
    e_wsfe();
    io___290.ciunit = lunit_1.pof;
    s_wsfe(&io___290);
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    e_wsfe();
    jj = 20;
    for (j = 1; j <= 10; ++j) {
	io___292.ciunit = lunit_1.pof;
	s_wsfe(&io___292);
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	e_wsfe();
	io___293.ciunit = lunit_1.pof;
	s_wsfe(&io___293);
	for (i__ = 1; i__ <= 12; ++i__) {
	    do_fio(&c__1, (char *)&pin[i__ + j * 12 - 13], (ftnlen)sizeof(
		    integer));
	}
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&j, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	for (i__ = 1; i__ <= 7; ++i__) {
	    do_fio(&c__1, (char *)&iin[i__ - 1], (ftnlen)sizeof(integer));
	}
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&jj, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	for (i__ = 1; i__ <= 12; ++i__) {
	    do_fio(&c__1, (char *)&pin[i__ + jj * 12 - 13], (ftnlen)sizeof(
		    integer));
	}
	e_wsfe();
	io___294.ciunit = lunit_1.pof;
	s_wsfe(&io___294);
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	e_wsfe();
	io___295.ciunit = lunit_1.pof;
	s_wsfe(&io___295);
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	for (i__ = 1; i__ <= 7; ++i__) {
	    do_fio(&c__1, (char *)&iin[i__ + 6], (ftnlen)sizeof(integer));
	}
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	e_wsfe();
	for (ii = 1; ii <= 2; ++ii) {
	    for (i__ = 1; i__ <= 7; ++i__) {
/* L85: */
		iin[i__ + ii * 7 - 8] = iblank;
	    }
/* L86: */
	}
	--jj;
/* L88: */
    }
    io___296.ciunit = lunit_1.pof;
    s_wsfe(&io___296);
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
    e_wsfe();
    return 0;
} /* pinout_ */

#undef istar
#undef iblank



/* *********************************************************************** */

/* Subroutine */ int plot_(logical *lbuf, integer *ibuf, logical *lfuses, 
	integer *iprod, integer *title, logical *ldump, integer *itype, 
	logical *lprod, integer *iop, integer *iblow, integer *ipctr0)
{
    /* Initialized data */

    static struct {
	char e_1[8192];
	integer e_2;
	} equiv_320 = { "                                                   "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                                            "
		"                                         ", 0 };

#define isave ((integer *)&equiv_320)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_321 = { "*   ", 0 };

#define iand (*(integer *)&equiv_321)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_322 = { "/   ", 0 };

#define islash (*(integer *)&equiv_322)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_323 = { "-   ", 0 };

#define idash (*(integer *)&equiv_323)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_324 = { "X   ", 0 };

#define x (*(integer *)&equiv_324)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_325 = { "    ", 0 };

#define iblank (*(integer *)&equiv_325)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_326 = { "P   ", 0 };

#define p (*(integer *)&equiv_326)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_327 = { "B   ", 0 };

#define b (*(integer *)&equiv_327)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_328 = { "O   ", 0 };

#define hifant (*(integer *)&equiv_328)


    /* Format strings */
    static char fmt_62[] = "(/,\002 \002,80a1,//,\002                11 1111"
	    " 1111 2222 2222 2233\002,/,\002    0123 4567 8901 2345 6789 0123"
	    " 4567 8901\002,/)";
    static char fmt_90[] = "(\002 \002,i2,8(\002 \002,4a1),\002 \002,32a1)";
    static char fmt_96[] = "(1x)";
    static char fmt_110[] = "(/,\002 LEGEND:  X : FUSE NOT BLOWN (L,N,0)   -"
	    " : FUSE BLOWN   (H,P,1)\002)";
    static char fmt_111[] = "(\002          0 : PHANTOM FUSE   (L,N,0)   O :"
	    " PHANTOM FUSE (H,P,1)\002)";
    static char fmt_112[] = "(/,\002 NUMBER OF FUSES BLOWN = \002,i4)";

    /* Local variables */
    static integer i__, j, iout[64], i8pro, i88pro;
    extern /* Subroutine */ int fantom_(integer *, integer *, integer *, 
	    integer *);

    /* Fortran I/O blocks */
    static cilist io___311 = { 0, 0, 0, fmt_62, 0 };
    static cilist io___315 = { 0, 0, 0, fmt_90, 0 };
    static cilist io___316 = { 0, 0, 0, fmt_96, 0 };
    static cilist io___317 = { 0, 0, 0, fmt_110, 0 };
    static cilist io___318 = { 0, 0, 0, fmt_111, 0 };
    static cilist io___319 = { 0, 0, 0, fmt_112, 0 };


/*     THIS THIS SUBROUTINE PRODUCES THE FUSE PLOT */

    /* Parameter adjustments */
    --lprod;
    --title;
    lfuses -= 33;
    ibuf -= 9;
    --lbuf;

    /* Function Body */
    if (*ldump) {
	goto L60;
    }
    if (isave[*iprod - 1] != iblank) {
	return 0;
    }
    if (lbuf[1]) {
	goto L5;
    }
    for (j = 1; j <= 31; ++j) {
/* L30: */
	isave[*iprod + (j << 6) - 65] = isave[*iprod + (j + 1 << 6) - 65];
    }
    isave[*iprod + 1983] = islash;
L5:
    for (i__ = 1; i__ <= 8; ++i__) {
	if (isave[*iprod - 1] != iblank) {
	    return 0;
	}
	if (ibuf[i__ + 8] == iblank) {
	    goto L20;
	}
	for (j = 1; j <= 31; ++j) {
/* L10: */
	    isave[*iprod + (j << 6) - 65] = isave[*iprod + (j + 1 << 6) - 65];
	}
	isave[*iprod + 1983] = ibuf[i__ + 8];
L20:
	;
    }
    if (isave[*iprod - 1] != iblank) {
	return 0;
    }
/* L40: */
    for (j = 1; j <= 31; ++j) {
/* L50: */
	isave[*iprod + (j << 6) - 65] = isave[*iprod + (j + 1 << 6) - 65];
    }
    isave[*iprod + 1983] = iand;
    return 0;
/*     PRINT FUSE PLOT */
L60:
    io___311.ciunit = lunit_1.pof;
    s_wsfe(&io___311);
    do_fio(&c__80, (char *)&title[1], (ftnlen)sizeof(integer));
    e_wsfe();
    for (i88pro = 1; i88pro <= 57; i88pro += 8) {
	for (i8pro = 1; i8pro <= 8; ++i8pro) {
	    *iprod = i88pro + i8pro - 1;
	    isave[*iprod + 1983] = iblank;
	    for (i__ = 1; i__ <= 32; ++i__) {
		if (isave[*iprod - 1] != iblank) {
		    goto L70;
		}
		for (j = 1; j <= 31; ++j) {
		    isave[*iprod + (j << 6) - 65] = isave[*iprod + (j + 1 << 
			    6) - 65];
/* L65: */
		}
		isave[*iprod + 1983] = iblank;
L70:
		;
	    }
	    for (i__ = 1; i__ <= 32; ++i__) {
		iout[i__ - 1] = x;
		if (lfuses[i__ + (*iprod << 5)]) {
		    iout[i__ - 1] = idash;
		}
		iout[i__ + 31] = isave[*iprod + (i__ << 6) - 65];
/* L80: */
	    }
	    if (*itype <= 4) {
		fantom_(itype, iout, iprod, &i8pro);
	    }
	    --(*iprod);
	    for (j = 1; j <= 32; ++j) {
		if (*iop == b && iout[j - 1] == hifant) {
		    iout[j - 1] = iblank;
		}
/* L85: */
	    }
	    if (*iop == p || *iop == b && lprod[*iprod + 1]) {
		io___315.ciunit = lunit_1.pof;
		s_wsfe(&io___315);
		do_fio(&c__1, (char *)&(*iprod), (ftnlen)sizeof(integer));
		do_fio(&c__64, (char *)&iout[0], (ftnlen)sizeof(integer));
		e_wsfe();
	    }
/* L94: */
	}
	io___316.ciunit = lunit_1.pof;
	s_wsfe(&io___316);
	e_wsfe();

/* L100: */
    }
    io___317.ciunit = lunit_1.pof;
    s_wsfe(&io___317);
    e_wsfe();
    if (*iop == p && *itype <= 4) {
	io___318.ciunit = lunit_1.pof;
	s_wsfe(&io___318);
	e_wsfe();
    }
    io___319.ciunit = lunit_1.pof;
    s_wsfe(&io___319);
    do_fio(&c__1, (char *)&(*iblow), (ftnlen)sizeof(integer));
    e_wsfe();
    return 0;
} /* plot_ */

#undef hifant
#undef b
#undef p
#undef iblank
#undef x
#undef idash
#undef islash
#undef iand
#undef isave



/* *********************************************************************** */

/* Subroutine */ int hex_(logical *lfuses, integer *iop)
{
    /* Initialized data */

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_356 = { "H   ", 0 };

#define h__ (*(integer *)&equiv_356)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_357 = { "S   ", 0 };

#define s (*(integer *)&equiv_357)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_358 = { "    ", 0 };

#define iblank (*(integer *)&equiv_358)

    static struct {
	char e_1[64];
	integer e_2;
	} equiv_359 = { "0   1   2   3   4   5   6   7   8   9   A   B   C  "
		" D   E   F   ", 0 };

#define ztable ((integer *)&equiv_359)

    static integer soh = 1;
    static integer stx = 2;
    static integer etx = 3;
    static integer bel = 7;

    /* Format strings */
    static char fmt_10[] = "(//,\002                                        "
	    " .\002,//)";
    static char fmt_5[] = "(\002 \002,9a1)";
    static char fmt_30[] = "(\002 \002,32(a1,\002 \002),\002.\002,/,\002 "
	    "\002,32(a1,\002 \002),\002.\002)";
    static char fmt_50[] = "(\002 \002,64a1)";
    static char fmt_70[] = "(\002 \002,//,\002                              "
	    "         .\002,//)";
    static char fmt_80[] = "(\002 \002,a1)";
    static char fmt_90[] = "(/\002 HEX CHECK SUM = \002,4a1)";

    /* Local variables */
    static integer i__, j, inc, ihex, csum, isum2, iprod, itemp[64], ztemp, 
	    zcsum[4], isumx, iinput;

    /* Fortran I/O blocks */
    static cilist io___338 = { 0, 0, 0, fmt_10, 0 };
    static cilist io___339 = { 0, 0, 0, fmt_5, 0 };
    static cilist io___349 = { 0, 0, 0, fmt_30, 0 };
    static cilist io___350 = { 0, 0, 0, fmt_50, 0 };
    static cilist io___351 = { 0, 0, 0, fmt_70, 0 };
    static cilist io___352 = { 0, 0, 0, fmt_80, 0 };
    static cilist io___355 = { 0, 0, 0, fmt_90, 0 };


/*     THIS SUBROUTINE GENERATES HEX PROGRAMMING FORMATS */
    /* Parameter adjustments */
    lfuses -= 33;

    /* Function Body */
    csum = 0;
    if (*iop == h__) {
	io___338.ciunit = lunit_1.pdf;
	s_wsfe(&io___338);
	e_wsfe();
    }
/* ***** NOTE: SOME PROM PROGRAMMERS NEED A START CHARACTER. */
/* *****       THIS PROGRAM OUTPUTS AN STX FOR THE DATA I/O MODEL 9 */
/* *****         (USE SOH FOR MODEL 5) */
    io___339.ciunit = lunit_1.pdf;
    s_wsfe(&io___339);
    do_fio(&c__1, (char *)&bel, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&bel, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&bel, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&bel, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&bel, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&bel, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&bel, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&stx, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&soh, (ftnlen)sizeof(integer));
    e_wsfe();
    for (i__ = 1; i__ <= 33; i__ += 32) {
	inc = i__ - 1;
	for (iprod = 1; iprod <= 7; iprod += 2) {
	    for (j = 1; j <= 2; ++j) {
		for (iinput = 1; iinput <= 32; ++iinput) {
		    ihex = 0;
		    isum2 = iprod + j - 1 + inc;
		    if (lfuses[iinput + (isum2 << 5)]) {
			++ihex;
		    }
		    if (lfuses[iinput + (isum2 + 8 << 5)]) {
			ihex += 2;
		    }
		    if (lfuses[iinput + (isum2 + 16 << 5)]) {
			ihex += 4;
		    }
		    if (lfuses[iinput + (isum2 + 24 << 5)]) {
			ihex += 8;
		    }
		    csum += ihex;
		    isumx = iinput + (j - 1 << 5);
/* L20: */
		    itemp[isumx - 1] = ztable[ihex];
		}
	    }
	    if (*iop == h__) {
		io___349.ciunit = lunit_1.pdf;
		s_wsfe(&io___349);
		do_fio(&c__64, (char *)&itemp[0], (ftnlen)sizeof(integer));
		e_wsfe();
	    }
/* L40: */
	    if (*iop == s) {
		io___350.ciunit = lunit_1.pdf;
		s_wsfe(&io___350);
		do_fio(&c__64, (char *)&itemp[0], (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	}
    }
    if (*iop == h__) {
	io___351.ciunit = lunit_1.pdf;
	s_wsfe(&io___351);
	e_wsfe();
    }

    io___352.ciunit = lunit_1.pdf;
    s_wsfe(&io___352);
    do_fio(&c__1, (char *)&etx, (ftnlen)sizeof(integer));
    e_wsfe();
/*     CONVERT DECIMAL CHECK SUM INTO HEX CHECK SUM */
    for (i__ = 1; i__ <= 4; ++i__) {
	ztemp = csum - (csum / 16 << 4);
	zcsum[5 - i__ - 1] = ztable[ztemp];
	csum /= 16;
/* L85: */
    }
    if (zcsum[0] == ztable[0]) {
	zcsum[0] = iblank;
    }
    io___355.ciunit = lunit_1.pms;
    s_wsfe(&io___355);
    do_fio(&c__1, (char *)&zcsum[0], (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&zcsum[1], (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&zcsum[2], (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&zcsum[3], (ftnlen)sizeof(integer));
    e_wsfe();
    return 0;
} /* hex_ */

#undef ztable
#undef iblank
#undef s
#undef h__



/* *********************************************************************** */


/* Subroutine */ int tweek_(integer *itype, integer *iot, logical *lfuses, 
	logical *lphant)
{
    /* Initialized data */

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_364 = { "L   ", 0 };

#define l (*(integer *)&equiv_364)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_365 = { "C   ", 0 };

#define c__ (*(integer *)&equiv_365)


    static integer iprod, iinput;

/*     THIS SUBROUTINE TWEEKS LFUSES (THE PROGRAMMING FUSE PLOT) */
/*      FOR HIGH AND LOW PHANTOM FUSES */
    /* Parameter adjustments */
    lphant -= 33;
    lfuses -= 33;

    /* Function Body */
    if (*itype >= 4) {
	goto L20;
    }
    for (iprod = 1; iprod <= 64; ++iprod) {
	lfuses[(iprod << 5) + 15] = TRUE_;
	lfuses[(iprod << 5) + 16] = TRUE_;
	lfuses[(iprod << 5) + 19] = TRUE_;
	lfuses[(iprod << 5) + 20] = TRUE_;
	lphant[(iprod << 5) + 15] = TRUE_;
	lphant[(iprod << 5) + 16] = TRUE_;
	lphant[(iprod << 5) + 19] = TRUE_;
	lphant[(iprod << 5) + 20] = TRUE_;
	if (*itype >= 3) {
	    goto L10;
	}
	lfuses[(iprod << 5) + 11] = TRUE_;
	lfuses[(iprod << 5) + 12] = TRUE_;
	lfuses[(iprod << 5) + 23] = TRUE_;
	lfuses[(iprod << 5) + 24] = TRUE_;
	lphant[(iprod << 5) + 11] = TRUE_;
	lphant[(iprod << 5) + 12] = TRUE_;
	lphant[(iprod << 5) + 23] = TRUE_;
	lphant[(iprod << 5) + 24] = TRUE_;
	if (*itype >= 2) {
	    goto L10;
	}
	lfuses[(iprod << 5) + 7] = TRUE_;
	lfuses[(iprod << 5) + 8] = TRUE_;
	lfuses[(iprod << 5) + 27] = TRUE_;
	lfuses[(iprod << 5) + 28] = TRUE_;
	lphant[(iprod << 5) + 7] = TRUE_;
	lphant[(iprod << 5) + 8] = TRUE_;
	lphant[(iprod << 5) + 27] = TRUE_;
	lphant[(iprod << 5) + 28] = TRUE_;
L10:
	;
    }
    for (iinput = 7; iinput <= 28; ++iinput) {

	for (iprod = 1; iprod <= 57; iprod += 8) {
	    lfuses[iinput + (iprod + 4 << 5)] = FALSE_;
	    lfuses[iinput + (iprod + 5 << 5)] = FALSE_;
	    lfuses[iinput + (iprod + 6 << 5)] = FALSE_;
	    lfuses[iinput + (iprod + 7 << 5)] = FALSE_;
	    lphant[iinput + (iprod + 4 << 5)] = TRUE_;
	    lphant[iinput + (iprod + 5 << 5)] = TRUE_;
	    lphant[iinput + (iprod + 6 << 5)] = TRUE_;
/* L12: */
	    lphant[iinput + (iprod + 7 << 5)] = TRUE_;
	}
	if (*itype >= 3) {
	    goto L18;
	}
	for (iprod = 17; iprod <= 41; iprod += 8) {
	    lfuses[iinput + (iprod + 2 << 5)] = FALSE_;
	    lfuses[iinput + (iprod + 3 << 5)] = FALSE_;
	    lphant[iinput + (iprod + 2 << 5)] = TRUE_;
/* L14: */
	    lphant[iinput + (iprod + 3 << 5)] = TRUE_;
	}
	if (*itype >= 2) {
	    goto L18;
	}
	for (iprod = 1; iprod <= 57; iprod += 8) {
	    lfuses[iinput + (iprod + 2 << 5)] = FALSE_;
	    lfuses[iinput + (iprod + 3 << 5)] = FALSE_;
	    lphant[iinput + (iprod + 2 << 5)] = TRUE_;
/* L16: */
	    lphant[iinput + (iprod + 3 << 5)] = TRUE_;
	}
L18:
	;
    }
L20:
    if (*itype == 1) {
	return 0;
    }
    for (iinput = 1; iinput <= 32; ++iinput) {
	for (iprod = 1; iprod <= 8; ++iprod) {
	    lfuses[iinput + (iprod << 5)] = *iot != l;
	    lphant[iinput + (iprod << 5)] = TRUE_;
	    if (*iot == c__) {
		goto L30;
	    }
	    lfuses[iinput + (iprod + 56 << 5)] = *iot != l;
	    lphant[iinput + (iprod + 56 << 5)] = TRUE_;
L30:
	    ;
	}
	if (*itype <= 2) {
	    goto L99;
	}
	for (iprod = 1; iprod <= 8; ++iprod) {
	    lfuses[iinput + (iprod + 8 << 5)] = *iot != l;
	    lphant[iinput + (iprod + 8 << 5)] = TRUE_;
	    if (*iot == c__) {
		goto L40;
	    }
	    lfuses[iinput + (iprod + 48 << 5)] = *iot != l;
	    lphant[iinput + (iprod + 48 << 5)] = TRUE_;
L40:
	    ;
	}
	if (*itype <= 3) {
	    goto L99;
	}
	for (iprod = 1; iprod <= 8; ++iprod) {
	    lfuses[iinput + (iprod + 16 << 5)] = *iot != l;
	    lphant[iinput + (iprod + 16 << 5)] = TRUE_;
	    if (*iot == c__) {
		goto L50;
	    }
	    lfuses[iinput + (iprod + 40 << 5)] = *iot != l;
	    lphant[iinput + (iprod + 40 << 5)] = TRUE_;
L50:
	    ;
	}
L99:
	;
    }
    return 0;
} /* tweek_ */

#undef c__
#undef l



/* *********************************************************************** */

/* Subroutine */ int binr_(logical *lfuses, integer *h__, integer *l)
{
    /* Format strings */
    static char fmt_10[] = "(//,\002                                        "
	    " .\002,//)";
    static char fmt_30[] = "(\002 \002,8(\002b\002,4a1,\002f \002))";

    /* Local variables */
    static integer i__, j, k, inc, isum3, iprod, itemp[32]	/* was [4][8] 
	    */, iinput;

    /* Fortran I/O blocks */
    static cilist io___366 = { 0, 0, 0, fmt_10, 0 };
    static cilist io___375 = { 0, 0, 0, fmt_30, 0 };
    static cilist io___376 = { 0, 0, 0, fmt_10, 0 };


/*     THIS SUBROUTINE GENERATES BINARY PROGRAMMING FORMATS */

    /* Parameter adjustments */
    lfuses -= 33;

    /* Function Body */
    io___366.ciunit = lunit_1.pdf;
    s_wsfe(&io___366);
    e_wsfe();
    for (i__ = 1; i__ <= 33; i__ += 32) {
	inc = i__ - 1;
	for (iprod = 1; iprod <= 8; ++iprod) {
	    for (j = 1; j <= 25; j += 8) {
		for (k = 1; k <= 8; ++k) {
		    iinput = j + k - 1;
		    itemp[(k << 2) - 4] = *l;
		    itemp[(k << 2) - 3] = *l;
		    itemp[(k << 2) - 2] = *l;
		    itemp[(k << 2) - 1] = *l;
		    isum3 = iprod + inc;
		    if (lfuses[iinput + (isum3 << 5)]) {
			itemp[(k << 2) - 1] = *h__;
		    }
		    if (lfuses[iinput + (isum3 + 8 << 5)]) {
			itemp[(k << 2) - 2] = *h__;
		    }
		    if (lfuses[iinput + (isum3 + 16 << 5)]) {
			itemp[(k << 2) - 3] = *h__;
		    }
		    if (lfuses[iinput + (isum3 + 24 << 5)]) {
			itemp[(k << 2) - 4] = *h__;
		    }
/* L15: */
		}
/* L20: */
		io___375.ciunit = lunit_1.pdf;
		s_wsfe(&io___375);
		do_fio(&c__32, (char *)&itemp[0], (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	}
    }
    io___376.ciunit = lunit_1.pdf;
    s_wsfe(&io___376);
    e_wsfe();
    return 0;
} /* binr_ */


/* *********************************************************************** */

/* Subroutine */ int slip_(logical *lfuses, integer *i88pro, integer *inoai, 
	integer *iot, integer *inoo, integer *iblow)
{
    /* Initialized data */

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_384 = { "R   ", 0 };

#define r__ (*(integer *)&equiv_384)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_385 = { "1   ", 0 };

#define i1 (*(integer *)&equiv_385)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_386 = { "2   ", 0 };

#define i2 (*(integer *)&equiv_386)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_387 = { "4   ", 0 };

#define i4 (*(integer *)&equiv_387)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_388 = { "6   ", 0 };

#define i6 (*(integer *)&equiv_388)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_389 = { "8   ", 0 };

#define i8 (*(integer *)&equiv_389)


    static integer i__;

/*     THIS SUBROUTINE WILL BLOW THE ENTIRE CONDITIONAL THREE-STATE */
/*      PRODUCT LINE WHEN 'IF(VCC)' CONDITION IS USED FOR THE */
/*      CORRESPONDING OUTPUT PIN */
    /* Parameter adjustments */
    lfuses -= 33;

    /* Function Body */
    if (*inoai != i6 || *inoo == i1 || *inoo == i2 || *iot == r__ && *inoo == 
	    i8 || *i88pro >= 9 && *i88pro <= 49 && *inoo == i6 || *i88pro >= 
	    17 && *i88pro <= 41 && *inoo == i4) {
	return 0;
    }
    for (i__ = 1; i__ <= 32; ++i__) {
	++(*iblow);
/* L10: */
	lfuses[i__ + (*i88pro << 5)] = TRUE_;
    }
    ++(*i88pro);
    return 0;
} /* slip_ */

#undef i8
#undef i6
#undef i4
#undef i2
#undef i1
#undef r__



/* *********************************************************************** */

/* Subroutine */ int fantom_(integer *itype, integer *iout, integer *iprod, 
	integer *i8pro)
{
    /* Initialized data */

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_395 = { "X   ", 0 };

#define x (*(integer *)&equiv_395)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_396 = { "-   ", 0 };

#define idash (*(integer *)&equiv_396)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_397 = { "0   ", 0 };

#define lofant (*(integer *)&equiv_397)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_398 = { "O   ", 0 };

#define hifant (*(integer *)&equiv_398)


    static integer i__;

/*     THIS SUBROUTINE UPDATES IOUT (THE PRINTED FUSE PLOT) */
/*      FOR HIGH AND LOW PHANTOM FUSES */

    /* Parameter adjustments */
    --iout;

    /* Function Body */
    for (i__ = 1; i__ <= 32; ++i__) {
	if (iout[i__] == idash) {
	    iout[i__] = hifant;
	}
	if (iout[i__] == x) {
	    iout[i__] = lofant;
	}
/* L10: */
    }
    if (*itype == 4 && (*iprod <= 24 || *iprod >= 41)) {
	return 0;
    }
    if (*itype == 3 && (*iprod <= 16 || *iprod >= 45)) {
	return 0;
    }
    if (*itype == 2 && (*iprod <= 8 || *iprod >= 53)) {
	return 0;
    }
    if (*itype <= 3 && *i8pro >= 5) {
	return 0;
    }
    if (*itype <= 2 && *iprod >= 19 && *iprod <= 48 && *i8pro >= 3) {
	return 0;
    }
    if (*itype == 1 && *i8pro >= 3) {
	return 0;
    }
    for (i__ = 1; i__ <= 32; ++i__) {
	if ((i__ == 15 || i__ == 16 || i__ == 19 || i__ == 20) && *itype <= 3)
		 {
	    goto L50;
	}
	if ((i__ == 11 || i__ == 12 || i__ == 23 || i__ == 24) && *itype <= 2)
		 {
	    goto L50;
	}
	if ((i__ == 7 || i__ == 8 || i__ == 27 || i__ == 28) && *itype <= 1) {
	    goto L50;
	}
	if (iout[i__] == hifant) {
	    iout[i__] = idash;
	}
	if (iout[i__] == lofant) {
	    iout[i__] = x;
	}
L50:
	;
    }
    return 0;
} /* fantom_ */

#undef hifant
#undef lofant
#undef idash
#undef x



/* *********************************************************************** */

/* Subroutine */ int iodc2_(void)
{
    /* Initialized data */

    static integer bel = 7;
    static integer dc2 = 22;

    /* Format strings */
    static char fmt_10[] = "(\002 \002,2a1)";

    /* Fortran I/O blocks */
    static cilist io___401 = { 0, 0, 0, fmt_10, 0 };


/* ***** THIS ROUTINE IS OPTIONAL, IT MAY BE USED TO TURN PERIPHERALS ON */
    io___401.ciunit = lunit_1.pdf;
    s_wsfe(&io___401);
    do_fio(&c__1, (char *)&dc2, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&bel, (ftnlen)sizeof(integer));
    e_wsfe();
    return 0;
} /* iodc2_ */


/* *********************************************************************** */

/* Subroutine */ int iodc4_(void)
{
    /* Initialized data */

    static integer bel = 7;
    static integer dc3 = 23;
    static integer dc4 = 24;

    /* Format strings */
    static char fmt_10[] = "(\002 \002,3a1)";

    /* Fortran I/O blocks */
    static cilist io___405 = { 0, 0, 0, fmt_10, 0 };


/* ***** THIS ROUTINE IS OPTIONAL, IT MAY BE USED TO TURN PERIPHERALS OFF */
    io___405.ciunit = lunit_1.pdf;
    s_wsfe(&io___405);
    do_fio(&c__1, (char *)&bel, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&dc3, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&dc4, (ftnlen)sizeof(integer));
    e_wsfe();
    return 0;
} /* iodc4_ */


/* *********************************************************************** */

/* Subroutine */ int test_(logical *lphase, logical *lbuf, integer *title, 
	integer *ic, integer *il, integer *ile, integer *isym, integer *ibuf, 
	integer *itype, integer *inoo, logical *lfix, integer *ipctr, logical 
	*lerr, integer *isaf, integer *ipctr1, logical *lsa11, logical *lsa01,
	 logical *lprint)
{
    /* Initialized data */

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_482 = { "-   ", 0 };

#define idash (*(integer *)&equiv_482)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_483 = { "L   ", 0 };

#define l (*(integer *)&equiv_483)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_484 = { "H   ", 0 };

#define h__ (*(integer *)&equiv_484)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_485 = { "X   ", 0 };

#define x (*(integer *)&equiv_485)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_486 = { "C   ", 0 };

#define c__ (*(integer *)&equiv_486)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_487 = { "Z   ", 0 };

#define z__ (*(integer *)&equiv_487)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_488 = { "0   ", 0 };

#define n0 (*(integer *)&equiv_488)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_489 = { "1   ", 0 };

#define n1 (*(integer *)&equiv_489)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_490 = { "?   ", 0 };

#define err (*(integer *)&equiv_490)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_491 = { "    ", 0 };

#define iblank (*(integer *)&equiv_491)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_492 = { ";   ", 0 };

#define coment (*(integer *)&equiv_492)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_493 = { "4   ", 0 };

#define i4 (*(integer *)&equiv_493)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_494 = { "6   ", 0 };

#define i6 (*(integer *)&equiv_494)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_495 = { "8   ", 0 };

#define i8 (*(integer *)&equiv_495)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_496 = { "N   ", 0 };

#define nn (*(integer *)&equiv_496)

    static integer bel = 7;

    /* Format strings */
    static char fmt_2[] = "(/,\002 FUNCTION TABLE MUST BE SUPPLIED IN ORDER "
	    "TO PERFORM\002,\002 SIMULATION\002)";
    static char fmt_4[] = "(/,\002 \002,80a1,/)";
    static char fmt_6[] = "(/,\002 FUNCTION TABLE PIN LIST ERROR AT\002,8a1)";
    static char fmt_8[] = "(/,\002 \002,a1,\002 IS NOT AN ALLOWED FUNCTION T"
	    "ABLE ENTRY\002,\002 IN VECTOR \002,i4)";
    static char fmt_41[] = "(/,\002 FUNCTION TABLE ERROR IN VECTOR\002,i4"
	    ",\002  PIN =\002,8a1,\002  EXPECT = H  ACTUAL = L\002)";
    static char fmt_42[] = "(/,\002 FUNCTION TABLE ERROR IN VECTOR\002,i4"
	    ",\002  PIN =\002,8a1,\002  EXPECT = L  ACTUAL = H\002)";
    static char fmt_43[] = "(/,\002 FUNCTION TABLE ERROR IN VECTOR\002,i4"
	    ",\002  PIN =\002,8a1,/,\002  EXPECT  = OUTPUT ENABLE  ACTUAL = "
	    "Z\002)";
    static char fmt_44[] = "(/,\002 FUNCTION TABLE ERROR IN VECTOR\002,i4"
	    ",\002  PIN =\002,8a1,\002  EXPECT = Z  ACTUAL = \002,a1)";
    static char fmt_45[] = "(\002 \002,a1)";
    static char fmt_60[] = "(\002 \002,i4,\002 \002,20a1)";
    static char fmt_1000[] = "(\002 WARNING: MORE THAN 50 TEST VECTORS.  JED"
	    "EC DATA\002,/,\002 WILL CONTAIN ONLY THE FIRST 50 VECTORS.\002)";
    static char fmt_150[] = "(\002 \002,\002 PRODUCT: \002,i3,\002 OF \002"
	    ",\002EQUATION.\002,i3,\002\r                UNTESTED(SA1)FAUL"
	    "T\002)";
    static char fmt_155[] = "(\002 \002,\002 PRODUCT: \002,i3,\002 OF \002"
	    ",\002EQUATION.\002,i3,\002\r                UNTESTED(SA0)FAUL"
	    "T\002)";
    static char fmt_67[] = "(/,\002 PASS SIMULATION\002)";
    static char fmt_68[] = "(/,\002 NUMBER OF FUNCTION TABLE ERRORS =\002,i4)"
	    ;
    static char fmt_101[] = "(/,\002 ERROR SYMBOL = \002,8a1,\002      IN LI"
	    "NE NUMBER \002,i4,/,\002 \002,80a1,/,\002 THIS PIN NAME IS NOT D"
	    "EFINED IN THE\002,\002 FUNCTION TABLE PIN LIST\002)";

    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__, j, ic1, il1, ill, iifb;
    static logical lsa12, lsa02, nreg;
    static integer ieqn;
    extern /* Subroutine */ int incr_(integer *, integer *, logical *);
    static integer imax, ipin[20], iinp, nerr, isum, iout;
    static logical lout[20];
    static integer ieqn1, isym1[160]	/* was [8][20] */;
    extern /* Subroutine */ int match_(integer *, integer *, integer *);
    static logical lsame;
    static integer ivect[20], iprod, ilerr, nvect, imess, itest, ioutp;
    static logical loutp[20];
    static integer itrst;
    static logical lphas1[20];
    static integer ipctr2, ipctr3, ipctr4;
    static logical lenabl[20];
    static integer iclock, imatch;
    static logical lclock;
    static integer ivectp[20], istate[20];
    static logical xorfnd;
    static integer istatt[20];
    static logical lctrst;
    extern /* Subroutine */ int getsym_(logical *, integer *, integer *, 
	    integer *, integer *, logical *), fixtst_(logical *, logical *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, logical *, logical *, logical *);
    static logical lptrst;
    static integer xorsum;

    /* Fortran I/O blocks */
    static cilist io___422 = { 0, 0, 0, fmt_2, 0 };
    static cilist io___423 = { 0, 0, 0, fmt_4, 0 };
    static cilist io___433 = { 0, 0, 0, fmt_6, 0 };
    static cilist io___447 = { 0, 0, 0, fmt_8, 0 };
    static cilist io___467 = { 0, 0, 0, fmt_41, 0 };
    static cilist io___468 = { 0, 0, 0, fmt_42, 0 };
    static cilist io___469 = { 0, 0, 0, fmt_43, 0 };
    static cilist io___470 = { 0, 0, 0, fmt_44, 0 };
    static cilist io___471 = { 0, 0, 0, fmt_45, 0 };
    static cilist io___472 = { 0, 0, 0, fmt_60, 0 };
    static cilist io___473 = { 0, 0, 0, fmt_1000, 0 };
    static cilist io___474 = { 0, 0, 0, fmt_150, 0 };
    static cilist io___477 = { 0, 0, 0, fmt_155, 0 };
    static cilist io___478 = { 0, 0, 0, fmt_67, 0 };
    static cilist io___479 = { 0, 0, 0, fmt_68, 0 };
    static cilist io___481 = { 0, 0, 0, fmt_101, 0 };



/*     THIS SUBROUTINE PERFORMS THE FUNCTION TABLE SIMULATION */
/*      AND GENERATES TEST VECTORS */
    /* Parameter adjustments */
    ibuf -= 9;
    isym -= 9;
    --title;
    --lbuf;
    --lphase;

    /* Function Body */

    tstvec_1.ntest = 0;
/*     PRINT AN ERROR MESSAGE IF NO FUNCTION TABLE IS SUPPLIED */
    if (ftest_1.ifunct != 0) {
	goto L3;
    }
    io___422.ciunit = lunit_1.pms;
    s_wsfe(&io___422);
    e_wsfe();
    return 0;
/*     PRINT TITLE */
L3:
    if (! (*lsa11) && ! (*lsa01) && *lprint) {
	io___423.ciunit = lunit_1.pof;
	s_wsfe(&io___423);
	do_fio(&c__80, (char *)&title[1], (ftnlen)sizeof(integer));
	e_wsfe();
    }
/*     INITIALIZE LERR (FUNCTION TABLE ERROR FLAG) TO NO ERROR */
    *lerr = FALSE_;
/*     INITIALIZE NERR (NUMBER OF FUNCTION TABLE ERRORS) TO NO ERRORS */
    nerr = 0;
/*     INITIALIZE ITRST (THREE-STATE ENABLE FUNCTION TABLE PIN NUMBER) */
    itrst = 0;
/*     SET THE STARTING POINT OF THE FUNCTION TABLE TO COLUMN 0 */
/*     AND IFUNCT + 1 */
    *ic = 0;
    *il = ftest_1.ifunct + 1;
/*     INITIALIZE SA1/SA0 PARAMETERS */
    ipctr3 = 0;
    ieqn = 0;
    *ipctr = 0;

/*     MAKE A DUMMY CALL TO INCR */
    incr_(ic, il, lfix);
/*     GET THE FUNCTION TABLE PIN LIST (UP TO 18) */
/*      GO ONE MORE THAN MAX TO LOOK FOR DASHED LINE */

    for (i__ = 1; i__ <= 19; ++i__) {
	getsym_(lphas1, isym1, &i__, ic, il, lfix);
	for (j = 1; j <= 8; ++j) {
/* L5: */
	    ibuf[j + 8] = isym1[j + (i__ << 3) - 9];
	}
	if (ibuf[16] == idash) {
	    goto L12;
	}
	match_(&imatch, &ibuf[9], &isym[9]);
	if (imatch != 0) {
	    goto L7;
	}
	io___433.ciunit = lunit_1.pms;
	s_wsfe(&io___433);
	for (j = 1; j <= 8; ++j) {
	    do_fio(&c__1, (char *)&ibuf[j + 8], (ftnlen)sizeof(integer));
	}
	e_wsfe();
	return 0;
L7:
	lout[i__ - 1] = FALSE_;
	istatt[i__ - 1] = x;
	ivectp[i__ - 1] = x;
/*     IF APPROPIATE PAL TYPE, REMEMBER LOCATION OF CLOCK AND THREE-STATE */
/*      ENABLE PIN IN FUNCTION TABLE PIN LIST */
	if (*itype != 6) {
	    goto L10;
	}
	if (imatch == 1) {
	    iclock = i__;
	}
	if (imatch == 11) {
	    itrst = i__;
	}
L10:
	ipin[i__ - 1] = imatch;
    }
/*     ALL SIGNAL NAMES FOR THE FUNCTIONAL TEST HAVE BEEN READ IN */
/*      ADJUST COUNT */
L12:
    imax = i__ - 1;
    nvect = 0;

/* *****START OF MAIN LOOP FOR SIMULATION***** */

/*     * SA1/SA0  TESTS */
/*     INITIALLY THERE ARE NO FAULTS. IPCTR2 IS THE POINTER FOR */
/*     TOTAL NUMBER OF PRODUCT TERMS. IEQN IS EQUATION COUNT. */
/*     IPCTR3 IS THE PRODUCT TERM POINTER IN A PARTICULAR EQN. */
L90:
    ipctr2 = 0;
    ieqn = 0;
    ipctr3 = 0;
    lsa12 = FALSE_;
    lsa02 = FALSE_;
/*     *************END OF ADDITION********************** */
    ++nvect;
    ic1 = 0;
    il1 = *ile;
/*     GO PASSED COMMENT LINES */
L23:
    if (pge_1.ipage[*il * 80 - 80] != coment) {
	goto L24;
    }
    ++(*il);
    goto L23;
L24:
/*     GETS VECTORS FROM FUNCTION TABLE */
    i__1 = imax;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (pge_1.ipage[*ic + *il * 80 - 81] == iblank) {
	    goto L21;
	}
	goto L22;
L21:
	++(*ic);
	if (pge_1.ipage[*ic + *il * 80 - 81] == iblank) {
	    goto L21;
	}
L22:
	ivect[i__ - 1] = pge_1.ipage[*ic + *il * 80 - 81];
	++(*ic);
/* L20: */
    }
/*     ADVANCE LINE COUNT TO SKIP FUNCTION TABLE COMMENTS */
    ++(*il);

    *ic = 1;
    if (ivect[0] == idash) {
	goto L95;
    }
/*     CHECK FOR VALID FUNCTION TABLE VALUES (L,H,X,Z,C) */
    i__1 = imax;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (ivect[i__ - 1] == l || ivect[i__ - 1] == h__ || ivect[i__ - 1] == 
		x || ivect[i__ - 1] == z__ || ivect[i__ - 1] == c__) {
	    goto L11;
	}
	io___447.ciunit = lunit_1.pms;
	s_wsfe(&io___447);
	do_fio(&c__1, (char *)&ivect[i__ - 1], (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&nvect, (ftnlen)sizeof(integer));
	e_wsfe();
	return 0;
L11:
	;
    }
/*     INITIALIZE CLOCK AND THREE-STATE ENABLE FLAGS */
    lclock = FALSE_;
    lctrst = TRUE_;
    lptrst = TRUE_;
    i__1 = imax;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L13: */
	lenabl[i__ - 1] = TRUE_;
    }
/*     INITIALIZE NREG (NOT REGISTERED OUTPUT) TO FALSE */
    nreg = FALSE_;
/*     INITIALIZE ISTATE ARRAY TO ALL X'S */
    for (i__ = 1; i__ <= 20; ++i__) {
	istate[i__ - 1] = x;
/* L15: */
    }
/*     CHECK IF THIS PAL TYPE HAS REGISTERS */
    if (*itype != 6) {
	goto L25;
    }
/*     CHECK CLOCK AND THREE-STATE ENABLE PINS AND CHANGE FLAG IF NEEDED */
    if (ivect[iclock - 1] == c__) {
	lclock = TRUE_;
    }
    if (itrst == 0) {
	goto L25;
    }
    lsame = lphase[11] && lphas1[itrst - 1] || ! lphase[11] && ! lphas1[itrst 
	    - 1];
    if (ivect[itrst - 1] == l && ! lsame || ivect[itrst - 1] == h__ && lsame) 
	    {
	lptrst = FALSE_;
    }
    if (lptrst) {
	goto L25;
    }
/*     DISABLE REGISTERED OUTPUTS IF APPROPRIATE */
    i__1 = imax;
    for (i__ = 1; i__ <= i__1; ++i__) {
	j = ipin[i__ - 1];
	if (j == 14 || j == 15 || j == 16 || j == 17) {
	    lenabl[i__ - 1] = FALSE_;
	}
	if (*inoo == i6 && (j == 13 || j == 18)) {
	    lenabl[i__ - 1] = FALSE_;
	}
	if (*inoo == i8 && (j == 12 || j == 13 || j == 18 || j == 19)) {
	    lenabl[i__ - 1] = FALSE_;
	}
/* L46: */
    }

/* *****SCAN THROUGH THE LOGIC EQUATIONS***** */

/*     MAKE A DUMMY CALL TO INCR */
L25:
    incr_(&ic1, &il1, lfix);
L26:
    getsym_(&lbuf[1], &ibuf[9], &c__1, &ic1, &il1, lfix);
    if (_BLNK__1.lleft) {
	goto L29;
    }
L27:
    if (! _BLNK__1.lequal) {
	goto L26;
    }
/* *************ADDED FOR EQN CONT********** */
    if (_BLNK__1.lequal) {
	++ieqn;
    }
/* ***************************************** */
/*     EVALUATE CONDITIONAL THREE-STATE PRODUCT LINE */
L29:
    if (_BLNK__1.lequal) {
	goto L35;
    }
    nreg = TRUE_;
L33:
    getsym_(&lbuf[1], &ibuf[9], &c__1, &ic1, &il1, lfix);

    match_(&iinp, &ibuf[9], isym1);
/*     CHECK FOR GND, VCC, /GND, OR /VCC IN CONDITIONAL THREE-STATE */
/*      PRODUCT LINE */
    if (iinp != 0) {
	goto L32;
    }
    match_(&imatch, &ibuf[9], &isym[9]);
    ill = il1;
    if (iinp == 0 && imatch != 10 && imatch != 20) {
	goto L100;
    }
    if (imatch == 10 && lbuf[1] || imatch == 20 && ! lbuf[1]) {
	lctrst = FALSE_;
    }
    goto L34;
L32:
    itest = ivect[iinp - 1];
    if (itest == l && lphas1[iinp - 1] && lbuf[1] || itest == h__ && lphas1[
	    iinp - 1] && ! lbuf[1] || itest == h__ && ! lphas1[iinp - 1] && 
	    lbuf[1] || itest == l && ! lphas1[iinp - 1] && ! lbuf[1]) {
	lctrst = FALSE_;
    }
    if (itest == x || itest == z__) {
	lctrst = FALSE_;
    }
L34:
    if (_BLNK__1.land) {
	goto L33;
    }
    goto L27;

/*     EVALUATE THE LOGIC EQUATION */

/*     FIND THE PIN NUMBER OF THE OUTPUT VECTORS */
/*     *ADDTION FOR SA0/SA1 TEST */
L35:
    ipctr3 = 0;
/*     *END OF ADDITION */
    match_(&ioutp, &ibuf[9], isym1);
/*     FLAG UNREGISTERED OUTPUTS */
    match_(&iout, &ibuf[9], &isym[9]);
    if (*itype <= 5) {
	nreg = TRUE_;
    }
    if ((*inoo == i4 || *inoo == i6) && (iout == 12 || iout == 19)) {
	nreg = TRUE_;
    }
    if (*inoo == i4 && (iout == 13 || iout == 18)) {
	nreg = TRUE_;
    }
    ill = il1;
    if (ioutp == 0) {
	goto L100;
    }
    if (nreg) {
	lenabl[ioutp - 1] = lctrst;
    }
    lout[ioutp - 1] = TRUE_;
    if (! lctrst) {
	lout[ioutp - 1] = FALSE_;
    }
    lctrst = TRUE_;
    loutp[ioutp - 1] = lbuf[1];
/*     DETERMINE PRODUCT TERM AND EVENTUALLY SUM FOR OUTPUT KEEPING */
/*      TRACK TO SEE IF AN XOR (EXCLUSIVE OR) HAS BEEN FOUND */
    xorsum = h__;
    xorfnd = FALSE_;
    isum = l;
/*     *********THE FOLLOWING IS THE ADDITION FOR SA1/SA0  TESTS*** */
L28:
    ++ipctr2;
    ++ipctr3;
/*     *********END OF ADDITION********* */
    ++(*ipctr);
    iprod = h__;
L30:
    ill = il1;
    getsym_(&lbuf[1], &ibuf[9], &c__1, &ic1, &il1, lfix);
    if (! (*lfix)) {
	goto L39;
    }
/*     EVALUATE THE FIXED SYMBOLS FOUND IN THE PAL16X4 AND PAL16A4 */

    *lfix = FALSE_;
    fixtst_(lphas1, &lbuf[1], &ic1, &il1, &isym[9], isym1, &ibuf[9], ivect, 
	    ivectp, &itest, &lclock, &nreg, lfix);
    if (iprod == h__) {
	iprod = itest;
    }
    goto L38;
L39:
    match_(&iinp, &ibuf[9], isym1);
    if (iinp != 0) {
	goto L47;
    }
    match_(&imatch, &ibuf[9], &isym[9]);
    if (imatch != 10 && imatch != 20) {
	goto L100;
    }
/*     TWEEK FOR GND AND VCC IN PRODUCT LINE */
    if (imatch == 10) {
	itest = l;
    }
    if (imatch == 20) {
	itest = h__;
    }
    iinp = 19;
    lphas1[18] = TRUE_;
    goto L37;
L47:
    itest = ivect[iinp - 1];
/*     GET REGISTERED FEED BACK VALUES */
    if (nreg) {
	goto L37;
    }
    match_(&iifb, &ibuf[9], &isym[9]);
    if ((*inoo == i4 || *inoo == i6 || *inoo == i8) && (iifb == 14 || iifb == 
	    15 || iifb == 16 || iifb == 17)) {
	itest = ivectp[iinp - 1];
    }
    if ((*inoo == i6 || *inoo == i8) && (iifb == 13 || iifb == 18)) {
	itest = ivectp[iinp - 1];
    }
    if (*inoo == i8 && (iifb == 12 || iifb == 19)) {
	itest = ivectp[iinp - 1];
    }
L37:
    if (itest == x || itest == z__) {
	itest = l;
    }
    if (itest == l && lphas1[iinp - 1] && lbuf[1] || itest == h__ && lphas1[
	    iinp - 1] && ! lbuf[1] || itest == h__ && ! lphas1[iinp - 1] && 
	    lbuf[1] || itest == l && ! lphas1[iinp - 1] && ! lbuf[1]) {
	iprod = l;
    }
/*     *THE FOLLOWING ADDITION IS FOR SA1 TEST */
/*     CHECK FOR A PARTICULAR PRODUCT TERM AND GO FOR SA1 TEST */
    if (ipctr2 == *ipctr1 && *lsa11) {
	goto L110;
    }
/*     *END OF ADDITION */
L38:
    if (_BLNK__1.lright) {
	incr_(&ic1, &il1, lfix);
    }
    if (_BLNK__1.land) {
	goto L30;
    }
/*     *SA0 ADDITION */
/*     CHECK FOR A PARTICULAR PRODUCT TERM AND GO FOR SA0 TEST */
    if (ipctr2 == *ipctr1 && *lsa01) {
	goto L120;
    }
/*     *END OF ADDITION */
L121:
    if (isum == l && iprod == x) {
	isum = x;
    }
    if (isum != h__ && iprod == h__) {
	isum = h__;
    }
/*     CHECK FOR XOR (EXCLUSIVE OR) AND SAVE INTERMEDIATE VALUE */
    if (! _BLNK__1.lxor) {
	goto L31;
    }
    xorsum = isum;
    xorfnd = TRUE_;
    isum = l;
    goto L28;
L31:
    if (_BLNK__1.lor) {
	goto L28;
    }
    ipctr3 = 0;
/*     IF END OF EQUATION HAS BEEN FOUND, DETERMINE FINAL SUM AND SAVE IT */
    if (! xorfnd) {
	istatt[ioutp - 1] = isum;
    }
    if (xorfnd && (isum == l && xorsum == l || isum == h__ && xorsum == h__)) 
	    {
	istatt[ioutp - 1] = l;
    }

    if (xorfnd && (isum == h__ && xorsum == l || isum == l && xorsum == h__)) 
	    {
	istatt[ioutp - 1] = h__;
    }
    if (xorfnd && (isum == x || xorsum == x)) {
	istatt[ioutp - 1] = x;
    }
/*     REGISTER DOES NOT CHANGE STATE IF NO CLOCK PULSE IS RECEIVED */
    if (lclock || nreg) {
	goto L36;
    }
    lsame = loutp[ioutp - 1] && lphas1[ioutp - 1] || ! loutp[ioutp - 1] && ! 
	    lphas1[ioutp - 1];
    if (ivectp[ioutp - 1] == l && lsame) {
	istatt[ioutp - 1] = l;
    }
    if (ivectp[ioutp - 1] == h__ && lsame) {
	istatt[ioutp - 1] = h__;
    }
    if (ivectp[ioutp - 1] == l && ! lsame) {
	istatt[ioutp - 1] = h__;
    }
    if (ivectp[ioutp - 1] == h__ && ! lsame) {
	istatt[ioutp - 1] = l;
    }
L36:
    nreg = FALSE_;
/*     CHECK IF ALL EQUATIONS HAVE BEEN PROCESSED BY COMPARING CURRENT */
/*      LINE NUMBER WITH FUNCTION TABLE LINE NUMBER */
    if (ftest_1.idesc != 0 && il1 < ftest_1.ifunct && il1 < ftest_1.idesc || 
	    ftest_1.idesc == 0 && il1 < ftest_1.ifunct) {
	goto L27;
    }
/*     DETERMINE OUTPUT LOGIC VALUES */
/*      COMPARE OUTPUTS TO SEE IF VECTOR AGREES WITH RESULTS */
    i__1 = imax;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (! lout[i__ - 1]) {
	    goto L50;
	}
	if (istatt[i__ - 1] == x && ivect[i__ - 1] == x) {
	    goto L50;
	}
	lsame = loutp[i__ - 1] && lphas1[i__ - 1] || ! loutp[i__ - 1] && ! 
		lphas1[i__ - 1];
	imess = 40;
	if (istatt[i__ - 1] == l && ivect[i__ - 1] == l && ! lsame) {
	    imess = 41;
	}
	if (istatt[i__ - 1] == h__ && ivect[i__ - 1] == h__ && ! lsame) {
	    imess = 42;
	}
	if (istatt[i__ - 1] == l && ivect[i__ - 1] == h__ && lsame) {
	    imess = 42;
	}
	if (istatt[i__ - 1] == h__ && ivect[i__ - 1] == l && lsame) {
	    imess = 41;
	}
	if (lenabl[i__ - 1] && ivect[i__ - 1] == z__) {
	    imess = 43;
	}
	if (! lenabl[i__ - 1] && lout[i__ - 1] && ivect[i__ - 1] != z__) {
	    imess = 44;
	}
	if (imess != 40) {
	    *lerr = TRUE_;
	}
/*     *THIS IS AN ADDITION FOR SA1/SA0  TESTS */
/*     IF NO FAULT GO FOR NEXT VECTOR ELSE GET OUT OF SIMULATION AND */
/*     START SIMULATION FOR THE NEXT PRODUCT TERM. */
	if (! (*lerr) && (*lsa11 || *lsa01)) {
	    goto L50;
	}
	if (*lerr && (*lsa11 || *lsa01)) {
	    goto L115;
	}
/*     *************************************************************** */
	if (imess == 41) {
	    io___467.ciunit = lunit_1.pms;
	    s_wsfe(&io___467);
	    do_fio(&c__1, (char *)&nvect, (ftnlen)sizeof(integer));
	    for (j = 1; j <= 8; ++j) {
		do_fio(&c__1, (char *)&isym1[j + (i__ << 3) - 9], (ftnlen)
			sizeof(integer));
	    }
	    e_wsfe();
	}
	if (imess == 42) {
	    io___468.ciunit = lunit_1.pms;
	    s_wsfe(&io___468);
	    do_fio(&c__1, (char *)&nvect, (ftnlen)sizeof(integer));
	    for (j = 1; j <= 8; ++j) {
		do_fio(&c__1, (char *)&isym1[j + (i__ << 3) - 9], (ftnlen)
			sizeof(integer));
	    }
	    e_wsfe();
	}
	if (imess == 43) {
	    io___469.ciunit = lunit_1.pms;
	    s_wsfe(&io___469);
	    do_fio(&c__1, (char *)&nvect, (ftnlen)sizeof(integer));
	    for (j = 1; j <= 8; ++j) {
		do_fio(&c__1, (char *)&isym1[j + (i__ << 3) - 9], (ftnlen)
			sizeof(integer));
	    }
	    e_wsfe();
	}
	if (imess == 44) {
	    io___470.ciunit = lunit_1.pms;
	    s_wsfe(&io___470);
	    do_fio(&c__1, (char *)&nvect, (ftnlen)sizeof(integer));
	    for (j = 1; j <= 8; ++j) {
		do_fio(&c__1, (char *)&isym1[j + (i__ << 3) - 9], (ftnlen)
			sizeof(integer));
	    }
	    do_fio(&c__1, (char *)&ivect[i__ - 1], (ftnlen)sizeof(integer));
	    e_wsfe();
	}
	if (imess != 40 && lunit_1.pms == 6) {
	    io___471.ciunit = lunit_1.pms;
	    s_wsfe(&io___471);
	    do_fio(&c__1, (char *)&bel, (ftnlen)sizeof(integer));
	    e_wsfe();
	}
	if (imess != 40) {
	    ivect[i__ - 1] = err;
	}
	if (imess != 40) {
	    ++nerr;
	}
L50:
	;
    }

/*     CHANGE THE ORDER OF VECTORS FROM THE ORDER OF APPEARANCE IN THE */
/*      FUNCTION TABLE TO THAT OF THE PIN LIST AND TWEEK FOR OUTPUT */
    for (i__ = 1; i__ <= 20; ++i__) {
	i__1 = imax;
	for (j = 1; j <= i__1; ++j) {
	    if (ipin[j - 1] != i__) {
		goto L55;
	    }
	    if (ivect[j - 1] == l || ivect[j - 1] == h__) {
		goto L51;
	    }
	    istate[i__ - 1] = ivect[j - 1];
	    goto L65;
L51:
	    lsame = lphase[i__] && lphas1[j - 1] || ! lphase[i__] && ! lphas1[
		    j - 1];
	    if (*inoo == n1 && (i__ == 15 || i__ == 16)) {
		lout[j - 1] = TRUE_;
	    }
	    if (! lout[j - 1] && lsame && ivect[j - 1] == l) {
		istate[i__ - 1] = n0;
	    }
	    if (! lout[j - 1] && lsame && ivect[j - 1] == h__) {
		istate[i__ - 1] = n1;
	    }
	    if (! lout[j - 1] && ! lsame && ivect[j - 1] == l) {
		istate[i__ - 1] = n1;
	    }
	    if (! lout[j - 1] && ! lsame && ivect[j - 1] == h__) {
		istate[i__ - 1] = n0;
	    }
	    if (lout[j - 1] && lsame && ivect[j - 1] == l && lenabl[j - 1]) {
		istate[i__ - 1] = l;
	    }
	    if (lout[j - 1] && lsame && ivect[j - 1] == h__ && lenabl[j - 1]) 
		    {
		istate[i__ - 1] = h__;
	    }
	    if (lout[j - 1] && ! lsame && ivect[j - 1] == l && lenabl[j - 1]) 
		    {
		istate[i__ - 1] = h__;
	    }
	    if (lout[j - 1] && ! lsame && ivect[j - 1] == h__ && lenabl[j - 1]
		    ) {
		istate[i__ - 1] = l;
	    }
	    if (ivect[j - 1] == err) {
		istate[i__ - 1] = err;
	    }
	    goto L65;
L55:
	    ;
	}
/*     SAVE PRESENT VECTORS FOR FEED BACK USED WITH NEXT SET OF VECTORS */
/*      IF CLOCK PULSE AND NOT Z (HI-Z IS ASYNCHRONOUS) */
L65:
	if (lclock && ivect[j - 1] != z__) {
	    ivectp[j - 1] = ivect[j - 1];
	}
    }
/*     ASSIGN X TO GROUND PIN AND 1 TO VCC PIN */
    istate[9] = x;
    istate[19] = n1;
/*     PRINT TEST VECTORS */
    if (! (*lsa11) && ! (*lsa01) && *lprint) {
	io___472.ciunit = lunit_1.pof;
	s_wsfe(&io___472);
	do_fio(&c__1, (char *)&nvect, (ftnlen)sizeof(integer));
	for (i__ = 1; i__ <= 20; ++i__) {
	    do_fio(&c__1, (char *)&istate[i__ - 1], (ftnlen)sizeof(integer));
	}
	e_wsfe();
    }

/*         GENERATE TEST VECTORS FOR */
/*         JEDEC FORMAT OUTPUT */

    if (nvect == 51) {
	io___473.ciunit = lunit_1.pms;
	s_wsfe(&io___473);
	e_wsfe();
    }
    if (nvect > 50) {
	goto L90;
    }
    ++tstvec_1.ntest;
    for (i__ = 1; i__ <= 20; ++i__) {
	if (istate[i__ - 1] == l) {
	    tstvec_1.tstvec[i__ + tstvec_1.ntest * 20 - 21] = l;
	}
	if (istate[i__ - 1] == h__) {
	    tstvec_1.tstvec[i__ + tstvec_1.ntest * 20 - 21] = h__;
	}
	if (istate[i__ - 1] == x) {
	    tstvec_1.tstvec[i__ + tstvec_1.ntest * 20 - 21] = x;
	}
	if (istate[i__ - 1] == c__) {
	    tstvec_1.tstvec[i__ + tstvec_1.ntest * 20 - 21] = c__;
	}
	if (istate[i__ - 1] == z__) {
	    tstvec_1.tstvec[i__ + tstvec_1.ntest * 20 - 21] = z__;
	}

	if (istate[i__ - 1] == n0) {
	    tstvec_1.tstvec[i__ + tstvec_1.ntest * 20 - 21] = n0;
	}
	if (istate[i__ - 1] == n1) {
	    tstvec_1.tstvec[i__ + tstvec_1.ntest * 20 - 21] = n1;
	}
	tstvec_1.tstvec[tstvec_1.ntest * 20 - 11] = nn;
	tstvec_1.tstvec[tstvec_1.ntest * 20 - 1] = nn;
	if (tstvec_1.tstvec[tstvec_1.ntest * 20 - 10] == x) {
	    tstvec_1.tstvec[tstvec_1.ntest * 20 - 10] = n0;
	}
/* L1010: */
    }

/*                  END OF ADDITIONS */

    goto L90;
/*     TERMINATE SIMULATION */
/*     *ADITION FOR SA0/SA1 TESTS */
L95:
    if (! (*lerr) && *lsa11 && *lprint) {
	io___474.ciunit = lunit_1.pof;
	s_wsfe(&io___474);
	do_fio(&c__1, (char *)&ipctr4, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ieqn1, (ftnlen)sizeof(integer));
	e_wsfe();
    }
    if (! (*lerr) && *lsa01 && *lprint) {
	io___477.ciunit = lunit_1.pof;
	s_wsfe(&io___477);
	do_fio(&c__1, (char *)&ipctr4, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ieqn1, (ftnlen)sizeof(integer));
	e_wsfe();
    }
/*     *END OF ADDITION */
    if (! (*lerr) && (! (*lsa11) && ! (*lsa01)) && *lprint) {
	io___478.ciunit = lunit_1.pof;
	s_wsfe(&io___478);
	e_wsfe();
    }
    *ipctr /= nvect - 1;
    if (*lerr && (! (*lsa11) && ! (*lsa01)) && *lprint) {
	io___479.ciunit = lunit_1.pof;
	s_wsfe(&io___479);
	do_fio(&c__1, (char *)&nerr, (ftnlen)sizeof(integer));
	e_wsfe();
    }
    return 0;
/*     PRINT AN ERROR MESSAGE FOR AN UNDEFINED PIN NAME */
L100:
    ilerr = ill + 4;
    io___481.ciunit = lunit_1.pms;
    s_wsfe(&io___481);
    for (i__ = 1; i__ <= 8; ++i__) {
	do_fio(&c__1, (char *)&ibuf[i__ + 8], (ftnlen)sizeof(integer));
    }
    do_fio(&c__1, (char *)&ilerr, (ftnlen)sizeof(integer));
    for (i__ = 1; i__ <= 80; ++i__) {
	do_fio(&c__1, (char *)&pge_1.ipage[i__ + ill * 80 - 81], (ftnlen)
		sizeof(integer));
    }
    e_wsfe();
    return 0;
/*     *THIS IS AN ADDITION FOR SA1 TEST */
/*     THE PRODUCT TERM IS PULLED HIGH AND THE PRODUCT NUMBER */
/*     AND EQN NUMBER IS REMEMBERED */
L110:
    iprod = h__;
    lsa12 = TRUE_;
    ieqn1 = ieqn;
    ipctr4 = ipctr3;
    goto L38;
/*     *END OF ADDITION */

/*     *SA0 ADDITION */
/*     THE PRODUCT TERM IS TESTED FOR SA0 FAULT AND ALSO REMEMBERED */
L120:
    iprod = l;
    lsa02 = TRUE_;
    ieqn1 = ieqn;
    ipctr4 = ipctr3;
    goto L121;
/*     *END OF ADDITION */

/*     *ADDITION FOR SA1/SA0  TESTS */
/*     IF NO FAULT THEN NEXT PRODUCT TERM */
L115:
    ++(*isaf);

    *lerr = FALSE_;
    return 0;
/*     *END OF ADDITION */
} /* test_ */

#undef nn
#undef i8
#undef i6
#undef i4
#undef coment
#undef iblank
#undef err
#undef n1
#undef n0
#undef z__
#undef c__
#undef x
#undef h__
#undef l
#undef idash



/* *********************************************************************** */

/* Subroutine */ int fixtst_(logical *lphas1, logical *lbuf, integer *ic1, 
	integer *il1, integer *isym, integer *isym1, integer *ibuf, integer *
	ivect, integer *ivectp, integer *itest, logical *lclock, logical *
	nreg, logical *lfix)
{
    /* Initialized data */

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_511 = { "L   ", 0 };

#define l (*(integer *)&equiv_511)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_512 = { "H   ", 0 };

#define h__ (*(integer *)&equiv_512)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_513 = { "X   ", 0 };

#define x (*(integer *)&equiv_513)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_514 = { "Z   ", 0 };

#define z__ (*(integer *)&equiv_514)


    static logical tor;
    static integer iifb;
    static logical tand;
    static integer iinp;
    static logical txor;
    extern /* Subroutine */ int match_(integer *, integer *, integer *);
    static logical txnor, lphasa, lphasb;
    static integer itesta, itestb;
    extern /* Subroutine */ int getsym_(logical *, integer *, integer *, 
	    integer *, integer *, logical *);

/*     THIS SUBROUTINE EVALUATES THE FIXED SYMBOLS FOUND IN THE */
/*      PAL16X4 AND PAL16A4 FOR THE FUNCTION TABLE */
    /* Parameter adjustments */
    --ivectp;
    --ivect;
    ibuf -= 9;
    isym1 -= 9;
    isym -= 9;
    --lbuf;
    --lphas1;

    /* Function Body */
/*     GET OUTPUT PIN AN (WHERE N=0,1,2,3) */
    getsym_(&lbuf[1], &ibuf[9], &c__1, ic1, il1, lfix);
    match_(&iinp, &ibuf[9], &isym1[9]);
    itesta = ivect[iinp];
    lphasa = lbuf[1] && lphas1[iinp] || ! lbuf[1] && ! lphas1[iinp];
/*     GET REGISTERED FEED BACK VALUES */
    if (*nreg) {
	goto L5;
    }
    match_(&iifb, &ibuf[9], &isym[9]);
    if (iifb == 14 || iifb == 15 || iifb == 16 || iifb == 17) {
	itesta = ivectp[iinp];
    }
L5:
    if (! lphasa && itesta == l) {
	goto L10;
    }
    if (! lphasa && itesta == h__) {
	goto L15;
    }
    goto L20;
L10:
    itesta = h__;
    goto L20;
L15:
    itesta = l;
L20:
    if (! _BLNK__1.lright) {
	goto L25;
    }
    *itest = itesta;
    return 0;
/*     SAVE THE FIXED SYMBOL OPERATORS */
L25:
    tor = _BLNK__1.lor && ! _BLNK__1.lxor;
    txor = _BLNK__1.lxor;
    txnor = _BLNK__1.lxnor;
    tand = _BLNK__1.land && ! _BLNK__1.lxnor;
/*     GET INPUT BN (WHERE N=0,1,2,3) */
    getsym_(&lbuf[1], &ibuf[9], &c__1, ic1, il1, lfix);
    match_(&iinp, &ibuf[9], &isym1[9]);
    itestb = ivect[iinp];
    lphasb = lbuf[1] && lphas1[iinp] || ! lbuf[1] && ! lphas1[iinp];
    if (! lphasb && itestb == l) {
	goto L30;
    }
    if (! lphasb && itestb == h__) {
	goto L35;
    }
    goto L40;
L30:
    itestb = h__;

    goto L40;
L35:
    itestb = l;
/*     EVALUATE THE FIXED SYMBOL EXPRESSION */
L40:
    *itest = l;
    if (tor && (itesta == h__ || itestb == h__)) {
	*itest = h__;
    }
    if (txor && (itesta == h__ && itestb != h__ || itesta != h__ && itestb == 
	    h__)) {
	*itest = h__;
    }
    if (txnor && (itesta == itestb || (itesta == x || itestb == x))) {
	*itest = h__;
    }
    if (tand && (itesta != l && itestb != l)) {
	*itest = h__;
    }
    if ((itesta == x || itesta == z__) && itestb == x) {
	*itest = x;
    }
    return 0;
} /* fixtst_ */

#undef z__
#undef x
#undef h__
#undef l



/* *********************************************************************** */

/* Subroutine */ int exit_(void)
{
    s_stop("", (ftnlen)0);
    return 0;
} /* exit_ */


/*  ************THIS SUBROUTINE IS ADDED FOR JEDEC FORMAT********* */
/*  THE FOLLOWING SUBROUTINE GIVES JEDEC FORMATTED OUTPUT FOR */
/*  PROGRAMMING COMPATIBILITY WITH DATA I/O PROGRAMMERS */
/* Subroutine */ int plotf_(integer *itype, integer *iot)
{
    /* Initialized data */

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_542 = { "0   ", 0 };

#define zero (*(integer *)&equiv_542)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_543 = { "1   ", 0 };

#define one (*(integer *)&equiv_543)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_544 = { "*   ", 0 };

#define star (*(integer *)&equiv_544)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_545 = { "L   ", 0 };

#define l (*(integer *)&equiv_545)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_546 = { "H   ", 0 };

#define h__ (*(integer *)&equiv_546)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_547 = { "X   ", 0 };

#define x (*(integer *)&equiv_547)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_548 = { "C   ", 0 };

#define c__ (*(integer *)&equiv_548)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_549 = { "Z   ", 0 };

#define z__ (*(integer *)&equiv_549)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_550 = { "0   ", 0 };

#define n0 (*(integer *)&equiv_550)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_551 = { "1   ", 0 };

#define n1 (*(integer *)&equiv_551)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_552 = { "N   ", 0 };

#define nn (*(integer *)&equiv_552)


    /* Format strings */
    static char fmt_10[] = "(\002 \002,a1,\002*D22\002,i2,\002*F0*\002)";
    static char fmt_201[] = "(\002 L\002,4a1,\002 \002,32a1,a1)";
    static char fmt_410[] = "(\002 V\002,4i1,1x,20a1,\002 *\002)";
    static char fmt_400[] = "(\002 \002,a1,4a1,/)";

    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer i__, j, j1, j2, j3, etx, stx, iadr, iinp;
    extern integer iconv_(integer *);
    static integer idecio[4];
    extern /* Subroutine */ int sumchk_(void);
    static integer isumio[4], pinout;

    /* Fortran I/O blocks */
    static cilist io___531 = { 0, 0, 0, fmt_10, 0 };
    static cilist io___537 = { 0, 0, 0, fmt_201, 0 };
    static cilist io___539 = { 0, 0, 0, fmt_410, 0 };
    static cilist io___541 = { 0, 0, 0, fmt_400, 0 };


    iadr = 0;
    stx = 2;
    etx = 3;
    j3 = 0;

/*           ADDITIONS  TO GENERATE PINOUT AND */
/*           FAMILY CODE. */

    if (*itype != 6) {
	goto L24;
    }
    pinout = 24;
    goto L30;
L24:
    if (*iot != l) {
	goto L28;
    }
    pinout = *itype + 12;
    goto L30;
L28:
    pinout = *itype + 17;
    if (*itype == 4 && *iot == h__) {
	++pinout;
    }
L30:
    sum_1.isum[3] = pinout / 10 + 536 + pinout % 10;
    sum_1.isum[1] = sum_1.isum[3] / 256 % 256;
    sum_1.isum[3] %= 256;
    io___531.ciunit = lunit_1.pdf;
    s_wsfe(&io___531);
    do_fio(&c__1, (char *)&stx, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&pinout, (ftnlen)sizeof(integer));
    e_wsfe();


    for (ipt_1.ipt = 1; ipt_1.ipt <= 64; ++ipt_1.ipt) {
	if (lfuses_1.lphant[(ipt_1.ipt << 5) - 24]) {
	    goto L300;
	}
	sum_1.nfuse = 0;
	for (iinp = 1; iinp <= 32; ++iinp) {
	    if (lfuses_1.lphant[iinp + (ipt_1.ipt << 5) - 33]) {
		goto L50;
	    }
	    ++sum_1.nfuse;
	    if (lfuses_1.lfuses[iinp + (ipt_1.ipt << 5) - 33]) {
		sum_1.ipbuf[sum_1.nfuse - 1] = one;
	    }
	    if (! lfuses_1.lfuses[iinp + (ipt_1.ipt << 5) - 33]) {
		sum_1.ipbuf[sum_1.nfuse - 1] = zero;
	    }
L50:
	    ;
	}
	if (lfuses_1.lfuses[(ipt_1.ipt << 5) - 32]) {
	    goto L100;
	}
	if (! lfuses_1.lfuses[(ipt_1.ipt << 5) - 31]) {
	    goto L250;
	}
L100:
	sum_1.idec[3] = iadr;
	for (j = 1; j <= 3; ++j) {
	    j1 = 5 - j;
	    j2 = 4 - j;
	    sum_1.idec[j2 - 1] = sum_1.idec[j1 - 1] / 10;
	    sum_1.idec[j1 - 1] -= sum_1.idec[j2 - 1] * 10;
	    idecio[j1 - 1] = iconv_(&sum_1.idec[j1 - 1]);
/* L150: */
	}
	idecio[0] = iconv_(sum_1.idec);
	sumchk_();
	io___537.ciunit = lunit_1.pdf;
	s_wsfe(&io___537);
	do_fio(&c__4, (char *)&idecio[0], (ftnlen)sizeof(integer));
	i__1 = sum_1.nfuse;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    do_fio(&c__1, (char *)&sum_1.ipbuf[i__ - 1], (ftnlen)sizeof(
		    integer));
	}
	do_fio(&c__1, (char *)&star, (ftnlen)sizeof(integer));
	e_wsfe();
L250:
	iadr += sum_1.nfuse;
L300:
	;
    }

/*            GENERATE TEST VECTORS. */

    if (tstvec_1.ntest <= 0) {
	goto L380;
    }
    if (tstvec_1.ntest > 50) {
	tstvec_1.ntest = 50;
    }
    i__1 = tstvec_1.ntest;
    for (j = 1; j <= i__1; ++j) {
	sum_1.isum[3] += 215;
	sum_1.isum[1] = (sum_1.isum[1] + sum_1.isum[3] / 256) % 256;
	sum_1.isum[3] %= 256;
	for (i__ = 1; i__ <= 20; ++i__) {
	    j1 = tstvec_1.tstvec[i__ + j * 20 - 21];
	    if (j1 == l) {
		sum_1.isum[3] += 76;
	    }
	    if (j1 == h__) {
		sum_1.isum[3] += 72;
	    }
	    if (j1 == c__) {
		sum_1.isum[3] += 67;
	    }
	    if (j1 == x) {
		sum_1.isum[3] += 88;
	    }
	    if (j1 == z__) {
		sum_1.isum[3] += 90;
	    }
	    if (j1 == nn) {
		sum_1.isum[3] += 78;
	    }
	    if (j1 == n1) {
		sum_1.isum[3] += 49;
	    }
	    if (j1 == n0) {
		sum_1.isum[3] += 48;
	    }
	    sum_1.isum[1] = (sum_1.isum[1] + sum_1.isum[3] / 256) % 256;
	    sum_1.isum[3] %= 256;
/* L350: */
	}
	for (i__ = 1; i__ <= 4; ++i__) {
	    i__2 = 4 - i__;
	    idecio[i__ - 1] = j / pow_ii(&c__10, &i__2) % 10;
	    sum_1.isum[3] = sum_1.isum[3] + idecio[i__ - 1] + 48;
	    sum_1.isum[1] = (sum_1.isum[1] + sum_1.isum[3] / 256) % 256;
	    sum_1.isum[3] %= 256;
/* L360: */
	}
	io___539.ciunit = lunit_1.pdf;
	s_wsfe(&io___539);
	do_fio(&c__4, (char *)&idecio[0], (ftnlen)sizeof(integer));
	for (i__ = 1; i__ <= 20; ++i__) {
	    do_fio(&c__1, (char *)&tstvec_1.tstvec[i__ + j * 20 - 21], (
		    ftnlen)sizeof(integer));
	}
	e_wsfe();
/* L370: */
    }

/*           ENDIF */

L380:
    i__1 = sum_1.isum[1] / 16;
    isumio[0] = iconv_(&i__1);

    sum_1.isum[1] %= 16;
    isumio[1] = iconv_(&sum_1.isum[1]);
    i__1 = sum_1.isum[3] / 16;
    isumio[2] = iconv_(&i__1);
    sum_1.isum[3] %= 16;
    isumio[3] = iconv_(&sum_1.isum[3]);
    io___541.ciunit = lunit_1.pdf;
    s_wsfe(&io___541);
    do_fio(&c__1, (char *)&etx, (ftnlen)sizeof(integer));
    do_fio(&c__4, (char *)&isumio[0], (ftnlen)sizeof(integer));
    e_wsfe();
    return 0;
} /* plotf_ */

#undef nn
#undef n1
#undef n0
#undef z__
#undef c__
#undef x
#undef h__
#undef l
#undef star
#undef one
#undef zero



/* ******************************************************* */

/* THIS  SUBROUTINE CALCULATES THE SUMCHECK */
/* Subroutine */ int sumchk_(void)
{
    static integer j;

    for (j = 1; j <= 32; ++j) {
	if (lfuses_1.lphant[j + (ipt_1.ipt << 5) - 33]) {
	    goto L50;
	}
	if (lfuses_1.lfuses[j + (ipt_1.ipt << 5) - 33]) {
	    sum_1.bufio[j - 1] = 49;
	}
	if (! lfuses_1.lfuses[j + (ipt_1.ipt << 5) - 33]) {
	    sum_1.bufio[j - 1] = 48;
	}
	sum_1.isum[3] += sum_1.bufio[j - 1];
	sum_1.isum[1] = (sum_1.isum[1] + sum_1.isum[3] / 256) % 256;
	sum_1.isum[3] %= 256;
L50:
	;
    }
    for (j = 1; j <= 4; ++j) {
	sum_1.isum[3] = sum_1.isum[3] + sum_1.idec[j - 1] + 48;
	sum_1.isum[1] = (sum_1.isum[1] + sum_1.isum[3] / 256) % 256;
	sum_1.isum[3] %= 256;
/* L100: */
    }
    sum_1.isum[3] += 173;
    sum_1.isum[1] = (sum_1.isum[1] + sum_1.isum[3] / 256) % 256;
    sum_1.isum[3] %= 256;
    return 0;
} /* sumchk_ */


/* **************************************************************** */

/* Subroutine */ int intel_(logical *lfuses, integer *iop)
{
    /* Initialized data */

    static struct {
	char e_1[64];
	integer e_2;
	} equiv_566 = { "0   1   2   3   4   5   6   7   8   9   A   B   C  "
		" D   E   F   ", 0 };

#define ztable ((integer *)&equiv_566)


    /* Format strings */
    static char fmt_60[] = "(1x,\002 :20\002,4a1,\00200\002,32(\0020\002,a1)"
	    ",2a1)";
    static char fmt_70[] = "(1x,\002 :00000001FF\002)";

    /* Local variables */
    static integer i__, inc, addr__, ihex, csum, isum2, iprod, itemp[32], 
	    iinput;

    /* Fortran I/O blocks */
    static cilist io___564 = { 0, 0, 0, fmt_60, 0 };
    static cilist io___565 = { 0, 0, 0, fmt_70, 0 };


/*     THIS SUBROUTINE GENERATES INTEL HEX PROGRAMMING FORMATS */
    /* Parameter adjustments */
    lfuses -= 33;

    /* Function Body */
    addr__ = 0;

    for (i__ = 1; i__ <= 33; i__ += 32) {
	inc = i__ - 1;
	for (iprod = 1; iprod <= 8; ++iprod) {
	    csum = (addr__ / 256 + addr__ % 256 + 32) % 256;
	    for (iinput = 1; iinput <= 32; ++iinput) {
		ihex = 0;
		isum2 = iprod + inc;
		if (lfuses[iinput + (isum2 << 5)]) {
		    ++ihex;
		}
		if (lfuses[iinput + (isum2 + 8 << 5)]) {
		    ihex += 2;
		}
		if (lfuses[iinput + (isum2 + 16 << 5)]) {
		    ihex += 4;
		}
		if (lfuses[iinput + (isum2 + 24 << 5)]) {
		    ihex += 8;
		}
		csum = (csum + ihex) % 256;
		itemp[iinput - 1] = ztable[ihex];
/* L20: */
	    }
	    if (csum != 0) {
		csum = 256 - csum;
	    }
	    io___564.ciunit = lunit_1.pdf;
	    s_wsfe(&io___564);
	    do_fio(&c__1, (char *)&ztable[addr__ / 4096], (ftnlen)sizeof(
		    integer));
	    do_fio(&c__1, (char *)&ztable[addr__ / 256 % 16], (ftnlen)sizeof(
		    integer));
	    do_fio(&c__1, (char *)&ztable[addr__ / 16 % 16], (ftnlen)sizeof(
		    integer));
	    do_fio(&c__1, (char *)&ztable[addr__ % 16], (ftnlen)sizeof(
		    integer));
	    do_fio(&c__32, (char *)&itemp[0], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&ztable[csum / 16], (ftnlen)sizeof(integer))
		    ;
	    do_fio(&c__1, (char *)&ztable[csum % 16], (ftnlen)sizeof(integer))
		    ;
	    e_wsfe();
	    addr__ += 32;
/* L40: */
	}
    }
    io___565.ciunit = lunit_1.pdf;
    s_wsfe(&io___565);
    e_wsfe();
    return 0;
} /* intel_ */

#undef ztable


