/* palasm24.f -- translated by f2c (version 12.02.01).
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
    logical lblank, lleft, land, lor, lslash, lequal, lright, lxor;
} _BLNK__;

#define _BLNK__1 _BLNK__

struct {
    integer ipage[16000]	/* was [80][200] */;
} pge_;

#define pge_1 pge_

struct {
    logical lfuses[3200]	/* was [40][80] */, lphant[3200]	/* 
	    was [40][80] */;
} lfuses_;

#define lfuses_1 lfuses_

struct {
    integer ifunct, idesc, iend;
} ftest_;

#define ftest_1 ftest_

struct {
    integer pms, pof, pdf;
} lunit_;

#define lunit_1 lunit_

union {
    struct {
	integer ntest, tstvec;
    } _1;
    struct {
	integer ntest, tstvec[1008]	/* was [24][42] */;
    } _2;
} tstvec_;

#define tstvec_1 (tstvec_._1)
#define tstvec_2 (tstvec_._2)

struct {
    integer ipt;
} ipt_;

#define ipt_1 ipt_

struct {
    integer isum[4], idec[4], ipbuf[40], bufio[40], nfuse;
} sum_;

#define sum_1 sum_

struct blk_1_ {
    integer pr8x10[140]	/* was [10][14] */, prod8[88]	/* was [8][11] */, 
	    prodln[280]	/* was [40][7] */;
};

#define blk_1 (*(struct blk_1_ *) &blk_)

/* Initialized data */

struct {
    integer e_1[228];
    char e_2[1120];
    } blk_ = { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 6, 5, 5, 5, 5, 5, 5, 6, 3, 3, 
	    3, 7, 7, 8, 8, 7, 7, 3, 3, 3, 3, 3, 9, 10, 10, 9, 3, 3, 3, 3, 3, 
	    3, 3, 1, 1, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 3, 3, 3, 3, 11, 11, 11, 
	    11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 
	    11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 
	    11, 11, 11, 11, 11, 3, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 
	    1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 1, 1, 
	    1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 
	    3, 3, 3, 3, 3, 4, 4, 3, 3, 3, 3, 3, 3, 5, 5, 3, 3, 3, 3, 3, 3, 5, 
	    5, 5, 5, 3, 3, 3, 3, 6, 6, 6, 6, 3, 3, 3, 3, 6, 6, 3, 3, 3, 3, 3, 
	    3, 7, 7, 7, 7, 7, 7, 3, 3, 7, 7, 7, 7, 3, 3, 3, 3, 1, 1, 1, 1, 3, 
	    3, 3, 3, "X   X   X   X   X   X   X   X   X   X   X   X   X   X "
	    "  X   X   X   X   X   X   X   X   X   X   X   X   X   X   X   X "
	    "  X   X   X   X   X   X   X   X   X   X   P   P   P   P   P   P "
	    "  P   P   P   P   P   P   P   P   P   P   P   P   P   P   P   P "
	    "  P   P   P   P   P   P   P   P   P   P   P   P   P   P   P   P "
	    "  P   P   N   N   N   N   N   N   N   N   N   N   N   N   N   N "
	    "  N   N   N   N   N   N   N   N   N   N   N   N   N   N   N   N "
	    "  N   N   N   N   N   N   N   N   N   N   X   X   X   X   X   X "
	    "  P   P   X   X   P   P   X   X   P   P   X   X   P   P   X   X "
	    "  P   P   X   X   P   P   X   X   P   P   X   X   P   P   X   X "
	    "  X   X   X   X   X   X   X   X   X   X   X   X   P   P   X   X "
	    "  P   P   X   X   P   P   X   X   P   P   X   X   P   P   X   X "
	    "  P   P   X   X   X   X   X   X   X   X   X   X   X   X   X   X "
	    "  X   X   X   X   X   X   X   X   P   P   X   X   P   P   X   X "
	    "  P   P   X   X   P   P   X   X   X   X   X   X   X   X   X   X "
	    "  X   X   X   X   X   X   X   X   X   X   X   X   X   X   X   X "
	    "  X   X   X   X   P   P   X   X   P   P   X   X   X   X   X   X "
	    "  X   X   X   X   X   X   X   X   X   X   " };


/* Table of constant values */

static integer c__1 = 1;
static integer c__3 = 3;
static integer c__5 = 5;
static integer c__72 = 72;
static integer c__80 = 80;
static logical c_false = FALSE_;
static logical c_true = TRUE_;
static integer c__40 = 40;
static integer c__64 = 64;
static integer c__4 = 4;
static integer c__10 = 10;
static integer c__50 = 50;

/* **PALASM24**PALASM24**PALASM24**PALASM24**PALASM24**PALASM24**PALASM24* */

/*  P A L A S M  2 4  -  TRANSLATES SYMBOLIC EQUATIONS INTO PAL OBJECT */
/*                       CODE FORMATTED FOR DIRECT INPUT TO STANDARD */
/*                       PROM AND PAL PROGRAMMERS. */


/*                       REV LEVEL:   VERSION 1.6C  (08/16/83) */
/*                       (C)  COPYRIGHT 1983 MONOLITHIC MEMORIES */


/*                       *********************************************** */
/*                       *                                             * */
/*                       *                  APPROVAL                   * */
/*                       *                                             * */
/*                       *                                             * */
/*                       *   1: JOHN BIRKNER                           * */
/*                       *                                             * */
/*                       *  PROGRAMMABLE LOGIC PLANNER                 * */
/*                       *                                             * */
/*               	*                                             * */
/*                       *   2: VINCENT COLI                           * */
/* 	                *                                             * */
/*                       *  APPLICATIONS ENGINEER                      * */
/*                       *                                             * */
/*                       *                                             * */
/*                       *   3: MANOUCHEHR VAFAI                       * */
/*                       *                                             * */
/*                       *  APPLICATIONS ENGINEER                      * */
/*                       *                                             * */
/*                       *                                             * */
/*                       *********************************************** */


/*                       INPUT:       PAL DESIGN SPECIFICATION ASSIGNED */
/*                                    TO RPD(1).  OPERATION CODES ARE */
/*                                    ASSIGNED TO ROP(5). */

/*                       OUTPUT:      ECHO, SIMULATION, AND FUSE PATTERN */
/*                                    ARE ASSIGNED TO POF(6).  JEDEC, HEX, */
/*                                    AND BINARY PROGRAMMING FORMATS ARE */
/*                                    ASSIGNED TO PDF(6).  PROMPTS AND */
/*                                    ERROR MESSAGES ARE ASSIGNED TO */
/*                                    PMS(6). */

/*                       PART NUMBER: THE PAL PART NUMBER MUST APPEAR */
/*                                    IN COLUMN ONE OF LINE ONE. */

/*                       PIN LIST:    24 SYMBOLIC PIN NAMES MUST APPEAR */
/*                                    STARTING ON LINE FIVE. */

/*                       EQUATIONS:   STARTING FIRST LINE AFTER THE */
/*                                    PIN LIST IN THE FOLLOWING FORMS: */

/*                                       A = B*C + D */

/*                                       A := B*C + D */


/*                                       IF( A*B )  C = D + E */

/*                                    ALL CHARACTERS FOLLOWING ';' ARE */
/*                                    IGNORED UNTIL THE NEXT LINE. */

/*                                    BLANKS ARE IGNORED. */
/*                       OPERATORS:   ( IN HIERARCHY OF EVALUATION ) */

/*                                     ;    COMMENT FOLLOWS */
/*                                     /    COMPLEMENT */
/*                                     *    AND (PRODUCT) */
/*                                     +    OR (SUM) */
/*                                    :+:   XOR (EXCLUSIVE OR) */
/*                                    ( )   CONDITIONAL THREE-STATE */
/*                                     =    EQUALITY */
/*                                    :=    REPLACED BY (AFTER CLOCK) */

/*                       FUNCTION     L, H, X, Z, AND C ARE VALID */
/*                         TABLE:     FUNCTION TABLE VECTOR ENTRIES. */

/*                       REFERENCE:   A COMPLETE USERS GUIDE TO */
/*                                    DESIGNING WITH PALS USING PALASM */
/*                                    IS PROVIDED IN THE MONOLITHIC */
/*                                    MEMORIES PAL HANDBOOK. */

/*                       SUBROUTINES: INITLZ,GETSYM,INCR,MATCH,IXLATE, */
/*                                    ECHO,CAT,PINOUT,PLOT,PLOTF,SUMCHK */
/*                                    ICONV,HEX,SUMCHK,TWEEK,BINR,SLIP, */
/*                                    FANTOM,IODC2,IODC4,TEST,INTEL */



/*                       AUTHORS:     JOHN BIRKNER AND VINCENT COLI */
/*                                    FAULT TESTING BY IMTIYAZ BENGALI */
/*                                    REVISED JEDEC FORMAT BY MANO VAFAI */
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
/*                                    COMPUTER WITH FORTRAN 77 AND A */
/*                                    NATIONAL CSS IBM SYSTEM/370 */
/*                                    WITH FORTRAN IV (LEVEL G). */

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
	} equiv_97 = { "E   ", 0 };

#define e (*(integer *)&equiv_97)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_98 = { "N   ", 0 };

#define n (*(integer *)&equiv_98)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_99 = { "C   ", 0 };

#define c__ (*(integer *)&equiv_99)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_100 = { "Q   ", 0 };

#define q (*(integer *)&equiv_100)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_101 = { "F   ", 0 };

#define f (*(integer *)&equiv_101)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_102 = { "B   ", 0 };

#define bb (*(integer *)&equiv_102)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_103 = { "C   ", 0 };

#define cc (*(integer *)&equiv_103)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_104 = { "D   ", 0 };

#define dd (*(integer *)&equiv_104)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_105 = { "E   ", 0 };

#define ee (*(integer *)&equiv_105)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_106 = { "O   ", 0 };

#define o (*(integer *)&equiv_106)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_107 = { "F   ", 0 };

#define ff (*(integer *)&equiv_107)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_108 = { "I   ", 0 };

#define ii (*(integer *)&equiv_108)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_109 = { "N   ", 0 };

#define nn (*(integer *)&equiv_109)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_110 = { "O   ", 0 };

#define oo (*(integer *)&equiv_110)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_111 = { "P   ", 0 };

#define pp (*(integer *)&equiv_111)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_112 = { "R   ", 0 };

#define rr (*(integer *)&equiv_112)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_113 = { "S   ", 0 };

#define ss (*(integer *)&equiv_113)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_114 = { "T   ", 0 };

#define tt (*(integer *)&equiv_114)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_115 = { "U   ", 0 };

#define uu (*(integer *)&equiv_115)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_116 = { "J   ", 0 };

#define jj (*(integer *)&equiv_116)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_117 = { "T   ", 0 };

#define t (*(integer *)&equiv_117)

    static integer bel = 7;
    static struct {
	char e_1[4];
	integer e_2;
	} equiv_118 = { "P   ", 0 };

#define p (*(integer *)&equiv_118)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_119 = { "B   ", 0 };

#define b (*(integer *)&equiv_119)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_120 = { "H   ", 0 };

#define h__ (*(integer *)&equiv_120)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_121 = { "S   ", 0 };

#define s (*(integer *)&equiv_121)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_122 = { "L   ", 0 };

#define l (*(integer *)&equiv_122)


    /* Format strings */
    static char fmt_3[] = "(/,\002 MONOLITHIC MEMORIES 24-PIN PALASM VERSION"
	    " 1.6C\002)";
    static char fmt_31[] = "(\002 (C) COPYRIGHT 1983 MONOLITHIC MEMORIES\002)"
	    ;
    static char fmt_1[] = "(/,\002 WHAT IS THE LOGICAL UNIT NUMBER FOR OUTPU"
	    "T(6)?: \002$)";
    static char fmt_2[] = "(i4)";
    static char fmt_10[] = "(3a1,5a1,72a1,/,80a1,/,80a1,/,80a1)";
    static char fmt_11[] = "(80a1)";
    static char fmt_18[] = "(/,\002 PAL PART TYPE \002,3a1,5a1,\002 IS INCOR"
	    "RECT\002)";
    static char fmt_23[] = "(/,\002 LESS THAN 24 PIN NAMES IN PIN LIST\002)";
    static char fmt_99[] = "(\002 \002,a1)";
    static char fmt_101[] = "(/,\002 ERROR SYMBOL =  \002,8a1,\002      IN L"
	    "INE NUMBER \002,i3,/,\002 \002,80a1)";
    static char fmt_103[] = "(\002 OUTPUT MUST BE INVERTED SINCE \002,3a1,5a"
	    "1,\002 IS AN ACTIVE LOW DEVICE\002)";
    static char fmt_105[] = "(\002 THIS PIN, NUMBER \002,i2,\002 IS AN INVAL"
	    "ID OUTPUT PIN\002,\002 FOR \002,3a1,5a1)";
    static char fmt_115[] = "(\002 THIS PIN NUMBER \002,i2,\002 IS AN INVALI"
	    "D INPUT PIN\002,\002 FOR \002,3a1,5a1)";
    static char fmt_106[] = "(/,\002 OPERATION CODES:\002)";
    static char fmt_107[] = "(/,\002 E=ECHO INPUT  O=PINOUT  T=SIMULATE  F=F"
	    "AULT TEST\002,/,\002 P=PLOT  B=BRIEF  J=JEDEC FORMAT  H=HEX  S=S"
	    "HORT\002,/,\002 L=BHLF  N=BNPF   I=INTEL FORMAT C=CATALOG  Q=QUIT"
	    "\002)";
    static char fmt_110[] = "(/,\002 ENTER OPERATION CODE: \002,$)";
    static char fmt_120[] = "(a1)";
    static char fmt_125[] = "(\0021\002)";
    static char fmt_220[] = "(/,\002 NUMBER OF STUCK AT ONE  (SA1) FAULTS AR"
	    "E =\002,i3)";
    static char fmt_225[] = "(/,\002 NUMBER OF STUCK AT ZERO (SA0) FAULTS AR"
	    "E =\002,i3)";
    static char fmt_230[] = "(/,\002 PRODUCT   TERM   COVERAGE              "
	    "  =\002i4,\002%\002,/)";

    /* System generated locals */
    integer i__1;
    logical L__1;

    /* Local variables */
    static integer i__, j, ic, il;
    extern /* Subroutine */ int cat_(void);
    static integer ile, ill, roc, rpd, iop;
    extern /* Subroutine */ int hex_(logical *, integer *);
    static integer iot, lun, isa0, isa1;
    static logical lsa01, lsa11;
    extern /* Subroutine */ int echo_(integer *, integer *, integer *, 
	    integer *, integer *, integer *);
    static logical lact;
    static integer isaf, ipal[3], ibuf[192]	/* was [8][24] */;
    static logical lbuf[24];
    extern /* Subroutine */ int binr_(logical *, integer *, integer *), incr_(
	    integer *, integer *);
    static integer comp[80];
    static logical linp, lerr;
    extern /* Subroutine */ int slip_(logical *, integer *, integer *, 
	    integer *), sub_exit(void);
    static integer rest[72];
    extern /* Subroutine */ int plot_(), test_();
    static integer isym[192]	/* was [8][24] */, i8pro, iname[5];
    extern /* Subroutine */ int match_(integer *, integer *, integer *);
    static logical lsame;
    extern /* Subroutine */ int intel_(logical *, integer *);
    static integer iblow, iprod, ilerr;
    static logical lprod[80];
    static integer title[80], ipctr;
    extern /* Subroutine */ int tweek_(integer *, logical *, logical *), 
	    plotf_(integer *, integer *);
    static integer i88pro, itype, ipctr0, ipctr1, imatch;
    static logical lphase[24];
    static integer ifault;
    extern /* Subroutine */ int ixlate_(integer *, logical *, integer *, 
	    logical *, integer *);
    static logical lfirst;
    static integer patnum[80];
    static logical loperr;
    extern /* Subroutine */ int getsym_(logical *, integer *, integer *, 
	    integer *, integer *), initlz_(integer *, integer *, logical *, 
	    logical *, integer *, integer *, integer *, integer *);
    static integer iinput;
    extern /* Subroutine */ int pinout_(integer *, integer *, integer *);

    /* Fortran I/O blocks */
    static cilist io___31 = { 0, 6, 0, fmt_3, 0 };
    static cilist io___32 = { 0, 6, 0, fmt_31, 0 };
    static cilist io___33 = { 0, 6, 0, fmt_1, 0 };
    static cilist io___34 = { 0, 5, 0, fmt_2, 0 };
    static cilist io___41 = { 0, 0, 0, fmt_10, 0 };
    static cilist io___49 = { 0, 0, 1, fmt_11, 0 };
    static cilist io___56 = { 0, 0, 0, fmt_18, 0 };
    static cilist io___59 = { 0, 0, 0, fmt_23, 0 };
    static cilist io___74 = { 0, 0, 0, fmt_99, 0 };
    static cilist io___75 = { 0, 0, 0, fmt_101, 0 };
    static cilist io___76 = { 0, 0, 0, fmt_103, 0 };
    static cilist io___77 = { 0, 0, 0, fmt_105, 0 };
    static cilist io___78 = { 0, 0, 0, fmt_115, 0 };
    static cilist io___79 = { 0, 6, 0, fmt_106, 0 };
    static cilist io___80 = { 0, 6, 0, fmt_107, 0 };
    static cilist io___81 = { 0, 6, 0, fmt_110, 0 };
    static cilist io___82 = { 0, 0, 0, fmt_120, 0 };
    static cilist io___83 = { 0, 0, 0, fmt_125, 0 };
    static cilist io___94 = { 0, 0, 0, fmt_220, 0 };
    static cilist io___95 = { 0, 0, 0, fmt_225, 0 };
    static cilist io___96 = { 0, 0, 0, fmt_230, 0 };





/*     ASSIGNMENT OF DATA SET REFERENCES */
/*     RPD - PAL DESIGN SPECIFICATION (INPUT) */
/*     ROC - OPERATION CODE (INPUT) */
/*     POF - ECHO, PINOUT, TEST, AND PLOT (OUTPUT) */
/*     PDF - JEDEC, HEX, AND BINARY PROGRAMMING FORMATS (OUTPUT) */
/*     PMS - PROMPTS AND ERROR MESSAGES (OUTPUT) */
    s_wsfe(&io___31);
    e_wsfe();
    s_wsfe(&io___32);
    e_wsfe();
    s_wsfe(&io___33);
    e_wsfe();
    s_rsfe(&io___34);
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
/*     READ IN FIRST 4 LINES OF PAL DESIGN SPECIFICATION */
    io___41.ciunit = rpd;
    s_rsfe(&io___41);
    do_fio(&c__3, (char *)&ipal[0], (ftnlen)sizeof(integer));
    do_fio(&c__5, (char *)&iname[0], (ftnlen)sizeof(integer));
    do_fio(&c__72, (char *)&rest[0], (ftnlen)sizeof(integer));
    do_fio(&c__80, (char *)&patnum[0], (ftnlen)sizeof(integer));
    do_fio(&c__80, (char *)&title[0], (ftnlen)sizeof(integer));
    do_fio(&c__80, (char *)&comp[0], (ftnlen)sizeof(integer));
    e_rsfe();
/*     READ IN PIN LIST (LINE 5) THROUGH THE END OF THE PAL DESIGN */
/*      SPECIFICATION */
    for (j = 1; j <= 200; ++j) {
	io___49.ciunit = rpd;
	i__1 = s_rsfe(&io___49);
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
/*         CHECK FOR 'FUNCTION TABLE' AND SAVE ITS LINE NUMBER */
	if (ftest_1.ifunct == 0 && pge_1.ipage[j * 80 - 80] == ff && 
		pge_1.ipage[j * 80 - 79] == uu && pge_1.ipage[j * 80 - 78] == 
		nn && pge_1.ipage[j * 80 - 77] == cc && pge_1.ipage[j * 80 - 
		76] == tt && pge_1.ipage[j * 80 - 75] == ii && pge_1.ipage[j *
		 80 - 74] == oo && pge_1.ipage[j * 80 - 73] == nn && 
		pge_1.ipage[j * 80 - 71] == tt && pge_1.ipage[j * 80 - 69] == 
		bb && pge_1.ipage[j * 80 - 67] == ee) {
	    ftest_1.ifunct = j;
	}
/*         CHECK FOR 'DESCRIPTION' AND SAVE ITS LINE NUMBER */

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
    initlz_(iname, &itype, lfuses_1.lfuses, lfuses_1.lphant, &ic, &il, &iblow,
	     &ipctr);
/*     PRINT ERROR MESSAGE FOR INVALID PAL PART TYPE */
    if (itype != 0) {
	goto L17;
    }
    io___56.ciunit = lunit_1.pms;
    s_wsfe(&io___56);
    do_fio(&c__3, (char *)&ipal[0], (ftnlen)sizeof(integer));
    do_fio(&c__5, (char *)&iname[0], (ftnlen)sizeof(integer));
    e_wsfe();
    sub_exit();
/*     GET 24 PIN NAMES */
L17:
    for (j = 1; j <= 24; ++j) {
/* L20: */
	getsym_(lphase, isym, &j, &ic, &il);
    }
    if (! (_BLNK__1.lequal || _BLNK__1.lleft || _BLNK__1.land || _BLNK__1.lor 
	    || _BLNK__1.lright)) {
	goto L24;
    }
    io___59.ciunit = lunit_1.pms;
    s_wsfe(&io___59);
    e_wsfe();
    sub_exit();
L24:
    ile = il;
/*     BYPASS FUSE PLOT ASSEMBLY IF HAL ('H' IN LINE 1, COLUMN 1) */
    if (ipal[0] == h__) {
	goto L108;
    }
L25:
    getsym_(lbuf, ibuf, &c__1, &ic, &il);
L28:
    if (! _BLNK__1.lequal) {
	goto L25;
    }
    ill = il;
    match_(&imatch, ibuf, isym);
    if (imatch == 0) {
	goto L100;
    }
/*         CHECK FOR VALID POLARITY (ACTIVE LOW) */
    lsame = lphase[imatch - 1] && lbuf[0] || ! lphase[imatch - 1] && ! lbuf[0]
	    ;
    if (itype != 6 && lsame) {
	lact = TRUE_;
    }
/*         CHECK FOR VALID OUTPUT PIN */
/* L29: */
    if ((itype == 1 || itype == 7 || itype == 8 || itype == 9 || itype == 10) 
	    && (imatch < 14 || imatch > 23)) {
	loperr = TRUE_;
    }
    if ((itype == 2 || itype == 11 || itype == 12 || itype == 13 || itype == 
	    14) && (imatch < 15 || imatch > 22)) {
	loperr = TRUE_;
    }
    if (itype == 3 && (imatch < 16 || imatch > 21)) {
	loperr = TRUE_;
    }
    if (itype == 4 && (imatch < 17 || imatch > 20)) {
	loperr = TRUE_;
    }
    if ((itype == 5 || itype == 6) && (imatch < 18 || imatch > 19)) {
	loperr = TRUE_;
    }
    if (lact || loperr) {
	goto L100;
    }
    i88pro = (23 - imatch << 3) + 1;
/*         START PAL20C1 ON PRODUCT LINE 32 (I88PRO=33) */
    if (iname[2] == c__) {
	i88pro = 33;
    }
    ic = 0;
L30:
    incr_(&ic, &il);
    if (! (_BLNK__1.lequal || _BLNK__1.lleft)) {
	goto L30;
    }
    lprod[i88pro - 1] = TRUE_;
    if (! _BLNK__1.lleft) {
	slip_(lfuses_1.lfuses, &i88pro, &itype, &iblow);
    }

    for (i8pro = 1; i8pro <= 16; ++i8pro) {
	if (_BLNK__1.lxor && i8pro != 3) {
	    goto L70;
	}
	iprod = i88pro + i8pro - 1;
	lprod[iprod - 1] = TRUE_;
	lfirst = TRUE_;
L50:
	ill = il;
	getsym_(lbuf, ibuf, &c__1, &ic, &il);
	match_(&imatch, ibuf, isym);
/*               CHECK FOR INVALID INPUT PIN */
	if (itype == 1 && (imatch >= 14 && imatch <= 23)) {
	    linp = TRUE_;
	}
	if (itype == 2 && (imatch >= 15 && imatch <= 22)) {
	    linp = TRUE_;
	}
	if (itype == 3 && (imatch >= 16 && imatch <= 21)) {
	    linp = TRUE_;
	}
	if (itype == 4 && (imatch >= 17 && imatch <= 20)) {
	    linp = TRUE_;
	}
	if (itype == 5 && (imatch == 18 || imatch == 19)) {
	    linp = TRUE_;
	}
	if (itype == 6 && (imatch == 18 || imatch == 19)) {
	    linp = TRUE_;
	}
	if (itype == 7 && (imatch == 14 || imatch == 23)) {
	    linp = TRUE_;
	}
	if ((real) itype == 8.f && (imatch == 1 || imatch == 13)) {
	    linp = TRUE_;
	}
	if ((real) itype == 9.f && (imatch == 1 || imatch == 13)) {
	    linp = TRUE_;
	}
	if ((real) itype == 10.f && (imatch == 1 || imatch == 13)) {
	    linp = TRUE_;
	}
	if (itype == 11 && (imatch == 15 || imatch == 22)) {
	    linp = TRUE_;
	}
	if (itype >= 12 && (imatch == 1 || imatch == 13)) {
	    linp = TRUE_;
	}
	ill = il;
	if (linp) {
	    goto L100;
	}
	if (imatch == 0) {
	    goto L100;
	}
	if (imatch == 12) {
	    goto L64;
	}
	if (! lfirst) {
	    goto L58;
	}
	lfirst = FALSE_;
	for (i__ = 1; i__ <= 40; ++i__) {
	    ++iblow;
/* L56: */
	    lfuses_1.lfuses[i__ + iprod * 40 - 41] = TRUE_;
	}
L58:
	ixlate_(&iinput, lphase, &imatch, lbuf, &itype);
	if (iinput <= 0) {
	    goto L60;
	}
	--iblow;
	lfuses_1.lfuses[iinput + iprod * 40 - 41] = FALSE_;
	plot_(lbuf, ibuf, lfuses_1.lfuses, &iprod, title, &c_false, &itype, 
		lprod, &iop, &iblow);
L60:
	if (_BLNK__1.land) {
	    goto L50;
	}
L64:
	if (! _BLNK__1.lright) {
	    goto L68;
	}
L66:
	incr_(&ic, &il);
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
    getsym_(lbuf, ibuf, &c__1, &ic, &il);

    if (_BLNK__1.lleft || _BLNK__1.lequal) {
	goto L28;
    }
L100:
    if (ill == ftest_1.ifunct || ill == ftest_1.idesc || ill == ftest_1.iend) 
	    {
	goto L102;
    }
/*     PRINT AN ERROR MESSAGE FOR AN UNRECOGNIZABLE SYMBOL */
    ilerr = ill + 4;
    io___74.ciunit = lunit_1.pms;
    s_wsfe(&io___74);
    do_fio(&c__1, (char *)&bel, (ftnlen)sizeof(integer));
    e_wsfe();
    io___75.ciunit = lunit_1.pms;
    s_wsfe(&io___75);
    for (i__ = 1; i__ <= 8; ++i__) {
	do_fio(&c__1, (char *)&ibuf[i__ - 1], (ftnlen)sizeof(integer));
    }
    do_fio(&c__1, (char *)&ilerr, (ftnlen)sizeof(integer));
    for (i__ = 1; i__ <= 80; ++i__) {
	do_fio(&c__1, (char *)&pge_1.ipage[i__ + ill * 80 - 81], (ftnlen)
		sizeof(integer));
    }
    e_wsfe();
/*     PRINT AN ERROR MESSAGE FOR ACTIVE HIGH/LOW ERRORS */
    if (lact && ! loperr) {
	io___76.ciunit = lunit_1.pms;
	s_wsfe(&io___76);
	do_fio(&c__3, (char *)&ipal[0], (ftnlen)sizeof(integer));
	do_fio(&c__5, (char *)&iname[0], (ftnlen)sizeof(integer));
	e_wsfe();
    }
/*     PRINT AN ERROR MESSAGE FOR AN INVALID OUTPUT PIN */
    if (loperr && imatch != 0) {
	io___77.ciunit = lunit_1.pms;
	s_wsfe(&io___77);
	do_fio(&c__1, (char *)&imatch, (ftnlen)sizeof(integer));
	do_fio(&c__3, (char *)&ipal[0], (ftnlen)sizeof(integer));
	do_fio(&c__5, (char *)&iname[0], (ftnlen)sizeof(integer));
	e_wsfe();
    }
/*     PRINT AN ERROR MESSAGE FOR AN INVALID INPUT PIN */
    if (linp) {
	io___78.ciunit = lunit_1.pms;
	s_wsfe(&io___78);
	do_fio(&c__1, (char *)&imatch, (ftnlen)sizeof(integer));
	do_fio(&c__3, (char *)&ipal[0], (ftnlen)sizeof(integer));
	do_fio(&c__5, (char *)&iname[0], (ftnlen)sizeof(integer));
	e_wsfe();
    }
    sub_exit();
L102:
    tweek_(&itype, lfuses_1.lfuses, lfuses_1.lphant);
L108:
    s_wsfe(&io___79);
    e_wsfe();
    s_wsfe(&io___80);
    e_wsfe();
    s_wsfe(&io___81);
    e_wsfe();
    io___82.ciunit = roc;
    s_rsfe(&io___82);
    do_fio(&c__1, (char *)&iop, (ftnlen)sizeof(integer));
    e_rsfe();
/*     CALL IODC2 */
    if (lunit_1.pof != 6) {
	io___83.ciunit = lunit_1.pof;
	s_wsfe(&io___83);
	e_wsfe();
    }
    if (iop == e) {
	echo_(ipal, iname, rest, patnum, title, comp);
    }
    if (iop == o) {
	pinout_(ipal, iname, title);
    }
    if (iop == t) {
	L__1 = iop != jj;
	test_(lphase, lbuf, title, &ic, &il, &ile, isym, ibuf, &itype, &ipctr,
		 &lerr, &isaf, &ipctr1, &c_false, &c_false, &L__1);
    }
    if (iop != jj) {
	goto L130;
    }
    L__1 = iop != jj;
    test_(lphase, lbuf, title, &ic, &il, &ile, isym, ibuf, &itype, &ipctr, &
	    lerr, &isaf, &ipctr1, &c_false, &c_false, &c_false, &L__1);
    plotf_(&itype, &iot);
L130:
    isaf = 0;
    if (iop == f) {
	goto L200;
    }
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
/*    (INTEL PROGRAMMING FORMAT) */
    if (iop == ii) {
	intel_(lfuses_1.lfuses, &ii);
    }
    if (iop != q) {
	goto L108;
    }
    sub_exit();
/*  SETTING THE PARAMETERS FOR THE SA0/SA1 TESTS */
L200:
    ipctr = 0;
    L__1 = iop != jj;
    test_(lphase, lbuf, title, &ic, &il, &ile, isym, ibuf, &itype, &ipctr, &
	    lerr, &isaf, &ipctr1, &c_false, &c_false, &L__1);
    ipctr0 = ipctr;
/*  LOOPING FOR SA1 TEST */
    i__1 = ipctr0;
    for (ipctr1 = 1; ipctr1 <= i__1; ++ipctr1) {
	lsa11 = TRUE_;
	L__1 = iop != jj;
	test_(lphase, lbuf, title, &ic, &il, &ile, isym, ibuf, &itype, &ipctr,
		 &lerr, &isaf, &ipctr1, &lsa11, &c_false, &L__1);
/* L210: */
    }
    isa1 = isaf;
/*   LOOPING FOR SA0 TEST */
    i__1 = ipctr0;
    for (ipctr1 = 1; ipctr1 <= i__1; ++ipctr1) {
	lsa01 = TRUE_;
	L__1 = iop != jj;
	test_(lphase, lbuf, title, &ic, &il, &ile, isym, ibuf, &itype, &ipctr,
		 &lerr, &isaf, &ipctr1, &c_false, &lsa01, &L__1);
/* L215: */
    }
    isa0 = isaf - isa1;
    ifault = isaf * 100 / (ipctr0 << 1);
    io___94.ciunit = lunit_1.pof;
    s_wsfe(&io___94);
    do_fio(&c__1, (char *)&isa1, (ftnlen)sizeof(integer));
    e_wsfe();
    io___95.ciunit = lunit_1.pof;
    s_wsfe(&io___95);
    do_fio(&c__1, (char *)&isa0, (ftnlen)sizeof(integer));
    e_wsfe();
    io___96.ciunit = lunit_1.pof;
    s_wsfe(&io___96);
    do_fio(&c__1, (char *)&ifault, (ftnlen)sizeof(integer));
    e_wsfe();
    goto L135;
} /* MAIN__ */

#undef l
#undef s
#undef h__
#undef b
#undef p
#undef t
#undef jj
#undef uu
#undef tt
#undef ss
#undef rr
#undef pp
#undef oo
#undef nn
#undef ii
#undef ff
#undef o
#undef ee
#undef dd
#undef cc
#undef bb
#undef f
#undef q
#undef c__
#undef n
#undef e



/* *********************************************************************** */

/* Subroutine */ int initlz_(integer *iname, integer *itype, logical *lfuses, 
	logical *lphant, integer *ic, integer *il, integer *iblow, integer *
	ipctr)
{
    /* Initialized data */

    static struct {
	char e_1[20];
	integer e_2;
	char e_3[20];
	integer e_4;
	char e_5[20];
	integer e_6;
	char e_7[20];
	integer e_8;
	char e_9[20];
	integer e_10;
	char e_11[20];
	integer e_12;
	char e_13[20];
	integer e_14;
	char e_15[20];
	integer e_16;
	char e_17[20];
	integer e_18;
	char e_19[20];
	integer e_20;
	char e_21[20];
	integer e_22;
	char e_23[20];
	integer e_24;
	char e_25[20];
	integer e_26;
	char e_27[20];
	integer e_28;
	} equiv_127 = { "1   2   L   1   0   ", 1, "1   4   L   8       ", 2, 
		"1   6   L   6       ", 3, "1   8   L   4       ", 4, "2   0"
		"   L   2       ", 5, "2   0   C   1       ", 6, "2   0   L  "
		" 1   0   ", 7, "2   0   X   1   0   ", 8, "2   0   X   8    "
		"   ", 9, "2   0   X   4       ", 10, "2   0   L   8       ", 
		11, "2   0   R   8       ", 12, "2   0   R   6       ", 13, 
		"2   0   R   4       ", 14 };

#define info ((integer *)&equiv_127)


    static integer i__, j;
    extern /* Subroutine */ int incr_(integer *, integer *);
    static logical lmatch;

/*     THIS SUBROUTINE INITIALIZIES VARIABLES AND MATCHES PAL PART */
/*     NUMBER WITH ITYPE */
    /* Parameter adjustments */
    lphant -= 41;
    lfuses -= 41;
    --iname;

    /* Function Body */

/*     INITIALIZE LFUSES ARRAY (FUSE ARRAY) */
    for (j = 1; j <= 80; ++j) {
	for (i__ = 1; i__ <= 40; ++i__) {
	    lphant[i__ + j * 40] = FALSE_;
/* L20: */
	    lfuses[i__ + j * 40] = FALSE_;
	}
    }
    tstvec_1.ntest = 0;
/*     INITIALIZE IBLOW (NUMBER OF FUSES BLOWN) */
    *iblow = 0;
    *ipctr = 0;
/*     INITIALIZE IC AND IL (COLUMN AND LINE POINTERS) */
    *ic = 0;
    *il = 1;
/*     INITIALIZE ITYPE (PAL PART TYPE) */
    *itype = 0;
/*     ITYPE IS ASSIGNED THE FOLLOWING VALUES FOR THESE PAL PART TYPES: */
/*     PAL12L10 =  1   PAL14L8  =  2   PAL16L6  =  3   PAL18L4  =  4 */
/*     PAL20L2  =  5   PAL20C1  =  6   PAL20L10 =  7   PAL20X10 =  8 */
/*     PAL20X8  =  9   PAL20X4  = 10   PAL20L8  = 11   PAL20R8  = 12 */
/*     PAL20R6  = 13   PAL20R4  = 14 */
    for (j = 1; j <= 14; ++j) {
	lmatch = TRUE_;
	for (i__ = 1; i__ <= 4; ++i__) {
/* L30: */
	    if (iname[i__] != info[i__ + j * 6 - 7]) {
		lmatch = FALSE_;
	    }
	}
	if (lmatch) {
	    *itype = info[j * 6 - 1];
	}
	if (lmatch) {
	    goto L50;
	}
/* L40: */
    }
    if (*itype == 0) {
	return 0;
    }
L50:
    incr_(ic, il);
    return 0;
} /* initlz_ */

#undef info



/* *********************************************************************** */

/* Subroutine */ int getsym_(logical *lphase, integer *isym, integer *j, 
	integer *ic, integer *il)
{
    /* Initialized data */

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_130 = { "    ", 0 };

#define iblank (*(integer *)&equiv_130)


    static integer i__;
    extern /* Subroutine */ int incr_(integer *, integer *);

/*     THIS SUBROUTINE GETS THE PIN NAME, / IF COMPLEMENT LOGIC, AND */
/*      THE FOLLOWING OPERATION SYMBOL IF ANY */
    /* Parameter adjustments */
    isym -= 9;
    --lphase;

    /* Function Body */
    if (! (_BLNK__1.lleft || _BLNK__1.land || _BLNK__1.lor || _BLNK__1.lequal 
	    || _BLNK__1.lright)) {
	goto L10;
    }
    incr_(ic, il);
L10:
    lphase[*j] = ! _BLNK__1.lslash;
    if (lphase[*j]) {
	goto L15;
    }
    incr_(ic, il);
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
    incr_(ic, il);
    if (_BLNK__1.lleft || _BLNK__1.lblank || _BLNK__1.land || _BLNK__1.lor || 
	    _BLNK__1.lright || _BLNK__1.lequal) {
	return 0;
    }
    goto L25;
} /* getsym_ */

#undef iblank



/* ********************************************************************** */

/* Subroutine */ int incr_(integer *ic, integer *il)
{
    /* Initialized data */

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_143 = { "    ", 0 };

#define iblank (*(integer *)&equiv_143)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_144 = { "(   ", 0 };

#define ileft (*(integer *)&equiv_144)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_145 = { "*   ", 0 };

#define iand (*(integer *)&equiv_145)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_146 = { "+   ", 0 };

#define ior (*(integer *)&equiv_146)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_147 = { ";   ", 0 };

#define coment (*(integer *)&equiv_147)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_148 = { "/   ", 0 };

#define islash (*(integer *)&equiv_148)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_149 = { "=   ", 0 };

#define iequal (*(integer *)&equiv_149)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_150 = { ")   ", 0 };

#define iright (*(integer *)&equiv_150)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_151 = { ":   ", 0 };

#define icolon (*(integer *)&equiv_151)

    static integer tab = 9;

    /* Format strings */
    static char fmt_15[] = "(/,\002 SOURCE FILE EXCEEDS 200 LINES OR MISSIN"
	    "G\002,\002 DESCRIPTION OR FUNCTION TABLE KEY WORD\002)";

    /* Local variables */
    extern /* Subroutine */ int sub_exit(void);
    static logical lxor1;

    /* Fortran I/O blocks */
    static cilist io___142 = { 0, 0, 0, fmt_15, 0 };


/*     THIS SUBROUTINE INCREMENTS COLUMN AND LINE POINTERS */
/*      BLANKS AND CHARACTERS AFTER ';' ARE IGNORED */
    _BLNK__1.lblank = FALSE_;
    _BLNK__1.lxor = FALSE_;
    lxor1 = FALSE_;
L10:
    ++(*ic);
    if (*ic <= 79 && pge_1.ipage[*ic + *il * 80 - 81] != coment) {
	goto L30;
    }
    ++(*il);
    if (*il <= 200) {
	goto L20;
    }
    io___142.ciunit = lunit_1.pms;
    s_wsfe(&io___142);
    e_wsfe();
    sub_exit();
L20:
    *ic = 0;
    goto L10;
L30:
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
    if (_BLNK__1.lxor) {
	goto L33;
    }
    lxor1 = TRUE_;
    goto L10;
L33:
    _BLNK__1.lor = TRUE_;
    return 0;
L32:
    if (! (pge_1.ipage[*ic + *il * 80 - 81] == ior && lxor1)) {
	goto L34;
    }
    _BLNK__1.lxor = TRUE_;
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
    static integer i__, j;
    static logical lmatch;

/*     THIS SUBROUTINE FINDS A MATCH BETWEEN THE PIN NAME IN THE EQUATION */

/*      AND THE PIN NAME IN THE PIN LIST OR FUNCTION TABLE PIN LIST */
    /* Parameter adjustments */
    isym -= 9;
    ibuf -= 9;

    /* Function Body */
    *imatch = 0;
    for (j = 1; j <= 24; ++j) {
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
    return 0;
} /* match_ */


/* *********************************************************************** */

/* Subroutine */ int ixlate_(integer *iinput, logical *lphase, integer *
	imatch, logical *lbuf, integer *itype)
{
    /* Initialized data */

    static integer itable[336]	/* was [24][14] */ = { 3,1,5,9,13,17,21,25,29,
	    33,37,0,39,0,0,0,0,0,0,0,0,0,0,0,3,1,5,9,13,17,21,25,29,33,37,0,
	    39,35,0,0,0,0,0,0,0,0,7,0,3,1,5,9,13,17,21,25,29,33,37,0,39,35,31,
	    0,0,0,0,0,0,11,7,0,3,1,5,9,13,17,21,25,29,33,37,0,39,35,31,27,0,0,
	    0,0,15,11,7,0,3,1,5,9,13,17,21,25,29,33,37,0,39,35,31,27,23,0,0,
	    19,15,11,7,0,3,1,5,9,13,17,21,25,29,33,37,0,39,35,31,27,23,0,0,19,
	    15,11,7,0,3,1,5,9,13,17,21,25,29,33,37,0,39,0,35,31,27,23,19,15,
	    11,7,0,0,0,1,5,9,13,17,21,25,29,33,37,0,0,39,35,31,27,23,19,15,11,
	    7,3,0,0,1,5,9,13,17,21,25,29,33,37,0,0,39,35,31,27,23,19,15,11,7,
	    3,0,0,1,5,9,13,17,21,25,29,33,37,0,0,39,35,31,27,23,19,15,11,7,3,
	    0,3,1,5,9,13,17,21,25,29,33,37,0,39,35,0,31,27,23,19,15,11,0,7,0,
	    0,1,5,9,13,17,21,25,29,33,37,0,0,39,35,31,27,23,19,15,11,7,3,0,0,
	    1,5,9,13,17,21,25,29,33,37,0,0,39,35,31,27,23,19,15,11,7,3,0,0,1,
	    5,9,13,17,21,25,29,33,37,0,0,39,35,31,27,23,19,15,11,7,3,0 };

    static integer ibubl;

/*     THIS SUBROUTINE FINDS A MATCH BETWEEN INPUT PIN NUMBER AND */
/*      THE INPUT LINE NUMBER FOR A SPECIFIC PAL.  ADD 1 TO THE INPUT */
/*      LINE NUMBER IF THE PIN IS A COMPLEMENT */
    /* Parameter adjustments */
    --lbuf;
    --lphase;

    /* Function Body */
    ibubl = 0;
    if (lphase[*imatch] && ! lbuf[1] || ! lphase[*imatch] && lbuf[1]) {
	ibubl = 1;
    }
    *iinput = itable[*imatch + *itype * 24 - 25] + ibubl;
    return 0;
} /* ixlate_ */


/* *********************************************************************** */

/* Subroutine */ int echo_(integer *ipal, integer *iname, integer *rest, 
	integer *patnum, integer *title, integer *comp)
{
    /* Initialized data */

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_163 = { "    ", 0 };

#define iblank (*(integer *)&equiv_163)


    /* Format strings */
    static char fmt_5[] = "(/,\002 \002,3a1,5a1,72a1,/,\002 \002,80a1,/"
	    ",\002 \002,80a1,/,\002 \002,80a1)";
    static char fmt_15[] = "(\002 \002,80a1)";

    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer i__, ic, il;

    /* Fortran I/O blocks */
    static cilist io___158 = { 0, 0, 0, fmt_5, 0 };
    static cilist io___161 = { 0, 0, 0, fmt_15, 0 };


/*     THIS SUBROUTINE PRINTS THE PAL DESIGN SPECIFICATION INPUT FILE */
    /* Parameter adjustments */
    --comp;
    --title;
    --patnum;
    --rest;
    --iname;
    --ipal;

    /* Function Body */
    io___158.ciunit = lunit_1.pof;
    s_wsfe(&io___158);
    do_fio(&c__3, (char *)&ipal[1], (ftnlen)sizeof(integer));
    do_fio(&c__5, (char *)&iname[1], (ftnlen)sizeof(integer));
    do_fio(&c__72, (char *)&rest[1], (ftnlen)sizeof(integer));
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
	io___161.ciunit = lunit_1.pof;
	s_wsfe(&io___161);
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
    static char fmt_10[] = "(/,\002 MONOLITHIC MEMORIES 24-PIN PALASM VERSIO"
	    "N 1.6C\002)";
    static char fmt_15[] = "(\002 (C) COPYRIGHT 1983 MONOLITHIC MEMORIES\002)"
	    ;
    static char fmt_20[] = "(/,\002 THIS PALASM AIDS THE USER IN THE DESIGN "
	    "AND\002,\002 PROGRAMMING OF THE\002,/,\002 SERIES 24 PAL FAMILY."
	    "  THE\002,\002 FOLLOWING OPTIONS ARE PROVIDED:\002,/,\002    ECH"
	    "O (E)     - PRINTS THE PAL DESIGN\002,\002 SPECIFICATION\002,//"
	    ",\002    PINOUT (O)   - PRINTS THE PINOUT OF THE PAL\002,//,\002"
	    "    SIMULATE (T) - EXERCISES THE FUNCTION TABLE\002,\002 VECTORS"
	    " IN THE LOGIC\002,/,\002                  \002,\002 EQUATIONS AN"
	    "D GENERATES TEST VECTORS\002,//,\002    FAULT (F)    - PERFORMS "
	    "FAULT TESTING\002,//,\002    PLOT (P)     - PRINTS THE ENTIRE FU"
	    "SE PLOT\002)";
    static char fmt_30[] = "(/,\002    BRIEF (B)    - PRINTS ONLY THE USED P"
	    "RODUCT LINES\002,\002 OF THE FUSE PLOT\002,/,\002               "
	    "    PHANTOM\002,\002 FUSES ARE OMITTED\002,//,\002    JEDEC (J) "
	    "   - GENERATES JEDEC PROGRAMMING FORMAT\002,//,\002    HEX (H)  "
	    "    - GENERATES HEX PROGRAMMING FORMAT\002,//,\002    SHORT (S) "
	    "   - GENERATES HEX PROGRAMMING FORMAT\002,//,\002    BHLF (L)   "
	    "  - GENERATES BHLF PROGRAMMING FORMAT\002,//,\002    BNPF (N)   "
	    "  - GENERATES BNPF PROGRAMMING FORMAT\002,//,\002    INTEL (I)  "
	    "  - GENERATES INTEL PROGRAMMING FORMAT\002,//,\002    CATALOG (C"
	    ")  - PRINTS THE PALASM CATALOG\002,//,\002    QUIT (Q)     - EXI"
	    "T PALASM\002)";

    /* Fortran I/O blocks */
    static cilist io___164 = { 0, 0, 0, fmt_10, 0 };
    static cilist io___165 = { 0, 0, 0, fmt_15, 0 };
    static cilist io___166 = { 0, 0, 0, fmt_20, 0 };
    static cilist io___167 = { 0, 0, 0, fmt_30, 0 };


/*     THIS SUBROUTINE PRINTS THE PALASM CATALOG */
    io___164.ciunit = lunit_1.pms;
    s_wsfe(&io___164);
    e_wsfe();
    io___165.ciunit = lunit_1.pms;
    s_wsfe(&io___165);
    e_wsfe();
    io___166.ciunit = lunit_1.pms;
    s_wsfe(&io___166);
    e_wsfe();
    io___167.ciunit = lunit_1.pms;
    s_wsfe(&io___167);
    e_wsfe();
    return 0;
} /* cat_ */


/* *********************************************************************** */

/* Subroutine */ int pinout_(integer *ipal, integer *iname, integer *title)
{
    /* Initialized data */

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_185 = { "    ", 0 };

#define iblank (*(integer *)&equiv_185)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_186 = { "*   ", 0 };

#define istar (*(integer *)&equiv_186)


    /* Format strings */
    static char fmt_76[] = "(/,\002 \002,80a1)";
    static char fmt_78[] = "(/,\002 \002,18x,14a1,3x,14a1,/,\002 \002,18x,a1"
	    ",13x,a1,1x,a1,13x,a1)";
    static char fmt_80[] = "(\002 \002,15x,4a1,29x,4a1)";
    static char fmt_81[] = "(\002 \002,12a1,3x,a1,i2,a1,11x,8a1,10x,a1,i2,a1"
	    ",3x,12a1)";
    static char fmt_82[] = "(\002 \002,15x,4a1,29x,4a1)";
    static char fmt_84[] = "(\002 \002,18x,a1,11x,8a1,10x,a1)";
    static char fmt_90[] = "(\002 \002,18x,31a1)";

    /* Local variables */
    static integer i__, j, ic, ii, jj, il, iin[16]	/* was [8][2] */, pin[
	    288]	/* was [12][24] */;

    /* Fortran I/O blocks */
    static cilist io___177 = { 0, 0, 0, fmt_76, 0 };
    static cilist io___178 = { 0, 0, 0, fmt_78, 0 };
    static cilist io___180 = { 0, 0, 0, fmt_80, 0 };
    static cilist io___181 = { 0, 0, 0, fmt_81, 0 };
    static cilist io___182 = { 0, 0, 0, fmt_82, 0 };
    static cilist io___183 = { 0, 0, 0, fmt_84, 0 };
    static cilist io___184 = { 0, 0, 0, fmt_90, 0 };


/*     THIS SUBROUTINE PRINTS THE PINOUT OF THE PAL */
    /* Parameter adjustments */
    --title;
    --iname;
    --ipal;

    /* Function Body */
    for (j = 1; j <= 24; ++j) {
	for (i__ = 1; i__ <= 12; ++i__) {
/* L5: */
	    pin[i__ + j * 12 - 13] = iblank;
	}

/* L10: */
    }
/* L15: */
    for (j = 1; j <= 2; ++j) {
	for (i__ = 1; i__ <= 8; ++i__) {
/* L20: */
	    iin[i__ + (j << 3) - 9] = iblank;
	}
/* L25: */
    }
    iin[1] = ipal[1];
    iin[3] = ipal[2];
    iin[5] = ipal[3];
    iin[8] = iname[1];
    iin[10] = iname[2];
    iin[12] = iname[3];
    iin[14] = iname[4];
    iin[15] = iname[5];
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
    if (j > 24) {
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
    for (j = 1; j <= 12; ++j) {
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
    io___177.ciunit = lunit_1.pof;
    s_wsfe(&io___177);
    do_fio(&c__80, (char *)&title[1], (ftnlen)sizeof(integer));
    e_wsfe();
    io___178.ciunit = lunit_1.pof;
    s_wsfe(&io___178);
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
    jj = 24;
    for (j = 1; j <= 12; ++j) {
	io___180.ciunit = lunit_1.pof;
	s_wsfe(&io___180);
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	e_wsfe();
	io___181.ciunit = lunit_1.pof;
	s_wsfe(&io___181);
	for (i__ = 1; i__ <= 12; ++i__) {
	    do_fio(&c__1, (char *)&pin[i__ + j * 12 - 13], (ftnlen)sizeof(
		    integer));
	}
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&j, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	for (i__ = 1; i__ <= 8; ++i__) {
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
	io___182.ciunit = lunit_1.pof;
	s_wsfe(&io___182);
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	e_wsfe();

	io___183.ciunit = lunit_1.pof;
	s_wsfe(&io___183);
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	for (i__ = 1; i__ <= 8; ++i__) {
	    do_fio(&c__1, (char *)&iin[i__ + 7], (ftnlen)sizeof(integer));
	}
	do_fio(&c__1, (char *)&istar, (ftnlen)sizeof(integer));
	e_wsfe();
	for (ii = 1; ii <= 2; ++ii) {
	    for (i__ = 1; i__ <= 8; ++i__) {
/* L85: */
		iin[i__ + (ii << 3) - 9] = iblank;
	    }
/* L86: */
	}
	--jj;
/* L88: */
    }
    io___184.ciunit = lunit_1.pof;
    s_wsfe(&io___184);
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
	char e_1[12800];
	integer e_2;
	} equiv_220 = { "                                                   "
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
		"                             ", 0 };

#define isave ((integer *)&equiv_220)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_221 = { "*   ", 0 };

#define iand (*(integer *)&equiv_221)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_222 = { "/   ", 0 };

#define islash (*(integer *)&equiv_222)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_223 = { "-   ", 0 };

#define idash (*(integer *)&equiv_223)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_224 = { "X   ", 0 };

#define x (*(integer *)&equiv_224)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_225 = { "    ", 0 };

#define iblank (*(integer *)&equiv_225)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_226 = { "P   ", 0 };

#define p (*(integer *)&equiv_226)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_227 = { "B   ", 0 };

#define b (*(integer *)&equiv_227)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_228 = { "D   ", 0 };

#define d__ (*(integer *)&equiv_228)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_229 = { "0   ", 0 };

#define zero (*(integer *)&equiv_229)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_230 = { "1   ", 0 };

#define one (*(integer *)&equiv_230)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_231 = { "0   ", 0 };

#define fx (*(integer *)&equiv_231)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_232 = { "O   ", 0 };

#define fidash (*(integer *)&equiv_232)

    static integer stx = 2;
    static integer etx = 3;

    /* Format strings */
    static char fmt_61[] = "(/,\002 \002,80a1,//,\002                11 1111"
	    " 1111 2222 2222 2233 3333 3333\002,/,\002    0123 4567 8901 2345"
	    " 6789 0123 4567 8901 2345 6789\002,/)";
    static char fmt_63[] = "(\002 \002,a1,\002*L0000\002/)";
    static char fmt_84[] = "(\002 \002,40(a1,\002 \002))";
    static char fmt_90[] = "(\002 \002,i2,10(\002 \002,4a1),\002 \002,24a1)";
    static char fmt_96[] = "(1x)";
    static char fmt_101[] = "(\002 \002,a1)";
    static char fmt_110[] = "(/,\002 LEGEND:  X : FUSE NOT BLOWN (L,N,0)   -"
	    " : FUSE BLOWN   (H,P,1)\002)";
    static char fmt_111[] = "(\002          0 : PHANTOM FUSE   (L,N,0)   O :"
	    " PHANTOM FUSE (H,P,1)\002)";
    static char fmt_112[] = "(/,\002 NUMBER OF FUSES BLOW = \002,i4)";
    static char fmt_113[] = "(//)";

    /* Local variables */
    static integer i__, j, k, iout[64], i8pro, idata[40], i88pro;
    extern /* Subroutine */ int fantom_(integer *, integer *, integer *, 
	    integer *, integer *);

    /* Fortran I/O blocks */
    static cilist io___205 = { 0, 0, 0, fmt_61, 0 };
    static cilist io___206 = { 0, 0, 0, fmt_63, 0 };
    static cilist io___212 = { 0, 0, 0, fmt_84, 0 };
    static cilist io___213 = { 0, 0, 0, fmt_90, 0 };
    static cilist io___214 = { 0, 0, 0, fmt_96, 0 };
    static cilist io___215 = { 0, 0, 0, fmt_101, 0 };
    static cilist io___216 = { 0, 0, 0, fmt_110, 0 };
    static cilist io___217 = { 0, 0, 0, fmt_111, 0 };
    static cilist io___218 = { 0, 0, 0, fmt_112, 0 };
    static cilist io___219 = { 0, 0, 0, fmt_113, 0 };


/*     THIS SUBROUTINE PRODUCES THE FUSE PLOT */
    /* Parameter adjustments */
    --lprod;
    --title;
    lfuses -= 41;
    ibuf -= 9;
    --lbuf;

    /* Function Body */
    if (*ldump) {
	goto L58;
    }
    if (isave[*iprod - 1] != iblank) {
	return 0;
    }
    if (lbuf[1]) {
	goto L5;
    }
    for (j = 1; j <= 39; ++j) {
/* L30: */
	isave[*iprod + j * 80 - 81] = isave[*iprod + (j + 1) * 80 - 81];
    }
    isave[*iprod + 3119] = islash;
L5:
    for (i__ = 1; i__ <= 8; ++i__) {
	if (isave[*iprod - 1] != iblank) {
	    return 0;
	}
	if (ibuf[i__ + 8] == iblank) {
	    goto L20;
	}
	for (j = 1; j <= 39; ++j) {
/* L10: */
	    isave[*iprod + j * 80 - 81] = isave[*iprod + (j + 1) * 80 - 81];
	}
	isave[*iprod + 3119] = ibuf[i__ + 8];
L20:
	;
    }
    if (isave[*iprod - 1] != iblank) {
	return 0;
    }
/* L40: */
    for (j = 1; j <= 39; ++j) {
/* L50: */
	isave[*iprod + j * 80 - 81] = isave[*iprod + (j + 1) * 80 - 81];
    }
    isave[*iprod + 3119] = iand;
    return 0;
/*     PRINT FUSE PLOT */
L58:
    if (*iop == d__) {
	goto L62;
    }
    io___205.ciunit = lunit_1.pof;
    s_wsfe(&io___205);
    do_fio(&c__80, (char *)&title[1], (ftnlen)sizeof(integer));
    e_wsfe();
    goto L64;
/* ***** STX DETERMINES THE STARTING CHARACTER FOR DATA I/O FORMAT */
L62:
    io___206.ciunit = lunit_1.pdf;
    s_wsfe(&io___206);
    do_fio(&c__1, (char *)&stx, (ftnlen)sizeof(integer));
    e_wsfe();

L64:
    for (i88pro = 1; i88pro <= 73; i88pro += 8) {
	for (i8pro = 1; i8pro <= 8; ++i8pro) {
	    *iprod = i88pro + i8pro - 1;
	    isave[*iprod + 3119] = iblank;
	    for (i__ = 1; i__ <= 40; ++i__) {
		if (isave[*iprod - 1] != iblank) {
		    goto L70;
		}
		for (j = 1; j <= 39; ++j) {
/* L65: */
		    isave[*iprod + j * 80 - 81] = isave[*iprod + (j + 1) * 80 
			    - 81];
		}
		isave[*iprod + 3119] = iblank;
L70:
		;
	    }
	    for (i__ = 1; i__ <= 24; ++i__) {
		iout[i__ + 39] = isave[*iprod + i__ * 80 - 81];
/* L75: */
	    }
	    if (isave[*iprod + 1919] != iblank) {
		iout[63] = idash;
	    }
	    for (i__ = 1; i__ <= 40; ++i__) {
		iout[i__ - 1] = x;
		if (lfuses[i__ + *iprod * 40]) {
		    iout[i__ - 1] = idash;
		}
/* L80: */
	    }
	    fantom_(itype, iop, iout, iprod, &i8pro);
	    if (*iop != d__) {
		goto L85;
	    }
	    k = 0;
/* L81: */
	    for (i__ = 1; i__ <= 40; ++i__) {
		if (iout[i__ - 1] == fx || iout[i__ - 1] == fidash) {
		    goto L82;
		}
		++k;
		if (iout[i__ - 1] == x) {
		    idata[k - 1] = zero;
		}
		if (iout[i__ - 1] == idash) {
		    idata[k - 1] = one;
		}
L82:
		;
	    }
	    for (i__ = 1; i__ <= 40; ++i__) {
		if (iout[i__ - 1] == x || iout[i__ - 1] == idash) {
		    goto L86;
		}
/* L83: */
	    }
	    goto L94;
L86:
	    io___212.ciunit = lunit_1.pdf;
	    s_wsfe(&io___212);
	    do_fio(&c__40, (char *)&idata[0], (ftnlen)sizeof(integer));
	    e_wsfe();
	    goto L94;
L85:
	    --(*iprod);
	    if (*iop == p || *iop == b && lprod[*iprod + 1]) {
		io___213.ciunit = lunit_1.pof;
		s_wsfe(&io___213);
		do_fio(&c__1, (char *)&(*iprod), (ftnlen)sizeof(integer));
		do_fio(&c__64, (char *)&iout[0], (ftnlen)sizeof(integer));
		e_wsfe();
	    }
L94:
	    ;
	}
	io___214.ciunit = lunit_1.pof;
	s_wsfe(&io___214);
	e_wsfe();
/* L100: */
    }
    if (*iop != d__) {
	goto L105;
    }
    io___215.ciunit = lunit_1.pdf;
    s_wsfe(&io___215);
    do_fio(&c__1, (char *)&etx, (ftnlen)sizeof(integer));
    e_wsfe();
    return 0;
L105:
    io___216.ciunit = lunit_1.pof;
    s_wsfe(&io___216);
    e_wsfe();
    if (*iop == p) {
	io___217.ciunit = lunit_1.pof;
	s_wsfe(&io___217);
	e_wsfe();
    }
    io___218.ciunit = lunit_1.pof;
    s_wsfe(&io___218);
    do_fio(&c__1, (char *)&(*iblow), (ftnlen)sizeof(integer));
    e_wsfe();
    io___219.ciunit = lunit_1.pof;
    s_wsfe(&io___219);
    e_wsfe();

    return 0;
} /* plot_ */

#undef fidash
#undef fx
#undef one
#undef zero
#undef d__
#undef b
#undef p
#undef iblank
#undef x
#undef idash
#undef islash
#undef iand
#undef isave



/* *********************************************************************** */

/* Subroutine */ int plotf_(integer *itype, integer *iot)
{
    /* Initialized data */

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_261 = { "0   ", 0 };

#define zero (*(integer *)&equiv_261)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_262 = { "1   ", 0 };

#define one (*(integer *)&equiv_262)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_263 = { "L   ", 0 };

#define l (*(integer *)&equiv_263)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_264 = { "H   ", 0 };

#define h__ (*(integer *)&equiv_264)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_265 = { "X   ", 0 };

#define x (*(integer *)&equiv_265)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_266 = { "C   ", 0 };

#define c__ (*(integer *)&equiv_266)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_267 = { "Z   ", 0 };

#define z__ (*(integer *)&equiv_267)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_268 = { "0   ", 0 };

#define n0 (*(integer *)&equiv_268)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_269 = { "1   ", 0 };

#define n1 (*(integer *)&equiv_269)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_270 = { "*   ", 0 };

#define star (*(integer *)&equiv_270)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_271 = { "N   ", 0 };

#define nn (*(integer *)&equiv_271)


    /* Format strings */
    static char fmt_10[] = "(\002 \002,a1,\002*D22\002,2i1,\002*F0*\002)";
    static char fmt_201[] = "(\002 L\002,4a1,\002 \002,40a1,a1)";
    static char fmt_410[] = "(\002 V\002,4i1,1x,24a1,\002 *\002)";
    static char fmt_400[] = "(\002 \002,a1,4a1,/)";

    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer i__, j, j1, j2, etx, stx, iadr, iinp;
    extern integer iconv_(integer *);
    static integer idecio[4];
    extern /* Subroutine */ int sumchk_(void);
    static integer isumio[4], pinout[2];

    /* Fortran I/O blocks */
    static cilist io___250 = { 0, 0, 0, fmt_10, 0 };
    static cilist io___256 = { 0, 0, 0, fmt_201, 0 };
    static cilist io___258 = { 0, 0, 0, fmt_410, 0 };
    static cilist io___260 = { 0, 0, 0, fmt_400, 0 };


/*     THIS SUBROUTINE GENERATES THE JEDEC PROGRAMMING FORMAT WHICH IS */
/*      COMPATIBLE WITH THE DATA I/O PROGRAMMABLE LOGIC PAK (PLDS) */
    iadr = 0;
    stx = 2;
    etx = 3;
/* 	DATA I/O PROGRAMMING ASSIGNMENT */
    if (*itype != 11) {
	goto L444;
    }
    pinout[0] = 2;
    pinout[1] = 6;
    goto L999;
L444:
    if (*itype != 12 && *itype != 14 && *itype != 13) {
	goto L555;
    }
    pinout[0] = 2;
    pinout[1] = 7;
    goto L999;
L555:
    if (*itype <= 5) {
	goto L666;
    }
    pinout[0] = 0;
    pinout[1] = 6;
    goto L999;
L666:
    pinout[1] = *itype;
    if (*iot == h__) {
	*itype += 6;
    }
    pinout[0] = pinout[1] / 10 % 10;
    pinout[1] %= 10;
L999:
    sum_1.isum[3] = pinout[0] + 536 + pinout[1];
    sum_1.isum[1] = sum_1.isum[3] / 256 % 256;
    sum_1.isum[3] %= 256;
    io___250.ciunit = lunit_1.pdf;
    s_wsfe(&io___250);
    do_fio(&c__1, (char *)&stx, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&pinout[0], (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&pinout[1], (ftnlen)sizeof(integer));
    e_wsfe();
    for (ipt_1.ipt = 1; ipt_1.ipt <= 80; ++ipt_1.ipt) {
	sum_1.nfuse = 0;
	for (iinp = 1; iinp <= 40; ++iinp) {
	    if (lfuses_1.lphant[iinp + ipt_1.ipt * 40 - 41]) {
		goto L50;
	    }
	    ++sum_1.nfuse;
	    if (lfuses_1.lfuses[iinp + ipt_1.ipt * 40 - 41]) {
		sum_1.ipbuf[sum_1.nfuse - 1] = one;
	    }
	    if (! lfuses_1.lfuses[iinp + ipt_1.ipt * 40 - 41]) {
		sum_1.ipbuf[sum_1.nfuse - 1] = zero;
	    }
L50:
	    ;
	}
	if (lfuses_1.lfuses[ipt_1.ipt * 40 - 40]) {
	    goto L100;
	}
	if (! lfuses_1.lfuses[ipt_1.ipt * 40 - 39]) {
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
	io___256.ciunit = lunit_1.pdf;
	s_wsfe(&io___256);
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
/* L300: */
    }
    if (tstvec_2.ntest == 0) {
	goto L371;
    }
    if (tstvec_2.ntest > 42) {
	tstvec_2.ntest = 42;
    }
    i__1 = tstvec_2.ntest;
    for (j = 1; j <= i__1; ++j) {
	sum_1.isum[3] += 215;
	sum_1.isum[1] = (sum_1.isum[1] + sum_1.isum[3] / 256) % 256;
	sum_1.isum[3] %= 256;
	for (i__ = 1; i__ <= 24; ++i__) {
	    j1 = tstvec_2.tstvec[i__ + j * 24 - 25];
	    if (j1 == c__) {
		sum_1.isum[3] += 67;
	    }
	    if (j1 == x) {
		sum_1.isum[3] += 88;
	    }
	    if (j1 == nn) {
		sum_1.isum[3] += 78;
	    }
	    if (j1 == l) {
		sum_1.isum[3] += 76;
	    }
	    if (j1 == h__) {
		sum_1.isum[3] += 72;
	    }
	    if (j1 == z__) {
		sum_1.isum[3] += 90;
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
	io___258.ciunit = lunit_1.pdf;
	s_wsfe(&io___258);
	do_fio(&c__4, (char *)&idecio[0], (ftnlen)sizeof(integer));
	for (i__ = 1; i__ <= 24; ++i__) {
	    do_fio(&c__1, (char *)&tstvec_2.tstvec[i__ + j * 24 - 25], (
		    ftnlen)sizeof(integer));
	}
	e_wsfe();
/* L370: */
    }
L371:
    i__1 = sum_1.isum[1] / 16;
    isumio[0] = iconv_(&i__1);
    sum_1.isum[1] %= 16;
    isumio[1] = iconv_(&sum_1.isum[1]);
    i__1 = sum_1.isum[3] / 16;
    isumio[2] = iconv_(&i__1);
    sum_1.isum[3] %= 16;
    isumio[3] = iconv_(&sum_1.isum[3]);
    io___260.ciunit = lunit_1.pdf;
    s_wsfe(&io___260);
    do_fio(&c__1, (char *)&etx, (ftnlen)sizeof(integer));
    do_fio(&c__4, (char *)&isumio[0], (ftnlen)sizeof(integer));
    e_wsfe();
    return 0;
} /* plotf_ */

#undef nn
#undef star
#undef n1
#undef n0
#undef z__
#undef c__
#undef x
#undef h__
#undef l
#undef one
#undef zero



/* *********************************************************************** */

/* Subroutine */ int sumchk_(void)
{
    static integer j;

/*     THIS SUBROUTINE GENERATES THE CHECK SUM FOR THE JEDEC PROGRAMMING */
/*     FORMAT */
    for (j = 1; j <= 40; ++j) {
	if (lfuses_1.lphant[j + ipt_1.ipt * 40 - 41]) {
	    goto L5;
	}
	if (lfuses_1.lfuses[j + ipt_1.ipt * 40 - 41]) {
	    sum_1.bufio[j - 1] = 49;
	}
	if (! lfuses_1.lfuses[j + ipt_1.ipt * 40 - 41]) {
	    sum_1.bufio[j - 1] = 48;
	}
	sum_1.isum[3] += sum_1.bufio[j - 1];
	sum_1.isum[1] = (sum_1.isum[1] + sum_1.isum[3] / 256) % 256;
	sum_1.isum[3] %= 256;
L5:
	;
    }

    for (j = 1; j <= 4; ++j) {
	sum_1.isum[3] = sum_1.isum[3] + sum_1.idec[j - 1] + 48;
	sum_1.isum[1] = (sum_1.isum[1] + sum_1.isum[3] / 256) % 256;
	sum_1.isum[3] %= 256;
/* L10: */
    }
    sum_1.isum[3] += 173;
    sum_1.isum[1] = (sum_1.isum[1] + sum_1.isum[3] / 256) % 256;
    sum_1.isum[3] %= 256;
    return 0;
} /* sumchk_ */


/* *********************************************************************** */

integer iconv_(integer *k)
{
    /* Initialized data */

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_289 = { "0   ", 0 };

#define a (*(integer *)&equiv_289)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_290 = { "1   ", 0 };

#define b (*(integer *)&equiv_290)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_291 = { "2   ", 0 };

#define c__ (*(integer *)&equiv_291)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_292 = { "3   ", 0 };

#define d__ (*(integer *)&equiv_292)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_293 = { "4   ", 0 };

#define e (*(integer *)&equiv_293)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_294 = { "5   ", 0 };

#define f (*(integer *)&equiv_294)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_295 = { "6   ", 0 };

#define g (*(integer *)&equiv_295)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_296 = { "7   ", 0 };

#define h__ (*(integer *)&equiv_296)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_297 = { "8   ", 0 };

#define i__ (*(integer *)&equiv_297)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_298 = { "9   ", 0 };

#define j (*(integer *)&equiv_298)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_299 = { "A   ", 0 };

#define x (*(integer *)&equiv_299)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_300 = { "B   ", 0 };

#define l (*(integer *)&equiv_300)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_301 = { "C   ", 0 };

#define m (*(integer *)&equiv_301)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_302 = { "D   ", 0 };

#define n (*(integer *)&equiv_302)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_303 = { "E   ", 0 };

#define o (*(integer *)&equiv_303)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_304 = { "F   ", 0 };

#define p (*(integer *)&equiv_304)


    /* System generated locals */
    integer ret_val;

/*     THIS INTEGER FUNCTION CONVERTS DECIMAL INTO ASCII CODE FOR THE */
/*      JEDEC PROGRAMMING FORMAT */
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

/* Subroutine */ int hex_(logical *lfuses, integer *iop)
{
    /* Initialized data */

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_333 = { "H   ", 0 };

#define h__ (*(integer *)&equiv_333)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_334 = { "S   ", 0 };

#define s (*(integer *)&equiv_334)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_335 = { "    ", 0 };

#define iblank (*(integer *)&equiv_335)

    static struct {
	char e_1[128];
	integer e_2;
	} equiv_336 = { "00  01  02  03  04  05  06  07  08  09  0A  0B  0C "
		" 0D  0E  0F  10  11  12  13  14  15  16  17  18  19  1A  1B "
		" 1C  1D  1E  1F  ", 0 };

#define ztabl1 ((integer *)&equiv_336)

    static struct {
	char e_1[64];
	integer e_2;
	} equiv_337 = { "0   1   2   3   4   5   6   7   8   9   A   B   C  "
		" D   E   F   ", 0 };

#define ztabl2 ((integer *)&equiv_337)

    static integer soh = 1;
    static integer stx = 2;
    static integer etx = 3;
    static integer bel = 7;

    /* Format strings */
    static char fmt_10[] = "(//,80(\002 \002),//)";
    static char fmt_5[] = "(\002 \002,9a1)";
    static char fmt_60[] = "(4(\002 \002,20(a2,\002 \002),\002.\002,/))";
    static char fmt_61[] = "(4(\002 \002,20a2,\002.\002,/))";
    static char fmt_70[] = "(//,80(\002 \002),//)";
    static char fmt_80[] = "(\002 \002,a1)";
    static char fmt_90[] = "(/,\002 HEX CHECK SUM = \002,4a1)";

    /* Local variables */
    static integer i__, j, inc, ihex, csum, isum2, iprod, itemp[80], ztemp, 
	    zcsum[4], isumx, iinput;

    /* Fortran I/O blocks */
    static cilist io___315 = { 0, 0, 0, fmt_10, 0 };
    static cilist io___316 = { 0, 0, 0, fmt_5, 0 };
    static cilist io___326 = { 0, 0, 0, fmt_60, 0 };
    static cilist io___327 = { 0, 0, 0, fmt_61, 0 };
    static cilist io___328 = { 0, 0, 0, fmt_70, 0 };
    static cilist io___329 = { 0, 0, 0, fmt_80, 0 };
    static cilist io___332 = { 0, 0, 0, fmt_90, 0 };


/*     THIS SUBROUTINE GENERATES HEX PROGRAMMING FORMATS */
    /* Parameter adjustments */
    lfuses -= 41;

    /* Function Body */
    csum = 0;

    if (*iop == h__) {
	io___315.ciunit = lunit_1.pdf;
	s_wsfe(&io___315);
	e_wsfe();
    }
/* ***** NOTE: SOME PROM PROGRAMMERS NEED A START CHARACTER. */
/* *****       THIS PROGRAM OUTPUTS AN STX FOR THE DATA I/O MODEL 9 */
/* *****         (USE SOH FOR MODEL 5) */
    io___316.ciunit = lunit_1.pdf;
    s_wsfe(&io___316);
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
    for (i__ = 1; i__ <= 41; i__ += 40) {
	inc = i__ - 1;
	for (iprod = 1; iprod <= 7; iprod += 2) {
	    for (j = 1; j <= 2; ++j) {
		for (iinput = 1; iinput <= 40; ++iinput) {
		    ihex = 0;
		    isum2 = iprod + j - 1 + inc;
		    if (lfuses[iinput + isum2 * 40]) {
			++ihex;
		    }
		    if (lfuses[iinput + (isum2 + 8) * 40]) {
			ihex += 2;
		    }
		    if (lfuses[iinput + (isum2 + 16) * 40]) {
			ihex += 4;
		    }
		    if (lfuses[iinput + (isum2 + 24) * 40]) {
			ihex += 8;
		    }
		    if (lfuses[iinput + (isum2 + 32) * 40]) {
			ihex += 16;
		    }
		    csum += ihex;
		    isumx = iinput + (j - 1) * 40;
/* L20: */
		    itemp[isumx - 1] = ztabl1[ihex];
		}
	    }
	    if (*iop == h__) {
		io___326.ciunit = lunit_1.pdf;
		s_wsfe(&io___326);
		do_fio(&c__80, (char *)&itemp[0], (ftnlen)sizeof(integer));
		e_wsfe();
	    }
/* L40: */
	    if (*iop == s) {
		io___327.ciunit = lunit_1.pdf;
		s_wsfe(&io___327);
		do_fio(&c__80, (char *)&itemp[0], (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	}
    }
    if (*iop == h__) {
	io___328.ciunit = lunit_1.pdf;
	s_wsfe(&io___328);
	e_wsfe();
    }
    io___329.ciunit = lunit_1.pdf;
    s_wsfe(&io___329);
    do_fio(&c__1, (char *)&etx, (ftnlen)sizeof(integer));
    e_wsfe();
/*     CONVERT DECIMAL CHECK SUM INTO HEX CHECK SUM */
    for (i__ = 1; i__ <= 4; ++i__) {
	ztemp = csum - (csum / 16 << 4);
	zcsum[5 - i__ - 1] = ztabl2[ztemp];
	csum /= 16;
/* L85: */
    }
    if (zcsum[0] == ztabl2[0]) {
	zcsum[0] = iblank;
    }
    io___332.ciunit = lunit_1.pms;
    s_wsfe(&io___332);
    do_fio(&c__1, (char *)&zcsum[0], (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&zcsum[1], (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&zcsum[2], (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&zcsum[3], (ftnlen)sizeof(integer));
    e_wsfe();
    return 0;
} /* hex_ */

#undef ztabl2
#undef ztabl1
#undef iblank
#undef s
#undef h__



/* *********************************************************************** */




/* *********************************************************************** */

/* Subroutine */ int tweek_(integer *itype, logical *lfuses, logical *lphant)
{
    /* Initialized data */

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_346 = { "P   ", 0 };

#define p (*(integer *)&equiv_346)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_347 = { "N   ", 0 };

#define n (*(integer *)&equiv_347)


    static integer col, prline, grtype, lntype, fusptr, output;

/*     THIS SUBROUTINE TWEEKS LFUSES (THE PROGRAMMING FUSE PLOT) */
/*      FOR HIGH AND LOW PHANTOM FUSES */
    /* Parameter adjustments */
    lphant -= 41;
    lfuses -= 41;

    /* Function Body */
    fusptr = 1;
    for (output = 1; output <= 10; ++output) {
	grtype = blk_1.pr8x10[output + *itype * 10 - 11];
	for (prline = 1; prline <= 8; ++prline) {
	    lntype = blk_1.prod8[prline + (grtype << 3) - 9];
	    for (col = 1; col <= 40; ++col) {
		if (blk_1.prodln[col + lntype * 40 - 41] != p) {
		    goto L15;
		}
		lfuses[col + fusptr * 40] = TRUE_;
		lphant[col + fusptr * 40] = TRUE_;
L15:
		if (blk_1.prodln[col + lntype * 40 - 41] != n) {
		    goto L20;
		}
		lfuses[col + fusptr * 40] = FALSE_;
		lphant[col + fusptr * 40] = TRUE_;
L20:
		;
	    }
	    ++fusptr;
/* L30: */
	}
    }

    return 0;
} /* tweek_ */

#undef n
#undef p



/* *********************************************************************** */

/* Subroutine */ int binr_(logical *lfuses, integer *h__, integer *l)
{
    /* Format strings */
    static char fmt_10[] = "(//,\002                                        "
	    " .\002,//)";
    static char fmt_30[] = "(\002 \002,10(\002B\002,5a1,\002F \002))";

    /* Local variables */
    static integer i__, j, k, inc, isum3, iprod, itemp[50]	/* was [5][10]
	     */, iinput;

    /* Fortran I/O blocks */
    static cilist io___348 = { 0, 0, 0, fmt_10, 0 };
    static cilist io___357 = { 0, 0, 0, fmt_30, 0 };
    static cilist io___358 = { 0, 0, 0, fmt_10, 0 };


/*     THIS SUBROUTINE GENERATES BINARY PROGRAMMING FORMATS */
    /* Parameter adjustments */
    lfuses -= 41;

    /* Function Body */
    io___348.ciunit = lunit_1.pdf;
    s_wsfe(&io___348);
    e_wsfe();
    for (i__ = 1; i__ <= 41; i__ += 40) {
	inc = i__ - 1;
	for (iprod = 1; iprod <= 8; ++iprod) {
	    for (j = 1; j <= 31; j += 10) {
		for (k = 1; k <= 10; ++k) {
		    iinput = j + k - 1;
		    itemp[k * 5 - 5] = *l;
		    itemp[k * 5 - 4] = *l;
		    itemp[k * 5 - 3] = *l;
		    itemp[k * 5 - 2] = *l;
		    itemp[k * 5 - 1] = *l;
		    isum3 = iprod + inc;
		    if (lfuses[iinput + isum3 * 40]) {
			itemp[k * 5 - 1] = *h__;
		    }
		    if (lfuses[iinput + (isum3 + 8) * 40]) {
			itemp[k * 5 - 2] = *h__;
		    }
		    if (lfuses[iinput + (isum3 + 16) * 40]) {
			itemp[k * 5 - 3] = *h__;
		    }
		    if (lfuses[iinput + (isum3 + 24) * 40]) {
			itemp[k * 5 - 4] = *h__;
		    }
		    if (lfuses[iinput + (isum3 + 32) * 40]) {
			itemp[k * 5 - 5] = *h__;
		    }
/* L15: */
		}
/* L20: */
		io___357.ciunit = lunit_1.pdf;
		s_wsfe(&io___357);
		do_fio(&c__50, (char *)&itemp[0], (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	}
    }
    io___358.ciunit = lunit_1.pdf;
    s_wsfe(&io___358);
    e_wsfe();
    return 0;
} /* binr_ */


/* *********************************************************************** */

/* Subroutine */ int slip_(logical *lfuses, integer *i88pro, integer *itype, 
	integer *iblow)
{
    /* Initialized data */

    static integer ienabl[140]	/* was [10][14] */ = { 0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,
	    0,0,0,1,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,0,1,1,1,1,1,1,1,1,0,
	    0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,1,1,0,0,0,0,1,1,0 };

    static integer i__, iout;

/*     THIS SUBROUTINE WILL BLOW THE ENTIRE CONDITIONAL THREE-STATE */
/*      PRODUCT LINE WHEN 'IF(VCC)' CONDITION IS USED FOR THE */
/*      CORRESPONDING OUTPUT PIN */
/*     1=ENABLED OUTPUT.   0=ANYTHING ELSE FOR THAT OUTPUT */
    /* Parameter adjustments */
    lfuses -= 41;

    /* Function Body */

    iout = (*i88pro - 1) / 8 + 1;
    if (ienabl[iout + *itype * 10 - 11] == 0) {
	return 0;
    }
    for (i__ = 1; i__ <= 40; ++i__) {
	++(*iblow);
/* L10: */
	lfuses[i__ + *i88pro * 40] = TRUE_;
    }
    ++(*i88pro);
    return 0;
} /* slip_ */


/* *********************************************************************** */

/* Subroutine */ int fantom_(integer *itype, integer *iop, integer *iout, 
	integer *iprod, integer *i8pro)
{
    /* Initialized data */

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_372 = { "B   ", 0 };

#define b (*(integer *)&equiv_372)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_373 = { "N   ", 0 };

#define n (*(integer *)&equiv_373)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_374 = { "P   ", 0 };

#define p (*(integer *)&equiv_374)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_375 = { "0   ", 0 };

#define lofant (*(integer *)&equiv_375)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_376 = { "O   ", 0 };

#define hifant (*(integer *)&equiv_376)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_377 = { "    ", 0 };

#define iblank (*(integer *)&equiv_377)


    static integer col, grtype, lntype, output;

/*     THIS SUBROUTINE UPDATES IOUT (THE PRINTED FUSE PLOT) */
/*      FOR HIGH AND LOW PHANTOM FUSES */
    /* Parameter adjustments */
    --iout;

    /* Function Body */
/*     GET OUTPUT GROUPING */
    output = (*iprod - 1) / 8 + 1;
    grtype = blk_1.pr8x10[output + *itype * 10 - 11];
    lntype = blk_1.prod8[*i8pro + (grtype << 3) - 9];
    for (col = 1; col <= 40; ++col) {
	if (blk_1.prodln[col + lntype * 40 - 41] == p && *iop == p) {
	    iout[col] = hifant;
	}
	if (blk_1.prodln[col + lntype * 40 - 41] == p && *iop == b) {
	    iout[col] = iblank;
	}
	if (blk_1.prodln[col + lntype * 40 - 41] == n) {
	    iout[col] = lofant;
	}
/* L10: */
    }
    return 0;
} /* fantom_ */

#undef iblank
#undef hifant
#undef lofant
#undef p
#undef n
#undef b



/* *********************************************************************** */

/* Subroutine */ int iodc2_(void)
{
    /* Initialized data */

    static integer bel = 7;
    static integer dc2 = 22;

    /* Format strings */
    static char fmt_10[] = "(\002 \002,2a1)";

    /* Fortran I/O blocks */
    static cilist io___380 = { 0, 0, 0, fmt_10, 0 };


/* ***** THIS ROUTINE IS OPTIONAL, IT MAY BE USED TO TURN PERIPHERALS ON */
    io___380.ciunit = lunit_1.pdf;
    s_wsfe(&io___380);
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

    /* Local variables */
    static integer pdf;

    /* Fortran I/O blocks */
    static cilist io___385 = { 0, 0, 0, fmt_10, 0 };


/* ***** THIS ROUTINE IS OPTIONAL, IT MAY BE USED TO TURN PERIPHERALS OFF */

    io___385.ciunit = pdf;
    s_wsfe(&io___385);
    do_fio(&c__1, (char *)&bel, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&dc3, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&dc4, (ftnlen)sizeof(integer));
    e_wsfe();
    return 0;
} /* iodc4_ */


/* *********************************************************************** */

/* Subroutine */ int test_(logical *lphase, logical *lbuf, integer *title, 
	integer *ic, integer *il, integer *ile, integer *isym, integer *ibuf, 
	integer *itype, integer *ipctr, logical *lerr, integer *isaf, integer 
	*ipctr1, logical *lsa11, logical *lsa01, logical *lprint)
{
    /* Initialized data */

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_459 = { "-   ", 0 };

#define idash (*(integer *)&equiv_459)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_460 = { "L   ", 0 };

#define l (*(integer *)&equiv_460)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_461 = { "H   ", 0 };

#define h__ (*(integer *)&equiv_461)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_462 = { "X   ", 0 };

#define x (*(integer *)&equiv_462)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_463 = { "C   ", 0 };

#define c__ (*(integer *)&equiv_463)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_464 = { "Z   ", 0 };

#define z__ (*(integer *)&equiv_464)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_465 = { "0   ", 0 };

#define n0 (*(integer *)&equiv_465)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_466 = { "1   ", 0 };

#define n1 (*(integer *)&equiv_466)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_467 = { "?   ", 0 };

#define err (*(integer *)&equiv_467)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_468 = { "    ", 0 };

#define iblank (*(integer *)&equiv_468)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_469 = { ";   ", 0 };

#define coment (*(integer *)&equiv_469)

    static struct {
	char e_1[4];
	integer e_2;
	} equiv_470 = { "N   ", 0 };

#define nn (*(integer *)&equiv_470)

    static integer bel = 7;

    /* Format strings */
    static char fmt_2[] = "(/,\002 FUNCTION TABLE MUST BE SUPPLIED IN ORDER "
	    "TO PERFORM\002,\002 SIMULATION\002)";
    static char fmt_4[] = "(/,\002 \002,80a1,/)";
    static char fmt_6[] = "(/,\002 FUNCTION TABLE PIN LIST ERROR AT\002,8a1)";
    static char fmt_8[] = "(/,\002 \002,a1,\002 IS NOT AN ALLOWED FUNCTION T"
	    "ABLE ENTRY\002,\002 IN VECTOR \002,i3)";
    static char fmt_41[] = "(/,\002 FUNCTION TABLE ERROR IN VECTOR\002,i3"
	    ",\002  PIN =\002,8a1,\002  EXPECT = H  ACTUAL = L\002)";
    static char fmt_42[] = "(/,\002 FUNCTION TABLE ERROR IN VECTOR\002,i3"
	    ",\002  PIN =\002,8a1,\002  EXPECT = L  ACTUAL = H\002)";
    static char fmt_43[] = "(/,\002 FUNCTION TABLE ERROR IN VECTOR\002,i3"
	    ",\002  PIN =\002,8a1,/,\002  EXPECT  = OUTPUT ENABLE  ACTUAL = "
	    "Z\002)";
    static char fmt_44[] = "(/,\002 FUNCTION TABLE ERROR IN VECTOR\002,i3"
	    ",\002  PIN =\002,8a1,\002  EXPECT = Z  ACTUAL = \002,a1)";
    static char fmt_45[] = "(\002 \002,a1)";
    static char fmt_60[] = "(\002 \002,i3,\002 \002,24a1)";
    static char fmt_1000[] = "(\002 WARNING: MORE THAN 42 VECTORS HAVE BEEN "
	    "PROVIDED\002)";
    static char fmt_150[] = "(\002 \002,\002 PRODUCT: \002,i3,\002 OF \002"
	    ",\002EQUATION\002,i3,\002 UNTESTED(SA1) FAULT\002)";
    static char fmt_155[] = "(\002 \002,\002 PRODUCT: \002,i3,\002 OF \002"
	    ",\002EQUATION\002,i3,\002 UNTESTED(SA0) FAULT\002)";
    static char fmt_67[] = "(/,\002 PASS SIMULATION\002)";
    static char fmt_68[] = "(/,\002 NUMBER OF FUNCTION TABLE ERRORS =\002,i3)"
	    ;
    static char fmt_101[] = "(/,\002 ERROR SYMBOL =  \002,8a1,\002      IN L"
	    "INE NUMBER \002,i3,/,\002 \002,80a1,/,\002 THIS PIN NAME IS NOT "
	    "DEFINED IN THE\002,\002 FUNCTION TABLE PIN LIST\002)";

    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__, j, ic1, il1, ill, iifb;
    static logical lsa12, lsa02, nreg;
    static integer ieqn;
    extern /* Subroutine */ int incr_(integer *, integer *);
    static integer imax, ipin[24], iinp, nerr, isum, iout;
    static logical lout[24];
    static integer ieqn1, isym1[192]	/* was [8][24] */;
    extern /* Subroutine */ int match_(integer *, integer *, integer *);
    static logical lsame;
    static integer ivect[24], iprod, ilerr, nvect, imess, itest, ioutp;
    static logical loutp[24];
    static integer itrst;
    static logical lphas1[24];
    static integer ipctr2, ipctr3, ipctr4;
    static logical lenabl[24];
    static integer iclock, imatch;
    static logical lclock;
    static integer istate[24], ivectp[24];
    static logical xorfnd;
    static integer istatt[24];
    static logical lctrst;
    extern /* Subroutine */ int getsym_(logical *, integer *, integer *, 
	    integer *, integer *);
    static logical lptrst;
    static integer xorsum;

    /* Fortran I/O blocks */
    static cilist io___399 = { 0, 0, 0, fmt_2, 0 };
    static cilist io___400 = { 0, 0, 0, fmt_4, 0 };
    static cilist io___410 = { 0, 0, 0, fmt_6, 0 };
    static cilist io___424 = { 0, 0, 0, fmt_8, 0 };
    static cilist io___444 = { 0, 0, 0, fmt_41, 0 };
    static cilist io___445 = { 0, 0, 0, fmt_42, 0 };
    static cilist io___446 = { 0, 0, 0, fmt_43, 0 };
    static cilist io___447 = { 0, 0, 0, fmt_44, 0 };
    static cilist io___448 = { 0, 0, 0, fmt_45, 0 };
    static cilist io___449 = { 0, 0, 0, fmt_60, 0 };
    static cilist io___450 = { 0, 0, 0, fmt_1000, 0 };
    static cilist io___451 = { 0, 0, 0, fmt_150, 0 };
    static cilist io___454 = { 0, 0, 0, fmt_155, 0 };
    static cilist io___455 = { 0, 0, 0, fmt_67, 0 };
    static cilist io___456 = { 0, 0, 0, fmt_68, 0 };
    static cilist io___458 = { 0, 0, 0, fmt_101, 0 };


/*     THIS SUBROUTINE PERFORMS THE FUNCTION TABLE SIMULATION */
/*      AND GENERATES TEST VECTORS */
    /* Parameter adjustments */
    ibuf -= 9;
    isym -= 9;
    --title;
    --lbuf;
    --lphase;

    /* Function Body */
    tstvec_2.ntest = 0;
/*     PRINT AN ERROR MESSAGE IF NO FUNCTION TABLE IS SUPPLIED */
    if (ftest_1.ifunct != 0) {
	goto L3;
    }
    io___399.ciunit = lunit_1.pms;
    s_wsfe(&io___399);
    e_wsfe();
    return 0;
/*     PRINT TITLE */
L3:
    if (! (*lsa11) && ! (*lsa01) && *lprint) {
	io___400.ciunit = lunit_1.pof;
	s_wsfe(&io___400);
	do_fio(&c__80, (char *)&title[1], (ftnlen)sizeof(integer));
	e_wsfe();
    }
/*     INITIALIZE LERR (FUNCTION TABLE ERROR FLAG) TO NO ERROR */
    *lerr = FALSE_;
/*     INITIALIZE NERR (NUMBER OF FUNCTION TABLE ERRORS) TO NO ERROR */
    nerr = 0;
/*     SET THE STARTING POINT OF THE FUNCTION TABLE TO COLUMN 0 */
/*      AND IFUNCT + 1 */
    *ic = 0;
    *il = ftest_1.ifunct + 1;
/*     INITIALISE SA1/SA0 PARAMETERS */
    ipctr3 = 0;
    ieqn = 0;
    *ipctr = 0;
/*     INITIALIZE ITRST (THREE-STATE ENABLE FUNCTION TABLE PIN NUMBER) */
    itrst = 0;
/*     MAKE A DUMMY CALL TO INCR */
    incr_(ic, il);
/*     GET THE FUNCTION TABLE PIN LIST (UP TO 22) */
/*      GO ONE MORE THAN MAX TO LOOK FOR DASHED LINE */

    for (i__ = 1; i__ <= 23; ++i__) {
	getsym_(lphas1, isym1, &i__, ic, il);
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
	io___410.ciunit = lunit_1.pms;
	s_wsfe(&io___410);
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
	if (! (*itype == 8 || *itype == 9 || *itype == 10 || *itype == 12 || *
		itype == 13 || *itype == 14)) {
	    goto L10;
	}
	if (imatch == 1) {
	    iclock = i__;
	}
	if (imatch == 13) {
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

L90:
    ipctr2 = 0;
    ieqn = 0;
    ipctr3 = 0;
    lsa12 = FALSE_;
    lsa02 = FALSE_;
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

	io___424.ciunit = lunit_1.pms;
	s_wsfe(&io___424);
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
/*     INITIALIZE ISTATE ARRAY TO ALL L & H */
    for (i__ = 1; i__ <= 24; ++i__) {
	if (isym[(i__ << 3) + 1] != isym1[(i__ << 3) - 8]) {
	    goto L555;
	}
	istate[i__ - 1] = l;
	goto L15;
L555:
	istate[i__ - 1] = h__;
L15:
	;
    }
/*     CHECK IF THIS PAL TYPE HAS REGISTERS */
    if (! (*itype == 8 || *itype == 9 || *itype == 10 || *itype == 12 || *
	    itype == 13 || *itype == 14)) {
	goto L25;
    }
/*     CHECK CLOCK AND THREE-STATE ENABLE PINS AND CHANGE FLAG IF NEEDED */
    if (ivect[iclock - 1] == c__) {
	lclock = TRUE_;
    }
    if (itrst == 0) {
	goto L25;
    }
    lsame = lphase[13] && lphas1[itrst - 1] || ! lphase[13] && ! lphas1[itrst 
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
	if (j == 17 || j == 18 || j == 19 || j == 20) {
	    lenabl[i__ - 1] = FALSE_;
	}
	if ((*itype == 8 || *itype == 9 || *itype == 12 || *itype == 13) && (
		j == 16 || j == 21)) {
	    lenabl[i__ - 1] = FALSE_;
	}
	if ((*itype == 8 || *itype == 9 || *itype == 12) && (j == 15 || j == 
		22)) {
	    lenabl[i__ - 1] = FALSE_;
	}
	if (*itype == 8 && (j == 14 || j == 23)) {
	    lenabl[i__ - 1] = FALSE_;
	}
/* L46: */
    }

/* *****SCAN THROUGH THE LOGIC EQUATIONS***** */

/*     MAKE A DUMMY CALL TO INCR */
L25:
    incr_(&ic1, &il1);
L26:
    getsym_(&lbuf[1], &ibuf[9], &c__1, &ic1, &il1);
    if (_BLNK__1.lleft) {
	goto L29;
    }
L27:
    if (! _BLNK__1.lequal) {
	goto L26;
    }
    if (_BLNK__1.lequal) {
	++ieqn;
    }
/*     EVALUATE CONDITIONAL THREE-STATE PRODUCT LINE */
L29:
    if (_BLNK__1.lequal) {
	goto L35;
    }
    nreg = TRUE_;
L33:
    getsym_(&lbuf[1], &ibuf[9], &c__1, &ic1, &il1);
    match_(&iinp, &ibuf[9], isym1);
/*     CHECK FOR GND, VCC, /GND, OR /VCC IN CONDITIONAL THREE-STATE */
/*     PRODUCT LINE */
    if (iinp != 0) {
	goto L32;
    }
    match_(&imatch, &ibuf[9], &isym[9]);
    ill = il1;

    if (imatch == 12 && lbuf[1] || imatch == 24 && ! lbuf[1]) {
	lctrst = FALSE_;
    }
    if (iinp == 0 && imatch != 12 && imatch != 24) {
	goto L100;
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

/*     FIND PIN NUMBER OF THE OUTPUT VECTORS */
L35:
    ipctr3 = 0;
    match_(&ioutp, &ibuf[9], isym1);
/*     FLAG FOR UNREGISTERED OUTPUTS */
    match_(&iout, &ibuf[9], &isym[9]);
    if (*itype <= 7 || *itype == 11) {
	nreg = TRUE_;
    }
    if ((*itype == 9 || *itype == 10) && (iout == 14 || iout == 23)) {
	nreg = TRUE_;
    }
    if ((*itype == 10 || *itype == 13 || *itype == 14) && (iout == 15 || iout 
	    == 22)) {
	nreg = TRUE_;
    }
    if ((*itype == 10 || *itype == 14) && (iout == 16 || iout == 21)) {
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
L28:
    ++ipctr2;
    ++ipctr3;
    ++(*ipctr);
    iprod = h__;
L30:
    ill = il1;
    getsym_(&lbuf[1], &ibuf[9], &c__1, &ic1, &il1);
    match_(&iinp, &ibuf[9], isym1);
    if (iinp != 0) {
	goto L47;
    }
    match_(&imatch, &ibuf[9], &isym[9]);
    if (imatch != 12 && imatch != 24) {
	goto L100;
    }
/*     TWEEK FOR GND AND VCC IN PRODUCT LINE */
    if (imatch == 12) {
	itest = l;
    }
    if (imatch == 24) {
	itest = h__;
    }
    iinp = 23;
    lphas1[22] = TRUE_;
    goto L37;
L47:
    itest = ivect[iinp - 1];
/*     GET REGISTERED FEED BACK VALUES */

    if (nreg) {
	goto L37;
    }
    match_(&iifb, &ibuf[9], &isym[9]);
    if ((*itype == 8 || *itype == 9 || *itype == 10 || *itype == 12 || *itype 
	    == 13 || *itype == 14) && (iifb == 17 || iifb == 18 || iifb == 19 
	    || iifb == 20)) {
	itest = ivectp[iinp - 1];
    }
    if ((*itype == 8 || *itype == 9 || *itype == 12 || *itype == 13) && (iifb 
	    == 16 || iifb == 21)) {
	itest = ivectp[iinp - 1];
    }
    if ((*itype == 8 || *itype == 9 || *itype == 12) && (iifb == 15 || iifb ==
	     22)) {
	itest = ivectp[iinp - 1];
    }
    if (*itype == 8 && (iifb == 14 || iifb == 23)) {
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
    if (ipctr2 == *ipctr1 && *lsa11) {
	goto L110;
    }
L38:
    if (_BLNK__1.land) {
	goto L30;
    }
    if (ipctr2 == *ipctr1 && *lsa01) {
	goto L120;
    }
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
	if (! (*lerr) && (*lsa11 || *lsa01)) {
	    goto L50;
	}
	if (*lerr && (*lsa11 || *lsa01)) {
	    goto L115;
	}
	if (imess == 41) {
	    io___444.ciunit = lunit_1.pms;
	    s_wsfe(&io___444);
	    do_fio(&c__1, (char *)&nvect, (ftnlen)sizeof(integer));
	    for (j = 1; j <= 8; ++j) {
		do_fio(&c__1, (char *)&isym1[j + (i__ << 3) - 9], (ftnlen)
			sizeof(integer));
	    }
	    e_wsfe();
	}
	if (imess == 42) {
	    io___445.ciunit = lunit_1.pms;
	    s_wsfe(&io___445);
	    do_fio(&c__1, (char *)&nvect, (ftnlen)sizeof(integer));
	    for (j = 1; j <= 8; ++j) {
		do_fio(&c__1, (char *)&isym1[j + (i__ << 3) - 9], (ftnlen)
			sizeof(integer));
	    }
	    e_wsfe();
	}
	if (imess == 43) {
	    io___446.ciunit = lunit_1.pms;
	    s_wsfe(&io___446);
	    do_fio(&c__1, (char *)&nvect, (ftnlen)sizeof(integer));
	    for (j = 1; j <= 8; ++j) {
		do_fio(&c__1, (char *)&isym1[j + (i__ << 3) - 9], (ftnlen)
			sizeof(integer));
	    }
	    e_wsfe();
	}
	if (imess == 44) {
	    io___447.ciunit = lunit_1.pms;
	    s_wsfe(&io___447);
	    do_fio(&c__1, (char *)&nvect, (ftnlen)sizeof(integer));
	    for (j = 1; j <= 8; ++j) {
		do_fio(&c__1, (char *)&isym1[j + (i__ << 3) - 9], (ftnlen)
			sizeof(integer));
	    }
	    do_fio(&c__1, (char *)&ivect[i__ - 1], (ftnlen)sizeof(integer));
	    e_wsfe();
	}
	if (imess != 40 && lunit_1.pms == 6) {
	    io___448.ciunit = lunit_1.pms;
	    s_wsfe(&io___448);
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
    for (i__ = 1; i__ <= 24; ++i__) {
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
	    if (*itype == 6 && (i__ == 18 || i__ == 19)) {
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
    istate[11] = x;
    istate[23] = n1;
/*     PRINT TEST VECTORS */
    if (! (*lsa11) && ! (*lsa01) && *lprint) {
	io___449.ciunit = lunit_1.pof;
	s_wsfe(&io___449);
	do_fio(&c__1, (char *)&nvect, (ftnlen)sizeof(integer));
	for (i__ = 1; i__ <= 24; ++i__) {
	    do_fio(&c__1, (char *)&istate[i__ - 1], (ftnlen)sizeof(integer));
	}
	e_wsfe();
    }
    if (nvect == 43) {
	io___450.ciunit = lunit_1.pms;
	s_wsfe(&io___450);
	e_wsfe();
    }
    if (nvect > 42) {
	goto L90;
    }
    ++tstvec_2.ntest;
    for (i__ = 1; i__ <= 24; ++i__) {
	if (istate[i__ - 1] == l) {
	    tstvec_2.tstvec[i__ + tstvec_2.ntest * 24 - 25] = l;
	}
	if (istate[i__ - 1] == h__) {
	    tstvec_2.tstvec[i__ + tstvec_2.ntest * 24 - 25] = h__;
	}
	if (istate[i__ - 1] == z__) {
	    tstvec_2.tstvec[i__ + tstvec_2.ntest * 24 - 25] = z__;
	}
	if (istate[i__ - 1] == x) {
	    tstvec_2.tstvec[i__ + tstvec_2.ntest * 24 - 25] = x;
	}
	if (istate[i__ - 1] == c__) {
	    tstvec_2.tstvec[i__ + tstvec_2.ntest * 24 - 25] = c__;
	}
	if (istate[i__ - 1] == n0) {
	    tstvec_2.tstvec[i__ + tstvec_2.ntest * 24 - 25] = n0;
	}
	if (istate[i__ - 1] == n1) {
	    tstvec_2.tstvec[i__ + tstvec_2.ntest * 24 - 25] = n1;
	}
	tstvec_2.tstvec[tstvec_2.ntest * 24 - 13] = nn;
	tstvec_2.tstvec[tstvec_2.ntest * 24 - 1] = nn;
	if (tstvec_2.tstvec[tstvec_2.ntest * 24 - 12] == x) {
	    tstvec_2.tstvec[tstvec_2.ntest * 24 - 12] = n0;
	}
/* L1010: */
    }
    goto L90;
/*     TERMINATE SIMULATION */
L95:
    if (! (*lerr) && *lsa11 && *lprint) {
	io___451.ciunit = lunit_1.pof;
	s_wsfe(&io___451);
	do_fio(&c__1, (char *)&ipctr4, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ieqn1, (ftnlen)sizeof(integer));
	e_wsfe();
    }
    if (! (*lerr) && *lsa01 && *lprint) {
	io___454.ciunit = lunit_1.pof;
	s_wsfe(&io___454);
	do_fio(&c__1, (char *)&ipctr4, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ieqn1, (ftnlen)sizeof(integer));
	e_wsfe();
    }
    if (! (*lerr) && (! (*lsa11) && ! (*lsa01)) && *lprint) {
	io___455.ciunit = lunit_1.pof;
	s_wsfe(&io___455);
	e_wsfe();
    }
    *ipctr /= nvect - 1;
    if (*lerr && (! (*lsa11) && ! (*lsa01)) && *lprint) {
	io___456.ciunit = lunit_1.pof;
	s_wsfe(&io___456);
	do_fio(&c__1, (char *)&nerr, (ftnlen)sizeof(integer));
	e_wsfe();
    }
    return 0;
/*     PRINT AN ERROR MESSAGE FOR AN UNDEFINED PIN NAME */
L100:
    ilerr = ill + 4;
    io___458.ciunit = lunit_1.pms;
    s_wsfe(&io___458);
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
L110:
    iprod = h__;
    lsa12 = TRUE_;
    ieqn1 = ieqn;
    ipctr4 = ipctr3;
    goto L38;
L120:
    iprod = l;
    lsa02 = TRUE_;

    ieqn1 = ieqn;
    ipctr4 = ipctr3;
    goto L121;
L115:
    ++(*isaf);
    *lerr = FALSE_;
    return 0;
} /* test_ */

#undef nn
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

/* Subroutine */ int intel_(logical *lfuses, integer *iop)
{
    /* Initialized data */

    static struct {
	char e_1[64];
	integer e_2;
	} equiv_483 = { "0   1   2   3   4   5   6   7   8   9   A   B   C  "
		" D   E   F   ", 0 };

#define ztable ((integer *)&equiv_483)


    /* Format strings */
    static char fmt_60[] = "(\002 :28\002,4a1,\00200\002,80a1,2a1)";
    static char fmt_25[] = "(\002 :00000001FF\002)";

    /* Local variables */
    static integer i__, inc, addr__, ihex, csum, isum2, iprod, itemp[80]	
	    /* was [2][40] */, iinput;

    /* Fortran I/O blocks */
    static cilist io___481 = { 0, 0, 0, fmt_60, 0 };
    static cilist io___482 = { 0, 0, 0, fmt_25, 0 };


/*     THIS SUBROUTINE GENERATES THE INTEL HEX PROGRAMMING FORMAT */
    /* Parameter adjustments */
    lfuses -= 41;

    /* Function Body */
    addr__ = 0;
    for (i__ = 1; i__ <= 41; i__ += 40) {
	inc = i__ - 1;
	for (iprod = 1; iprod <= 8; ++iprod) {
	    csum = (addr__ / 256 + addr__ % 256 + 40) % 256;
	    for (iinput = 1; iinput <= 40; ++iinput) {
		ihex = 0;
		isum2 = iprod + inc;
		if (lfuses[iinput + isum2 * 40]) {
		    ++ihex;
		}
		if (lfuses[iinput + (isum2 + 8) * 40]) {
		    ihex += 2;
		}
		if (lfuses[iinput + (isum2 + 16) * 40]) {
		    ihex += 4;
		}
		if (lfuses[iinput + (isum2 + 24) * 40]) {
		    ihex += 8;
		}
		if (lfuses[iinput + (isum2 + 32) * 40]) {
		    ihex += 16;
		}
		csum = (csum + ihex) % 256;
		itemp[(iinput << 1) - 2] = ztable[ihex / 16];
		itemp[(iinput << 1) - 1] = ztable[ihex % 16];
/* L10: */
	    }
	    if (csum != 0) {
		csum = 256 - csum;
	    }
	    io___481.ciunit = lunit_1.pdf;
	    s_wsfe(&io___481);
	    do_fio(&c__1, (char *)&ztable[addr__ / 4096], (ftnlen)sizeof(
		    integer));
	    do_fio(&c__1, (char *)&ztable[addr__ / 256 % 16], (ftnlen)sizeof(
		    integer));
	    do_fio(&c__1, (char *)&ztable[addr__ / 16 % 16], (ftnlen)sizeof(
		    integer));
	    do_fio(&c__1, (char *)&ztable[addr__ % 16], (ftnlen)sizeof(
		    integer));
	    do_fio(&c__80, (char *)&itemp[0], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&ztable[csum / 16], (ftnlen)sizeof(integer))
		    ;
	    do_fio(&c__1, (char *)&ztable[csum % 16], (ftnlen)sizeof(integer))
		    ;
	    e_wsfe();
	    addr__ += 40;
/* L15: */
	}
    }
    io___482.ciunit = lunit_1.pdf;
    s_wsfe(&io___482);
    e_wsfe();
    return 0;
} /* intel_ */

#undef ztable



/*  ******************************************************************** */

/* Subroutine */ int sub_exit(void)
{
    s_stop("", (ftnlen)0);
    return 0;
} /* sub_exit */

