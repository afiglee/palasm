! 
! 
!                             Online Fortran Compiler.
!                 Code, Compile, Run and Debug Fortran program online.
! Write your code in this editor and press "Run" button to execute it.
! 
! 


Program Hello
      LOGICAL LFUSES(32,64),LPHANT(32,64)
      INTEGER ITYPE, I, K
      CHARACTER IOT
      
      IOT='C'
      DO 220 ITYPE=1,6
      DO 110 I=1,32
      DO 120 K=1,64
      LFUSES(I,K)=.FALSE.
      LPHANT(I,K)=.FALSE.
120   CONTINUE      
110   CONTINUE      
      
      
      call tweek(ITYPE, IOT, LFUSES, LPHANT)
      
      WRITE(*, 95) ITYPE, IOT
  95  FORMAT('CHIP=', I1, ' SUB=', 1A, 1X)
      WRITE(*, 97)
  97  FORMAT('LPHUSES:', 1X)
      DO 19 I=1,32
      WRITE(*, 98) LFUSES(I,:)
  98  FORMAT(' ', 64L1, 1X)  
  19  END DO 
  
      WRITE(*, 96)
  96  FORMAT('LPHAND:', 1X)
      DO 17 I=1,32
      WRITE(*, 94) LPHANT(I,:)
  94  FORMAT(' ', 64L1, 1X)  
  17  END DO 
  220 END DO      
      END
      SUBROUTINE TWEEK(ITYPE,IOT,LFUSES,LPHANT)
!     THIS SUBROUTINE TWEEKS LFUSES (THE PROGRAMMING FUSE PLOT)
!      FOR HIGH AND LOW PHANTOM FUSES
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
!
          DO 12 IPROD=1,57,8
              LFUSES(IINPUT,IPROD+4)=.FALSE.
              LFUSES(IINPUT,IPROD+5)=.FALSE.
              LFUSES(IINPUT,IPROD+6)=.FALSE.
              LFUSES(IINPUT,IPROD+7)=.FALSE.
              LPHANT(IINPUT,IPROD+4)=.TRUE.
              LPHANT(IINPUT,IPROD+5)=.TRUE.
              LPHANT(IINPUT,IPROD+6)=.TRUE.
              LPHANT(IINPUT,IPROD+7)=.TRUE.
    12        END DO          
          IF(ITYPE.GE.3) GO TO 18
          DO 14 IPROD=17,41,8
              LFUSES(IINPUT,IPROD+2)=.FALSE.
              LFUSES(IINPUT,IPROD+3)=.FALSE.
              LPHANT(IINPUT,IPROD+2)=.TRUE.
              LPHANT(IINPUT,IPROD+3)=.TRUE.
    14        END DO          
          IF(ITYPE.GE.2) GO TO 18
          DO 16 IPROD=1,57,8
              LFUSES(IINPUT,IPROD+2)=.FALSE.
              LFUSES(IINPUT,IPROD+3)=.FALSE.
              LPHANT(IINPUT,IPROD+2)=.TRUE.
              LPHANT(IINPUT,IPROD+3)=.TRUE.
    16        END DO          
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

!End Program Hello
