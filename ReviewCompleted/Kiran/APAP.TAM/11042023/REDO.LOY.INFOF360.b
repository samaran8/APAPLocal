* @ValidationCode : MjotMjEzNjYzODMwOTpDcDEyNTI6MTY4MTIwNzYwMzg2MjozMzNzdTotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 11 Apr 2023 15:36:43
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : 333su
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.LOY.INFOF360(R.DATA)
*-----------------------------------------------------------------------------
*DESCRIPTION:
*------------
* This is an Nofile routine to get some data for enquiry REDO.LOY.INFOF360
* related to B.25 Loyalty Module.
*
* Input/Output:
*--------------
* IN : CUSTOMER.ID
* OUT : R.DATA (ALL DATA)
*---------------
*-----------------------------------------------------------------------------
* Modification History :
*   Date            Who                   Reference               Description
* 15-JUN-2011    RMONDRAGON              ODR-2011-06-0243        FIRST VERSION
* 06-OCT-2011    RMONDRAGON              ODR-2011-06-0243       SECOND VERSION
* 11-NOV-2011      MMACIAS               ODR-2011-06-0243        THIRD VERSION
* 26-JUN-2012    RMONDRAGON              ODR-2011-06-0243       FOURTH VERSION
* 10-DEC-2013    RMONDRAGON              ODR-2011-06-0243       FIFTH VERSION
*DATE                 WHO                  REFERENCE                     DESCRIPTION
*11/04/2023      CONVERSION TOOL     AUTO R22 CODE CONVERSION       SM TO @SM, VM TO @VM, ++ TO +=
*11/04/2023         SURESH           MANUAL R22 CODE CONVERSION           NOCHANGE
*-----------------------------------------------------------------------------
* <region name= Inserts>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON

    $INSERT I_F.REDO.LY.POINTS.TOT
    $INSERT I_F.REDO.LY.PROGRAM
    $INSERT I_F.REDO.LY.POINTS
* </region>
*-----------------------------------------------------------------------------

    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS

RETURN

*****
INIT:
*****

    Y.SEL.PROG = ''
    Y.SEL.PROG.LIST = ''
    Y.SEL.PROG.LIST.NO = ''
    PROG.ERR = ''
    Y.SEL.LOY = ''
    Y.SEL.LOY.LIST = ''
    Y.SEL.LOY.LIST.NO = ''
    LOY.ERR = ''
    Y.CNT.PROG = 1
    CNT.REC = 1
    Y.REC.ID = ''
    Y.LOY.REC = ''
    Y.REC.LYPROGRAM = ''
    Y.LY.PROGRAM.REC = ''
    Y.LY.PROGRAM.NAME = ''
    R.DATA = ''
    Y.AVAIL.POINTS.TOT = 0
    Y.TODUE.POINTS.TOT = 0
    Y.GEN.POINTS.TOT = 0        ;*MMC

RETURN

*********
OPENFILES:
*********

    FN.REDO.LY.POINTS.TOT = 'F.REDO.LY.POINTS.TOT'
    F.REDO.LY.POINTS.TOT = ''
    CALL OPF(FN.REDO.LY.POINTS.TOT,F.REDO.LY.POINTS.TOT)

    FN.REDO.LY.POINTS = 'F.REDO.LY.POINTS'
    F.REDO.LY.POINTS = ''
    CALL OPF(FN.REDO.LY.POINTS,F.REDO.LY.POINTS)

    FN.REDO.LY.PROGRAM = 'F.REDO.LY.PROGRAM'
    F.REDO.LY.PROGRAM = ''
    CALL OPF(FN.REDO.LY.PROGRAM,F.REDO.LY.PROGRAM)

RETURN

********
PROCESS:
********

    LOCATE 'CUSTOMER.ID' IN D.FIELDS<1> SETTING CUSTOMER.ID.POS THEN
        Y.CUSTOMER.ID = D.RANGE.AND.VALUE<CUSTOMER.ID.POS>
    END

    Y.SEL.PROG = 'SELECT ':FN.REDO.LY.PROGRAM
    CALL EB.READLIST(Y.SEL.PROG,Y.SEL.PROG.LIST,'',Y.SEL.PROG.LIST.NO,PROG.ERR)

    IF Y.SEL.PROG.LIST.NO EQ 0 THEN
        RETURN
    END

    Y.CNT.PROG = 1
    LOOP
    WHILE Y.CNT.PROG LE Y.SEL.PROG.LIST.NO DO
        Y.PROG.ID = Y.SEL.PROG.LIST<Y.CNT.PROG>
        Y.SEL.LOY = 'SELECT ':FN.REDO.LY.POINTS.TOT:' WITH @ID LIKE ':Y.CUSTOMER.ID:Y.PROG.ID:'...ALL...'
        CALL EB.READLIST(Y.SEL.LOY,Y.SEL.LOY.LIST,'',Y.SEL.LOY.LIST.NO,LOY.ERR)
        IF Y.SEL.LOY.LIST.NO NE 0 THEN
            GOSUB GET.LY.PROGRAM
            GOSUB GET.PT.ALLT
        END
        Y.CNT.PROG += 1 ;*AUTO R22 CODE CONVERSION
    REPEAT

    R.DATA<-1> := 'TOTALES*':Y.AVAIL.POINTS.TOT:'*':Y.TODUE.POINTS.TOT:'*':Y.GEN.POINTS.TOT ;*MMC

RETURN

************
GET.PT.ALLT:
************

    Y.AVAIL.POINTS = 0
    Y.TODUE.POINTS = 0
    Y.GEN.POINTS = 0

    CNT.REC = 1
    LOOP
    WHILE CNT.REC LE Y.SEL.LOY.LIST.NO DO
        Y.REC.ID = Y.SEL.LOY.LIST<CNT.REC>
        CALL F.READ(FN.REDO.LY.POINTS.TOT,Y.REC.ID,Y.LOY.REC,F.REDO.LY.POINTS.TOT,LOY.ERR)
        Y.AVAIL.POINTS += Y.LOY.REC<REDO.PT.T.TOT.AVAIL.POINTS>
        Y.GEN.POINTS += Y.LOY.REC<REDO.PT.T.TOT.GEN.POINTS>     ;* MMC
        GOSUB GET.PT.TO.DUE
        Y.TODUE.POINTS += Y.CONT.TOT.PT
        CNT.REC += 1
    REPEAT

    GOSUB SUM.TOT.POINTS

    R.DATA<-1> := Y.LY.PROGRAM.NAME:'*':Y.AVAIL.POINTS:'*':Y.TODUE.POINTS:'*':Y.GEN.POINTS:'*F3*':Y.PROG.ID     ;*MMC

RETURN

***************
GET.LY.PROGRAM:
***************

    CALL F.READ(FN.REDO.LY.PROGRAM,Y.PROG.ID,Y.LY.PROGRAM.REC,F.REDO.LY.PROGRAM,LOY.ERR)
    IF Y.LY.PROGRAM.REC THEN
        Y.LY.PROGRAM.NAME = Y.LY.PROGRAM.REC<REDO.PROG.NAME>
    END

RETURN

**************
GET.PT.TO.DUE:
**************

    GOSUB GET.EXP.DATE.LIMIT
    Y.CONT.TOT.PT = 0
    Y.REDO.LY.PT = ''; LY.PT.ERR =''
    CALL F.READ(FN.REDO.LY.POINTS,Y.CUSTOMER.ID,Y.REDO.LY.PT,F.REDO.LY.POINTS,LY.PT.ERR)
    Y.PRODUCTS = Y.REDO.LY.PT<REDO.PT.PRODUCT>
    Y.PROGRAMS = Y.REDO.LY.PT<REDO.PT.PROGRAM>
    Y.TOT.PROD = DCOUNT(Y.PRODUCTS,@VM)
    Y.PROD.CNT = 1
    LOOP
    WHILE Y.PROD.CNT LE Y.TOT.PROD
        Y.PRGS = FIELD(Y.PROGRAMS,@VM,Y.PROD.CNT)
        Y.TOT.PRGS = DCOUNT(Y.PRGS,@SM)
        Y.PRG.CNT = 1
        LOOP
        WHILE Y.PRG.CNT LE Y.TOT.PRGS DO
            IF Y.REDO.LY.PT<REDO.PT.PROGRAM,Y.PROD.CNT,Y.PRG.CNT> EQ Y.PROG.ID THEN
                GOSUB GET.POINTS
            END
            Y.PRG.CNT += 1 ;*AUTO R22 CODE CONVERSION
        REPEAT
        Y.PROD.CNT += 1 ;*AUTO R22 CODE CONVERSION
    REPEAT

RETURN

*******************
GET.EXP.DATE.LIMIT:
*******************

    DAYS.EXP = 30
    EXP.DATE.LIMIT = TODAY
    TEMP.DAYS.EXP = '+':DAYS.EXP:'C'
    CALL CDT('',EXP.DATE.LIMIT,TEMP.DAYS.EXP)

RETURN

***********
GET.POINTS:
***********

    Y.EXP.DATE = ''
    Y.EXP.DATE = Y.REDO.LY.PT<REDO.PT.EXP.DATE,Y.PROD.CNT,Y.PRG.CNT>
    IF Y.EXP.DATE GE TODAY AND Y.EXP.DATE LE EXP.DATE.LIMIT THEN
        Y.CONT.TOT.PT += Y.REDO.LY.PT<REDO.PT.QUANTITY,Y.PROD.CNT,Y.PRG.CNT>
    END

RETURN

***************
SUM.TOT.POINTS:
***************

    Y.AVAIL.POINTS.TOT += Y.AVAIL.POINTS
    Y.TODUE.POINTS.TOT += Y.TODUE.POINTS
    Y.GEN.POINTS.TOT += Y.GEN.POINTS      ;* MMC
RETURN

END
