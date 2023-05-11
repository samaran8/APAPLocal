* @ValidationCode : Mjo5Mjg4OTEwNjg6Q3AxMjUyOjE2ODEyNzcwMjkwMjA6SVRTUzotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 12 Apr 2023 10:53:49
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.LY.PGEN.EXPROD.LOAD
*-------------------------------------------------------------------------------------------------
*DESCRIPTION:
* This routine initialises and retrieves data for the local common variable
* This routine is the load routine of the batch job REDO.B.LY.PGEN.EXPROD which updates
*   REDO.LY.POINTS table based on the data defined in the parameter table
*   REDO.LY.MODALITY & REDO.LY.PROGRAM
* ------------------------------------------------------------------------------------------------
* Input/Output:
*--------------
* IN  : -NA-
* OUT : -NA-
*
* Dependencies:
*---------------
* CALLS     : -NA-
* CALLED BY : -NA-
*
* Revision History:
*------------------
*   Date               who           Reference            Description
* 17-JUN-2013   RMONDRAGON        ODR-2011-06-0243      Initial Creation
* Date                   who                   Reference              
* 12-04-2023         CONVERSTION TOOL     R22 AUTO CONVERSTION - FM TO @FM AND VM TO @VM 
* 12-04-2023          ANIL KUMAR B        R22 MANUAL CONVERSTION -NO CHANGES
*--------------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.DATES

    $INSERT I_REDO.B.LY.PGEN.EXPROD.COMMON

    GOSUB INIT
    GOSUB OPEN.FILES
    GOSUB GET.VAL

RETURN

*----
INIT:
*----

    PRG.MOD.LST = ''         ; PRG.PER.MOD = '';
    PRG.LST = ''             ; PRG.ST.DATE.LST = '';
    PRG.END.DATE.LST = ''    ; PRG.DAYS.EXP.LST = '';
    PRG.EXP.DATE.LST = ''    ; PRG.CUS.GRP.LST = '';
    PRG.POINT.VALUE.LST = '' ; PRG.AVAIL.IF.DELAY.LST = '';
    PRG.POINT.USE.LST = ''   ; PRG.PTS.IN.MOD = '';
    PRG.EX.PROD.IN.MOD = ''  ; PRG.COND.TYPE.EXINC = '';
    PRG.APP.EXC.COND = ''    ; PRG.EXC.EST.ACCT = '';
    PRG.APP.INC.COND = ''    ; PRG.INC.EST.ACCT = '';
    PRG.AIR.LST = ''

    G.DATE = ''
    I.DATE = DATE()
    CALL DIETER.DATE(G.DATE,I.DATE,'')

    CUR.DAY   = TODAY[7,2]
    CUR.MONTH = TODAY[5,2]
    CUR.YEAR  = TODAY[1,4]

    LOC.REF.POS = ''
    LOC.REF.APP = 'CUSTOMER':@FM:'ACCOUNT'
    LOC.REF.FIELD = 'L.CU.G.LEALTAD':@FM:'L.AC.STATUS1':@VM:'L.AC.STATUS2'
    CALL MULTI.GET.LOC.REF(LOC.REF.APP,LOC.REF.FIELD,LOC.REF.POS)
    POS.L.CU.G.LEALTAD = LOC.REF.POS<1,1>
    POS.L.AC.STATUS1 = LOC.REF.POS<2,1>
    POS.L.AC.STATUS2 = LOC.REF.POS<2,2>

RETURN

*----------
OPEN.FILES:
*----------

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.REDO.LY.POINTS = 'F.REDO.LY.POINTS'
    F.REDO.LY.POINTS = ''
    CALL OPF(FN.REDO.LY.POINTS,F.REDO.LY.POINTS)

    FN.REDO.LY.POINTS.TOT = 'F.REDO.LY.POINTS.TOT'
    F.REDO.LY.POINTS.TOT = ''
    CALL OPF(FN.REDO.LY.POINTS.TOT,F.REDO.LY.POINTS.TOT)

    FN.REDO.LY.MASTERPRGDR = 'F.REDO.LY.MASTERPRGDR'
    F.REDO.LY.MASTERPRGDR = ''
    CALL OPF(FN.REDO.LY.MASTERPRGDR,F.REDO.LY.MASTERPRGDR)

    FN.CUSTOMER.ACCOUNT = 'F.CUSTOMER.ACCOUNT'
    F.CUSTOMER.ACCOUNT = ''
    CALL OPF(FN.CUSTOMER.ACCOUNT,F.CUSTOMER.ACCOUNT)

    FN.TEMP.LY.PGEN.EXPROD = 'F.TEMP.LY.PGEN.EXPROD'
    F.TEMP.LY.PGEN.EXPROD = ''
    OPEN FN.TEMP.LY.PGEN.EXPROD TO F.TEMP.LY.PGEN.EXPROD ELSE

        TEXT = 'Error in opening : ':FN.TEMP.LY.PGEN.EXPROD
        CALL FATAL.ERROR('REDO.B.LY.PGEN.EXPROD.LOAD')
    END

RETURN

*-------
GET.VAL:
*-------

    READ PRG.MOD.LST FROM F.TEMP.LY.PGEN.EXPROD,'MOD' ELSE
        GOSUB READ.NUSED
    END

    READ PRG.PER.MOD FROM F.TEMP.LY.PGEN.EXPROD,'NOPRG' ELSE
        GOSUB READ.NUSED
    END

    READ PRG.LST FROM F.TEMP.LY.PGEN.EXPROD,'PRG' ELSE
        GOSUB READ.NUSED
    END

    READ PRG.ST.DATE.LST FROM F.TEMP.LY.PGEN.EXPROD,'ST.DATE' ELSE
        GOSUB READ.NUSED
    END

    READ PRG.END.DATE.LST FROM F.TEMP.LY.PGEN.EXPROD,'END.DATE' ELSE
        GOSUB READ.NUSED
    END

    READ PRG.DAYS.EXP.LST FROM F.TEMP.LY.PGEN.EXPROD,'DAYS.EXP' ELSE
        GOSUB READ.NUSED
    END

    READ PRG.EXP.DATE.LST FROM F.TEMP.LY.PGEN.EXPROD,'EXP.DATE' ELSE
        GOSUB READ.NUSED
    END

    READ PRG.CUS.GRP.LST FROM F.TEMP.LY.PGEN.EXPROD,'CUS.GROUP' ELSE
        GOSUB READ.NUSED
    END

    READ PRG.POINT.VALUE.LST FROM F.TEMP.LY.PGEN.EXPROD,'POINT.VALUE' ELSE
        GOSUB READ.NUSED
    END

    READ PRG.AVAIL.IF.DELAY.LST FROM F.TEMP.LY.PGEN.EXPROD,'AVAIL.IF.DELAY' ELSE
        GOSUB READ.NUSED
    END

    READ PRG.POINT.USE.LST FROM F.TEMP.LY.PGEN.EXPROD,'POINT.USE' ELSE
        GOSUB READ.NUSED
    END

    READ PRG.COND.TYPE.EXINC FROM F.TEMP.LY.PGEN.EXPROD,'COND.TYPE.EXINC' ELSE
        GOSUB READ.NUSED
    END

    READ PRG.APP.EXC.COND FROM F.TEMP.LY.PGEN.EXPROD,'APP.EXC.COND' ELSE
        GOSUB READ.NUSED
    END

    READ PRG.EXC.EST.ACCT FROM F.TEMP.LY.PGEN.EXPROD,'EXC.EST.ACCT' ELSE
        GOSUB READ.NUSED
    END

    READ PRG.APP.INC.COND FROM F.TEMP.LY.PGEN.EXPROD,'APP.INC.COND' ELSE
        GOSUB READ.NUSED
    END

    READ PRG.INC.EST.ACCT FROM F.TEMP.LY.PGEN.EXPROD,'INC.EST.ACCT' ELSE
        GOSUB READ.NUSED
    END

    READ PRG.PTS.IN.MOD FROM F.TEMP.LY.PGEN.EXPROD,'PTS.IN.MOD' ELSE
        GOSUB READ.NUSED
    END

    READ PRG.EX.PROD.IN.MOD FROM F.TEMP.LY.PGEN.EXPROD,'EX.PROD.IN.MOD' ELSE
        GOSUB READ.NUSED
    END

    READ PRG.AIR.LST FROM F.TEMP.LY.PGEN.EXPROD,'AIR' ELSE
        GOSUB READ.NUSED
    END

RETURN

*----------
READ.NUSED:
*----------

    CRT 'Reading in the record non-used for this generation'

RETURN

END
