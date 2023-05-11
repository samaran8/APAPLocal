* @ValidationCode : MjoxNzc4NzkyMDI3OkNwMTI1MjoxNjgxMTUxNjE4NzUxOklUU1M6LTE6LTE6MDoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 11 Apr 2023 00:03:38
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.VAN.MTS.AUTNEW2
*
* =============================================================================
*
*    First Release : Victor Nava
*    Developed for : APAP
*    Developed by  : Victor Nava
*    Date          : 2011/Oct/24
*
* ======================================================================
*
*  PACS00146869 - Uses previous cheque number value from the original
*  CHECK RECEIVED version (TELLER$NAU) .
* ======================================================================
*-----------------------------------------------------------------------------------------------------
* Modification History:
*
* Date             Who                   Reference      Description
* 11.04.2023       Conversion Tool       R22            Auto Conversion     - FM TO @FM, VM TO @VM, = TO EQ, New condition added
* 11.04.2023       Shanmugapriya M       R22            Manual Conversion   - No changes
*
*------------------------------------------------------------------------------------------------------

*
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_System
*
    $INSERT I_F.VERSION
    $INSERT I_F.TELLER
    $INSERT I_F.TELLER.ID
*
    $INSERT I_F.REDO.TRANSACTION.CHAIN

    GOSUB INITIALISE
    GOSUB OPEN.FILES
    GOSUB CHECK.PRELIM.CONDITIONS
    IF PROCESS.GOAHEAD THEN
        GOSUB PROCESS
        GOSUB RESET.USER.VARIABLES
    END
*
RETURN
*
*--------
PROCESS:
*--------
*
    WTM.FIRST.ID = System.getVariable("CURRENT.WTM.FIRST.ID")
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN           ;** R22 Auto Conversion - Start
        WTM.FIRST.ID = ""
    END                                            ;** R22 Auto Conversion - End

    IF WTM.FIRST.ID NE "" THEN
        R.REDO.TRANSACTION.CHAIN = "" ; ERR.MSJ = ''
        CALL F.READ(FN.REDO.TRANSACTION.CHAIN,WTM.FIRST.ID,R.REDO.TRANSACTION.CHAIN,F.REDO.TRANSACTION.CHAIN,ERR.MSJ)
        IF R.REDO.TRANSACTION.CHAIN<RTC.TRANS.AUTH> EQ "P" THEN              ;** R22 Auto conversion - = TO EQ
            WTID.NUMBER = DCOUNT(R.REDO.TRANSACTION.CHAIN<RTC.TRANS.ID>,@VM)
            WTXN.VERS = R.REDO.TRANSACTION.CHAIN<RTC.TRANS.VERS,WTID.NUMBER>
            GOSUB GET.NO.CHECK
        END
    END
*
RETURN
*
* -----------
GET.NO.CHECK:
* -----------
*
    R.TELLER.NAU = "" ; TT.ERR = ''
    CALL F.READ(FN.TELLER,WTM.FIRST.ID,R.TELLER.NAU,F.TELLER,TT.ERR)
    IF R.TELLER.NAU NE "" THEN
        R.NEW(TT.TE.LOCAL.REF)<1,WPOSNC> = R.TELLER.NAU<TT.TE.CHEQUE.NUMBER>
    END
*
RETURN
*
* ===================
RESET.USER.VARIABLES:
* ===================
*
    CALL System.setVariable("CURRENT.WTM.FIRST.ID","")
*
RETURN
*
* ---------
INITIALISE:
* ---------
    PROCESS.GOAHEAD    = 1
*
    FN.REDO.TRANSACTION.CHAIN = "F.REDO.TRANSACTION.CHAIN"
    F.REDO.TRANSACTION.CHAIN  = ""
*
    FN.TELLER = "F.TELLER$NAU"
    F.TELLER = ""
*
    WTID.NUMBER = ""
    WTXN.VERS = ""

    WAPP.LST = "TELLER"
    WCAMPO    = "L.INITIAL.ID"
    WCAMPO<2> = "L.NEXT.VERSION"
    WCAMPO<3> = "L.TT.NO.OF.CHQ"
    WCAMPO    = CHANGE(WCAMPO,@FM,@VM)
    CALL MULTI.GET.LOC.REF(WAPP.LST,WCAMPO,YPOS)
    WPOSLI    = YPOS<1,1>
    WPOSNV    = YPOS<1,2>
    WPOSNC    = YPOS<1,3>

    U.VARNAMES = ""
    U.VARVALS = ""
    VAR.NUM = ""
*
RETURN
*
*---------------
OPEN.FILES:
*---------------
*
    FN.REDO.TRANSACTION.CHAIN = 'F.REDO.TRANSACTION.CHAIN'
    F.REDO.TRANSACTION.CHAIN = ''
    CALL OPF(FN.REDO.TRANSACTION.CHAIN,F.REDO.TRANSACTION.CHAIN)

    FN.TELLER='F.TELLER'
    F.TELLER = ''
    CALL OPF(FN.TELLER,F.TELLER)
RETURN
*
* ======================
CHECK.PRELIM.CONDITIONS:
* ======================
*
    LOOP.CNT  = 1;    MAX.LOOPS = 1
*
* CAMBIOS DE CONDICION
    LOOP
    WHILE LOOP.CNT LE MAX.LOOPS AND PROCESS.GOAHEAD DO
        BEGIN CASE

            CASE LOOP.CNT EQ 1
                CALL System.getUserVariables( U.VARNAMES, U.VARVALS )
                VAR.NUM = DCOUNT( U.VARNAMES, @FM )
                IF VAR.NUM LT 1 THEN
                    PROCESS.GOAHEAD    = 0
                END
* -----
        END CASE

        LOOP.CNT +=1
    REPEAT

*
RETURN
*
END
