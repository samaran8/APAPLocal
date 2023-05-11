* @ValidationCode : MjotMTQzODM2NzMwOkNwMTI1MjoxNjgxMjc2MTQxODg5OklUU1M6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 12 Apr 2023 10:39:01
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
SUBROUTINE REDO.B.LY.GET.EXP.SELECT
*-----------------------------------------------------------------------------
* Select routine to setup the common area for the multi-threaded Close of Business
* job XX
*-------------------------------------------------------------------------------------
*Modification
* Date                   who                   Reference              
* 12-04-2023         CONVERSTION TOOL     R22 AUTO CONVERSTION - VM TO @VM AND SM TO @SM AND ++ TO += 1
* 12-04-2023          ANIL KUMAR B        R22 MANUAL CONVERSTION -NO CHANGES
*--------------------------------------------------------------------------------------
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_BATCH.FILES
*
    $INSERT I_REDO.B.LY.GET.EXP.COMMON
    $INSERT I_F.REDO.LY.POINTS
*-----------------------------------------------------------------------------
* Setup the parameters for BATCH.BUILD.LIST

* ID.LIST = predefined list, for example from a CONCAT file record.

*           ID.LIST will take precedence over LIST.PARAMETERS

* CONTROL.LIST = common list used by BATCH.JOB.CONTROL

    SEL.CMD = 'SELECT ':FN.REDO.LY.POINTS
    CUST.LST = ''
    CALL EB.READLIST(SEL.CMD,CUST.LST,'',Y.TOT.CUST,CUST.ERR)

    CNT.CUST = 1
    LOOP
    WHILE CNT.CUST LE Y.TOT.CUST
        Y.PTS.ID = CUST.LST<CNT.CUST>
        R.REC.POINTS = ''; PTS.ERR.R = ''
        CALL F.READ(FN.REDO.LY.POINTS,Y.PTS.ID,R.REC.POINTS,F.REDO.LY.POINTS,PTS.ERR.R)
        IF R.REC.POINTS THEN
            GOSUB GET.DETAILS
        END
        CNT.CUST += 1
    REPEAT

    CALL BATCH.BUILD.LIST('',ID.LIST)

RETURN

************
GET.DETAILS:
************

    Y.EXP.DATES = R.REC.POINTS<REDO.PT.EXP.DATE>
    Y.EXP.DATES.VM = DCOUNT(Y.EXP.DATES,@VM)

    Y.CNT.VM = 1
    LOOP
    WHILE Y.CNT.VM LE Y.EXP.DATES.VM

        Y.EXP.DATES.SM = R.REC.POINTS<REDO.PT.EXP.DATE,Y.CNT.VM>
        Y.TOT.SM.REC = DCOUNT(Y.EXP.DATES.SM,@SM)

        CNT.SM = 1
        LOOP
        WHILE CNT.SM LE Y.TOT.SM.REC
            Y.STATUS.SM = R.REC.POINTS<REDO.PT.STATUS,Y.CNT.VM,CNT.SM>
            IF Y.STATUS.SM EQ 'Liberada' OR Y.STATUS.SM EQ 'Generada' OR Y.STATUS.SM EQ 'Pendiente.Someter' THEN
                Y.EXP.DATE.SM = R.REC.POINTS<REDO.PT.EXP.DATE,Y.CNT.VM,CNT.SM>
                IF Y.EXP.DATE.SM GE TODAY AND Y.EXP.DATE.SM LT Y.NEXT.WDATE THEN
                    ID.LIST<-1> = Y.PTS.ID:',':Y.CNT.VM:',':CNT.SM
                END
            END
            CNT.SM += 1
        REPEAT
        Y.CNT.VM += 1
    REPEAT

RETURN

END
