* @ValidationCode : MjoxMDU4MzAxMzY6Q3AxMjUyOjE2ODI1MDA0Njg2MjE6MzMzc3U6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 26 Apr 2023 14:44:28
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
SUBROUTINE REDO.SUM.AZ.TOTAL.BAL
*******************************************************************************************
*Company   Name    : APAP
*Developed By      : Temenos Application Management
*Program   Name    : REDO.SUM.AZ.ACCREUED.INT
*------------------------------------------------------------------------------------------
*Description       : Used to get accrued interest
*Linked With       : ARCPDF
*In  Parameter     : ENQ.DATA
*Out Parameter     : ENQ.DATA
*ODR  Number:

*MODIFICATION:
*---------------------------------------------------------------------------------
*DATE           ODR                   DEVELOPER               VERSION
*--------       ----------------      -------------           --------------------
*07.04.2011     PACS00036498           Prabhu N            INITIAL CREATION
*10.04.2023     Conversion Tool        R22                 Auto Conversion     - FM TO @FM
*10.04.2023     Shanmugapriya M        R22                 Manual Conversion   - CALL routine format modified
*
*---------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.ACCOUNT
    $INSERT I_F.AZ.SCHEDULES


    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT  = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.AZ.ACCOUNT = 'F.AZ.ACCOUNT'
    F.AZ.ACCOUNT  = ''
    CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)

    FN.AZ.SCHEDULES = 'F.AZ.SCHEDULES'
    F.AZ.SCHEDULES  = ''
    CALL OPF(FN.AZ.SCHEDULES,F.AZ.SCHEDULES)

    LREF.APP = 'ACCOUNT':@FM:'AZ.ACCOUNT'
    LREF.FIELDS = 'L.AC.AV.BAL':@FM:'L.TYPE.INT.PAY'
    LREF.POS=''
    CALL APAP.TAM.MULTI.GET.LOC.REF(LREF.APP,LREF.FIELDS,LREF.POS) ;*MANUAL R22 CODE CONVERSION
    POS.L.AC.AV.BAL        = LREF.POS<1,1>
    POS.L.TYPE.INT.PAY.POS = LREF.POS<2,1>


    CALL F.READ(FN.AZ.ACCOUNT,O.DATA,R.AZ.ACCOUNT,F.AZ.ACCOUNT,AZ.ACCOUNT.ERR)
    Y.DEP.TYPE       = R.AZ.ACCOUNT<AZ.LOCAL.REF,POS.L.TYPE.INT.PAY.POS>
    Y.ORIG.PRINCIPAL = R.AZ.ACCOUNT<AZ.ORIG.PRINCIPAL>

    IF Y.DEP.TYPE EQ 'Reinvested' THEN
        Y.INTEREST.LIQU.ACCT  = R.AZ.ACCOUNT<AZ.INTEREST.LIQU.ACCT>
        CALL F.READ(FN.ACCOUNT,Y.INTEREST.LIQU.ACCT,R.ACCOUNT,F.ACCOUNT,ACCOUNT.ERR)
        O.DATA = Y.ORIG.PRINCIPAL + R.ACCOUNT<AC.LOCAL.REF,POS.L.AC.AV.BAL>
    END ELSE
        O.DATA = Y.ORIG.PRINCIPAL
    END

RETURN
END
