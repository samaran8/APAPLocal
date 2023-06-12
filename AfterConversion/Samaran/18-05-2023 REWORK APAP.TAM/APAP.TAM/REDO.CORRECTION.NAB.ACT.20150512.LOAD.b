* @ValidationCode : MjoxMTI2NzkxNDcxOkNwMTI1MjoxNjg0NDEwNDUzMzY1OnNhbWFyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 18 May 2023 17:17:33
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : samar
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM

** 21-04-2023 R22 Auto Conversion no changes
** 21-04-2023 Skanda R22 Manual Conversion - No changes

SUBROUTINE REDO.CORRECTION.NAB.ACT.20150512.LOAD

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_REDO.CORRECTION.NAB.ACT.20150512.COMMON



    FN.REDO.AA.NAB.HISTORY = 'F.REDO.AA.NAB.HISTORY'
    F.REDO.AA.NAB.HISTORY  = ''
    CALL OPF(FN.REDO.AA.NAB.HISTORY,F.REDO.AA.NAB.HISTORY)

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT  = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.REDO.CONCAT.ACC.NAB = 'F.REDO.CONCAT.ACC.NAB'
    F.REDO.CONCAT.ACC.NAB  = ''
    CALL OPF(FN.REDO.CONCAT.ACC.NAB,F.REDO.CONCAT.ACC.NAB)

    FN.AA.ARRANGEMENT = 'F.AA.ARRANGEMENT'
    F.AA.ARRANGEMENT  = ''
    CALL OPF(FN.AA.ARRANGEMENT,F.AA.ARRANGEMENT)

    FN.REDO.AA.INT.CLASSIFICATION = 'F.REDO.AA.INT.CLASSIFICATION'
    F.REDO.AA.INT.CLASSIFICATION  = ''
    CALL OPF(FN.REDO.AA.INT.CLASSIFICATION,F.REDO.AA.INT.CLASSIFICATION)

    FN.AA.BILL.DETAILS = 'F.AA.BILL.DETAILS'
    F.AA.BILL.DETAILS = ''
    CALL OPF(FN.AA.BILL.DETAILS, F.AA.BILL.DETAILS)

    FN.AA.ACCT.DET = 'F.AA.ACCOUNT.DETAILS'
    F.AA.ACCT.DET = ''
    CALL OPF(FN.AA.ACCT.DET,F.AA.ACCT.DET)

    FN.AA.ACCOUNT.DETAILS = FN.AA.ACCT.DET
    F.AA.ACCOUNT.DETAILS = F.AA.ACCT.DET

    FN.HELPTEXT = "F.HELPTEXT"
    F.HELPTEXT  = ""
    CALL OPF(FN.HELPTEXT,F.HELPTEXT)


    LOC.REF.APPLICATION   = "ACCOUNT"
    LOC.REF.FIELDS        = 'L.OD.STATUS'
    LOC.REF.POS           = ''
    CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LOC.REF.POS)
    POS.L.OD.STATUS       = LOC.REF.POS<1,1>

    Y.ID = 'SYSTEM'
*F.READ to CACHE.READ Converted by TUS-Convert
*  CALL F.READ(FN.REDO.AA.INT.CLASSIFICATION,Y.ID,R.REDO.AA.INT.CLASSIFICATION,F.REDO.AA.INT.CLASSIFICATION,CLS.ERR) ;*Tus Start
    CALL CACHE.READ(FN.REDO.AA.INT.CLASSIFICATION,Y.ID,R.REDO.AA.INT.CLASSIFICATION,CLS.ERR) ; * Tus End

RETURN
END