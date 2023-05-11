* @ValidationCode : MjoxMzk4Mjg5MjgwOkNwMTI1MjoxNjgyNTAyMDcwMDc0OklUU1MxOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 26 Apr 2023 15:11:10
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS1
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.LAPAP
SUBROUTINE L.APAP.AA.ACC.MOVE.RFN
*-------------------------------------------------------------------------------------
*Modification
* Date                  who                   Reference
* 21-04-2023         CONVERSTION TOOL      R22 AUTO CONVERSTION -$INSERT T24.BP TO $INSERT AND VM TO @VM AND FM TO @FM
* 21-04-2023          ANIL KUMAR B         R22 MANUAL CONVERSTION -NO CHANGES
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.ACCT.BALANCE.ACTIVITY

    Y.ID = ID.NEW.LAST
    FN.LIMPIAR.EB = 'F.ST.L.APAP.LIMPIAR.EB$NAU'
    FV.LIMPIAR.EB = ''
    CALL OPF(FN.LIMPIAR.EB,FV.LIMPIAR.EB)
    CALL F.READ(FN.LIMPIAR.EB,Y.ID,R.LIMPIAR,FV.LIMPIAR.EB,AA.LIMPIAR.ERR)

    AA.ID = R.LIMPIAR<1>
    Y.CUENTA.INTERNA = R.LIMPIAR<2>

    FN.AA.ACC.MOV ='F.AA.ACCOUNT.MOVEMENT'
    F.AA.ACC.MOV = ''
    CALL OPF(FN.AA.ACC.MOV, F.AA.ACC.MOV)

    FN.AA.ARR.STA ='F.AA.ARRANGEMENT.STATUS'
    F.AA.ARR.STA = ''
    CALL OPF(FN.AA.ARR.STA, F.AA.ARR.STA)

    FN.AA.ARR='F.AA.ARRANGEMENT'
    F.AA.ARR=''
    CALL OPF(FN.AA.ARR,F.AA.ARR)

    CALL F.READ(FN.AA.ARR,AA.ID,R.AA,F.AA.ARR,AA.ARR.ERR)
    Y.LINKED.APPL    = R.AA<AA.ARR.LINKED.APPL>
    Y.LINKED.APPL.ID = R.AA<AA.ARR.LINKED.APPL.ID>
    LOCATE "ACCOUNT" IN Y.LINKED.APPL<1,1> SETTING Y.LINKED.POS THEN
        CHANGE @VM TO @FM IN Y.LINKED.APPL.ID
        Y.IDCUENTA  = Y.LINKED.APPL.ID<Y.LINKED.POS>
    END
    CALL F.DELETE(FN.AA.ACC.MOV, Y.IDCUENTA)
    CALL F.DELETE(FN.AA.ARR.STA, AA.ID)

***---------------
***Executar la rutina de limpie de balance en EB.CONTRACT.BALANCE
***---------------
    CALL APAP.LAPAP.LAPAP.CLEAR.ECB.BALANCES.R15(AA.ID,Y.CUENTA.INTERNA)
    CALL APAP.LAPAP.LAPAP.FORM.POST.RESTR.ACTIVITY(AA.ID)
RETURN

END
