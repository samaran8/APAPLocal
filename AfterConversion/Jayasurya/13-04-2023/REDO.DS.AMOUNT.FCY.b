* @ValidationCode : MjotMTc1ODMwMjgzMTpDcDEyNTI6MTY4MTM3NzY0NDA0NTpJVFNTQk5HOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 13 Apr 2023 14:50:44
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.REDORETAIL
SUBROUTINE REDO.DS.AMOUNT.FCY(AMT.FCY)
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :SUDHARSANAN S
*Program   Name    :REDO.DS.AMOUNT.FCY
*Modify            :btorresalbornoz
*---------------------------------------------------------------------------------
*DESCRIPTION       : This program is used to get the credit amount details
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*13-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 VM TO @VM, FM TO @FM
*13-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
* ----------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.TELLER

    GOSUB INIT
    GOSUB PROCESS
RETURN
*********
PROCESS:
*********
*
*    IF Y.CCY.1 NE LCCY THEN
*        VAR.AMOUNT = R.NEW(TT.TE.AMOUNT.FCY.1)
*    END
*
*    IF Y.CCY.2 NE LCCY THEN
*        VAR.AMOUNT = R.NEW(TT.TE.AMOUNT.FCY.2)
*    END
*
    IF Y.DR.CR.MARKER EQ "DEBIT" THEN
        IF Y.CCY.1 NE LCCY THEN
            VAR.AMOUNT = R.NEW(TT.TE.LOCAL.REF)<1,DEBIT.POS>
        END
        ELSE
            VAR.AMOUNT = R.NEW(TT.TE.LOCAL.REF)<1,CREDIT.POS>
        END
    END
*
    IF Y.DR.CR.MARKER EQ "CREDIT" THEN
        IF Y.CCY.1 NE LCCY THEN
            VAR.AMOUNT = R.NEW(TT.TE.LOCAL.REF)<1,CREDIT.POS>
        END
        ELSE
            VAR.AMOUNT = R.NEW(TT.TE.LOCAL.REF)<1,DEBIT.POS>
        END
    END
*
    AMT.FCY = FMT(VAR.AMOUNT,"R2,#21")

RETURN
*
*****
INIT:
*****
*
    Y.CCY.1        = ""
    Y.CCY.2        = ""
    VAR.AMOUNT     = ""
    Y.DR.CR.MARKER = ""
*
    IF APPLICATION EQ "TELLER" THEN
        Y.CCY.1  = R.NEW(TT.TE.CURRENCY.1)
        Y.CCY.2  = R.NEW(TT.TE.CURRENCY.2)
    END
*
    LOC.REF.APPLICATION = 'TELLER'
    LOC.REF.FIELDS      = 'L.DEBIT.AMOUNT'
    LOC.REF.FIELDS<2>   = 'L.CREDIT.AMOUNT'
    CHANGE @FM TO @VM IN LOC.REF.FIELDS
*
    LOC.REF.POS         = ''
*
    CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LREF.POS)
    DEBIT.POS        = LREF.POS<1,1>
    CREDIT.POS       = LREF.POS<1,2>
*
    Y.DR.CR.MARKER   = R.NEW(TT.TE.DR.CR.MARKER)
*
RETURN
*

END
