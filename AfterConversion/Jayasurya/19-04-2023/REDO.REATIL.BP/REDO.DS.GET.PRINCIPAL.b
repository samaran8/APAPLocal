* @ValidationCode : MjotMjE0MjcwNjM4NjpDcDEyNTI6MTY4MTM4MjM4MzAyNjpJVFNTQk5HOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 13 Apr 2023 16:09:43
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
SUBROUTINE REDO.DS.GET.PRINCIPAL(VAR.AMOUNT)
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :S SUDHARSANAN
*Program   Name    :REDO.DS.GET.PRINCIPAL
*---------------------------------------------------------------------------------
* DESCRIPTION       :This program is used to get the AMOUNT VALUE
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*13-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 FM TO @FM , VM TO @VM
*13-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES

* ----------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT.CLOSURE
    $INSERT I_F.AZ.ACCOUNT

    GOSUB OPENFILES
    GOSUB PROCESS

RETURN
***********
OPENFILES:
***********

    LOC.REF.APP = 'ACCOUNT.CLOSURE':@FM:'AZ.ACCOUNT'
    LOC.REF.FIELD = 'L.AC.AZ.ACC.REF':@FM:'ORIG.DEP.AMT'
    LOC.REF.POS = ''
    CALL MULTI.GET.LOC.REF(LOC.REF.APP,LOC.REF.FIELD,LOC.REF.POS)

    L.AC.AZ.ACC.REF.POS = LOC.REF.POS<1,1>
    L.ORIG.DEP.AMT.POS  = LOC.REF.POS<2,1>
RETURN
*********
PROCESS:
**********
    BEGIN CASE

        CASE APPLICATION EQ 'ACCOUNT.CLOSURE'

            FN.AZ.ACCOUNT = 'F.AZ.ACCOUNT$HIS'
            F.AZ.ACCOUNT = ''
            CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)

            VAR.ID = R.NEW(AC.ACL.LOCAL.REF)<1,L.AC.AZ.ACC.REF.POS>

            CALL EB.READ.HISTORY.REC(F.AZ.ACCOUNT,VAR.ID,R.AZ.ACCOUNT,AZ.ERR)

            Y.AMOUNT = R.AZ.ACCOUNT<AZ.LOCAL.REF,L.ORIG.DEP.AMT.POS>
            Y.CURR = R.AZ.ACCOUNT<AZ.CURRENCY>

        CASE APPLICATION EQ 'AZ.ACCOUNT'

            Y.AMOUNT = R.NEW(AZ.LOCAL.REF)<1,L.ORIG.DEP.AMT.POS>
            Y.CURR = R.NEW(AZ.CURRENCY)
    END CASE

    Y.AMOUNT = TRIM(FMT(Y.AMOUNT,"L2,#19")," ",'B')

    VAR.AMOUNT = Y.CURR:" ":Y.AMOUNT

RETURN
END
*----------------------------------------------- End Of Record ----------------------------------
