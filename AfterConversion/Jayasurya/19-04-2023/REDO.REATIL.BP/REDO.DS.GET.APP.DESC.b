* @ValidationCode : MjoxMDU3MTI1MDY3OkNwMTI1MjoxNjgxMzgyMTI3NzkxOklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 13 Apr 2023 16:05:27
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
SUBROUTINE REDO.DS.GET.APP.DESC(VAR.APP.DESC)
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :S SUDHARSANAN
*Program   Name    :REDO.DS.GET.APP.DESC
*---------------------------------------------------------------------------------
* DESCRIPTION       :This program is used to get the APP description value
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*13-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*13-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
* ----------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT.CLOSURE
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.AZ.PRODUCT.PARAMETER

    GOSUB OPENFILES
    GOSUB PROCESS

RETURN
***********
OPENFILES:
***********

    FN.APP = 'F.AZ.PRODUCT.PARAMETER'
    F.APP = ''
    CALL OPF(FN.APP,F.APP)

    LOC.REF.APP = 'ACCOUNT.CLOSURE'
    LOC.REF.FIELD = 'L.AC.AZ.ACC.REF'
    LOC.REF.POS = ''
    CALL MULTI.GET.LOC.REF(LOC.REF.APP,LOC.REF.FIELD,LOC.REF.POS)

    L.AC.AZ.ACC.REF.POS = LOC.REF.POS<1,1>

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

            APP.ID  = R.AZ.ACCOUNT<AZ.ALL.IN.ONE.PRODUCT>

        CASE APPLICATION EQ 'AZ.ACCOUNT'

            APP.ID = R.NEW(AZ.ALL.IN.ONE.PRODUCT)

    END CASE

    CALL F.READ(FN.APP,APP.ID,R.APP,F.APP,APP.ERR)

    VAR.APP.DESC = R.APP<AZ.APP.DESCRIPTION,LNGG>

    IF NOT(VAR.APP.DESC) THEN
        VAR.APP.DESC = R.APP<AZ.APP.DESCRIPTION,1>
    END

RETURN
END
*----------------------------------------------- End Of Record ----------------------------------
