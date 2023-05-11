* @ValidationCode : MjoyMDg4OTI4MTk2OkNwMTI1MjoxNjgxMzgyMTYxNDE1OklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 13 Apr 2023 16:06:01
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
SUBROUTINE REDO.DS.GET.BENEF.NAME(BENEF.NAME)
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :S SUDHARSANAN
*Program   Name    :REDO.DS.GET.BENEF.NAME
*---------------------------------------------------------------------------------
* DESCRIPTION       :This program is used to get the account title value
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*13-04-2023            CONVERSION TOOL                AUTO R22 CODE CONVERSION           VM TO @VM ,FM TO @FM SM TO @SM
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

    LOC.REF.APP = 'AZ.ACCOUNT':@FM:'ACCOUNT.CLOSURE'
    LOC.REF.FIELD = 'BENEFIC.NAME':@FM:'L.AC.AZ.ACC.REF'
    LOC.REF.POS = ''
    CALL MULTI.GET.LOC.REF(LOC.REF.APP,LOC.REF.FIELD,LOC.REF.POS)

    L.BENEF.NAME.POS = LOC.REF.POS<1,1>
    L.AC.AZ.ACC.REF.POS = LOC.REF.POS<2,1>

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

            BENEF.NAME = R.AZ.ACCOUNT<AZ.LOCAL.REF,L.BENEF.NAME.POS>

            CHANGE @SM TO '' IN BENEF.NAME

        CASE APPLICATION EQ 'AZ.ACCOUNT'

            BENEF.NAME = R.NEW(AZ.LOCAL.REF)<1,L.BENEF.NAME.POS>

            CHANGE @SM TO '' IN BENEF.NAME

    END CASE

RETURN
END
*----------------------------------------------- End Of Record ----------------------------------
