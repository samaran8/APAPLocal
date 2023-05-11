* @ValidationCode : MjoyNTU5Njc2NTc6Q3AxMjUyOjE2ODIwNzExODgzODM6SVRTU0JORzotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 21 Apr 2023 15:29:48
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.LAPAP
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*21-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 BP REMOVED, VM TO @VM , FM TO @FM
*21-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*--------------------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE L.APAP.VAL.INAC
    $INSERT I_COMMON ;* AUTO R22 CODE CONVERSION START
    $INSERT I_EQUATE
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.ACCOUNT ;* AUTO R22 CODE CONVERSION END


    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)


    VAR.ID =  R.NEW(AZ.NOMINATED.ACCOUNT)

    CALL F.READ(FN.ACCOUNT,VAR.ID,R.ACCOUNT,F.ACCOUNT,YERR)

    CALL GET.LOC.REF("ACCOUNT","L.AC.STATUS1",ACC.POS)

    VAR.AZ.STATUS = R.ACCOUNT<AC.LOCAL.REF,ACC.POS>

    IF VAR.AZ.STATUS EQ "3YINACTIVE" THEN

        TEXT='REDO.AC.CHECK.ACTIVE':@FM:VAR.ID:@VM:' INACTIVA 3 ANOS'

        CURR.NO=1

        CALL STORE.OVERRIDE(CURR.NO)

    END

RETURN

IF VAR.AZ.STATUS EQ "ABANDONED" THEN
    TEXT='REDO.AC.CHECK.ACTIVE':@FM:VAR.ID:@VM:' ABANDONADA'

    CURR.NO=1

    CALL STORE.OVERRIDE(CURR.NO)

END

RETURN

IF VAR.AZ.STATUS EQ "6MINACTIVE" THEN

    TEXT='REDO.AC.CHECK.ACTIVE':@FM:VAR.ID:@VM:' INACTIVIDAD INTERNA'

    CURR.NO=1

    CALL STORE.OVERRIDE(CURR.NO)

END

RETURN

END
