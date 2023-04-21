* @ValidationCode : Mjo0MzEwNDQxMDQ6Q3AxMjUyOjE2ODIwNzA3MTAwMzA6SVRTU0JORzotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 21 Apr 2023 15:21:50
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
*21-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*21-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*------------------------------------------------------------------------------------------------------------
SUBROUTINE L.APAP.VAL.AZ.FA
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AZ.ACCOUNT

    VAR.ID  = ID.NEW

    FN.AZ.ACCOUNT = 'F.AZ.ACCOUNT'
    F.AZ.ACCOUNT = ''
    CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)

    CALL F.READ(FN.AZ.ACCOUNT,VAR.ID,R.AZ.ACCOUNT,F.AZ.ACCOUNT,YERR)

    CALL GET.LOC.REF("AZ.ACCOUNT","L.AC.STATUS2",ACC.POS)

    VAR.AZ.STATUS = R.AZ.ACCOUNT<AZ.LOCAL.REF,ACC.POS>

    FINDSTR "DECEASED" IN VAR.AZ.STATUS SETTING STATUS.POS,VALUE.POS THEN

        TEXT="L.APAP.AZ.FA"

        CURR.NO=1

        CALL STORE.OVERRIDE(CURR.NO)

    END

RETURN
