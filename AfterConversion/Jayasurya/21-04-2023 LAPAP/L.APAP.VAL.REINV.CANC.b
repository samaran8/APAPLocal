* @ValidationCode : MjotMjAzNjU5Mzk4NjpDcDEyNTI6MTY4MjA3MTM1ODg5OTpJVFNTQk5HOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 21 Apr 2023 15:32:38
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
*21-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                BP REMOVED
*21-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*------------------------------------------------------------------------------------------------------------------
SUBROUTINE L.APAP.VAL.REINV.CANC
    $INSERT I_COMMON ;* AUTO R22 CODE CONVERSION START
    $INSERT I_EQUATE
    $INSERT I_F.AZ.ACCOUNT ;* AUTO R22 CODE CONVERSION END

    VAR.ID  = ID.NEW

    FN.AZ.ACCOUNT = 'F.AZ.ACCOUNT'
    F.AZ.ACCOUNT = ''
    CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)

    CALL F.READ(FN.AZ.ACCOUNT,VAR.ID,R.AZ.ACCOUNT,F.AZ.ACCOUNT,YERR)

    VAR.CATEG  =  R.AZ.ACCOUNT<AZ.CATEGORY>
    VAR.ROLLOVER = R.AZ.ACCOUNT<AZ.ROLLOVER.DATE>

    IF  VAR.CATEG EQ 6614 OR VAR.CATEG  EQ 6615  THEN

        IF VAR.ROLLOVER EQ '' THEN

            TEXT="L.APAP.REINV"

            CURR.NO=1

            CALL STORE.OVERRIDE(CURR.NO)

        END


    END

RETURN
