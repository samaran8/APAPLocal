* @ValidationCode : MjoxMjA5MDI5MDYyOkNwMTI1MjoxNjgyMDczNTE3ODExOklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 21 Apr 2023 16:08:37
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
*21-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            CALL RTN METHOD ADDED
*----------------------------------------------------------------------------------------------------------------
SUBROUTINE LAPAP.AZ.PAYMET.METHOD1

    $INSERT I_COMMON ;* AUTO R22 CODE CONVERSION START
    $INSERT I_EQUATE
    $INSERT I_F.AZ.ACCOUNT ;* AUTO R22 CODE CONVERSION END


    ID = COMI
    CALL APAP.LAPAP.LAPAP.MON.DEFINE.PAYMENT(ID,RS,RT) ;* MANUAL R22 CODE CONVERSION

    IF RS EQ "CASHDEPOSIT" THEN
        COMI = RT
    END ELSE
        COMI = ""
    END


*DEBUG

END
