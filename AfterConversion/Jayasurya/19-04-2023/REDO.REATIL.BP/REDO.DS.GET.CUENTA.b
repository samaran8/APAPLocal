* @ValidationCode : MjoxODkyNzE2OTUxOkNwMTI1MjoxNjgxODc5OTI5NzI3OklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 19 Apr 2023 10:22:09
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
SUBROUTINE REDO.DS.GET.CUENTA(VAR.ACCOUNT.NO)
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :S SUDHARSANAN
*Program   Name    :REDO.DS.GET.CUENTA
*---------------------------------------------------------------------------------
* DESCRIPTION       :This program is used to get the account value
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*13-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*13-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            CALL RTN METHOD ADDED

* ----------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT.CLOSURE
    $INSERT I_F.AZ.ACCOUNT

    GOSUB PROCESS

RETURN
*********
PROCESS:
**********
    BEGIN CASE

        CASE APPLICATION EQ 'ACCOUNT.CLOSURE'

            Y.ACCOUNT.NO  = R.NEW(AC.ACL.SETTLEMENT.ACCT)

            IF PGM.VERSION EQ ',REDO.EN.LINEA' THEN
                VAR.ACCOUNT.NO = Y.ACCOUNT.NO

            END ELSE
                VAR.ACCOUNT.NO = ''
            END

        CASE APPLICATION EQ 'AZ.ACCOUNT'

            Y.ACCOUNT.NO = R.NEW(AZ.NOMINATED.ACCOUNT)

            VAR.PAY.FORM = ''
            CALL APAP.REDORETAIL.REDO.DS.GET.PAY.FORM(VAR.PAY.FORM) ;* MANUAL R22 CODE CONVERSION
* WHILE DEBUG CHECK PAY.FORM VALUE IS AVAILABLE IN R.NEW

            IF ((PGM.VERSION EQ ',NOR.PRECLOSURE') OR (PGM.VERSION EQ ',NOR.PRECLOSURE.AUTH')) AND (VAR.PAY.FORM NE 'EFFECTIVO' AND ISDIGIT(Y.ACCOUNT.NO[1,3])) THEN       ;*PACS00930966
                VAR.ACCOUNT.NO = Y.ACCOUNT.NO
            END ELSE
                VAR.ACCOUNT.NO = ''
            END

    END CASE

RETURN
END
*----------------------------------------------- End Of Record ----------------------------------
