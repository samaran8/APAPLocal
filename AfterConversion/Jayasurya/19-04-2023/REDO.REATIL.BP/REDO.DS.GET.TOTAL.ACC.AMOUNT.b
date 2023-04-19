* @ValidationCode : MjotMzc5ODU5MjI3OkNwMTI1MjoxNjgxMzgyNDMzNDg4OklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 13 Apr 2023 16:10:33
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
SUBROUTINE REDO.DS.GET.TOTAL.ACC.AMOUNT(VAR.TOT.AMOUNT)
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :S SUDHARSANAN
*Program   Name    :REDO.DS.GET.TOTAL.ACC.AMOUNT
*---------------------------------------------------------------------------------
* DESCRIPTION       :This program is used to get the AMOUNT VALUE
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*13-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*13-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES

* ----------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT.CLOSURE

    GOSUB PROCESS

RETURN
*********
PROCESS:
**********

    Y.AMOUNT = R.NEW(AC.ACL.TOTAL.ACC.AMT)
    Y.CURR = R.NEW(AC.ACL.CURRENCY)


    Y.AMOUNT = TRIM(FMT(Y.AMOUNT,"L2,#19")," ",'B')

    VAR.TOT.AMOUNT = Y.CURR:" ":Y.AMOUNT

RETURN
END
*----------------------------------------------- End Of Record ----------------------------------
