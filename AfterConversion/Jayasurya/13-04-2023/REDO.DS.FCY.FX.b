* @ValidationCode : MjotMTIyNDYwNzYwMjpDcDEyNTI6MTY4MTM3OTI4NDU2NDpJVFNTQk5HOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 13 Apr 2023 15:18:04
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
SUBROUTINE REDO.DS.FCY.FX(Y.CCY)
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :SUDHARSANAN S
*Program   Name    :REDO.DS.FCY.FX
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*13-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*13-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*---------------------------------------------------------------------------------
*DESCRIPTION       : This program is used to get the F.Currency type of the txn.
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
    IF Y.CCY EQ LCCY THEN
        Y.CCY = Y.CCY.2
    END
*
RETURN
*
*****
INIT:
*****
*
    Y.CCY.2        = ""
*
    IF APPLICATION EQ "TELLER" THEN
        Y.CCY.2  = R.NEW(TT.TE.CURRENCY.2)
    END
*
RETURN
*
END
