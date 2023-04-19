* @ValidationCode : MjotMTgzNjAxOTI2OkNwMTI1MjoxNjgxODc5MjMzMDQwOklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 19 Apr 2023 10:10:33
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
SUBROUTINE REDO.DS.SC.AMOUNT(Y.AMOUNT)
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos.
*Developed By      :btorresalbornoz
*Program   Name    :REDO.DS.SC.CCY
*Modify            :btorresalbornoz
*---------------------------------------------------------------------------------
*DESCRIPTION       :This program is used to get the Y.CCY value from EB.LOOKUP TABLE
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*13-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 = TO EQ, FREAD TO CACHEREAD
*13-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES

* ----------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.TELLER
    $INSERT I_F.USER
    $INSERT I_F.CURRENCY

    GOSUB PROCESS
RETURN
*********
PROCESS:
*********

    FN.CURRENCY = 'F.CURRENCY'
    F.CURRENCY = ''
    CALL OPF(FN.CURRENCY,F.CURRENCY)

    Y.CURRENCY=R.NEW(TT.TE.CURRENCY.1)
    CALL CACHE.READ(FN.CURRENCY, Y.CURRENCY, R.CURRENCY, Y.ERR) ;* AUTO R22 CODE CONVERSION
    Y.CCY=R.CURRENCY<EB.CUR.CCY.NAME>


    IF Y.CCY EQ 'PESOS DOMINICANOS' THEN ;* AUTO R22 CODE CONVERSION

        Y.AMOUNT=R.NEW(TT.TE.AMOUNT.LOCAL.1)
        Y.AMOUNT=FMT(Y.AMOUNT,"R2,#19")
    END
    IF Y.CCY EQ 'USD DOLLAR' THEN ;* AUTO R22 CODE CONVERSION

        Y.AMOUNT=R.NEW(TT.TE.AMOUNT.FCY.1)
        Y.AMOUNT=FMT(Y.AMOUNT,"R2,#19")
    END



RETURN
END
