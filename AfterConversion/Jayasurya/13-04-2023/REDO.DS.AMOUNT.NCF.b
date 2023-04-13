* @ValidationCode : Mjo4NDk1MDMyOTk6Q3AxMjUyOjE2ODEzNzc3MDk1NDM6SVRTU0JORzotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 13 Apr 2023 14:51:49
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
SUBROUTINE REDO.DS.AMOUNT.NCF(Y.AMOUNT)
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :btorresalbornoz
*Program   Name    :REDO.DS.SC.CCY
*Modify            :btorresalbornoz
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*13-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 = TO EQ, FREAD TO CACHEREAD
*13-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*---------------------------------------------------------------------------------
*DESCRIPTION       :This program is used to get the Y.CCY value from EB.LOOKUP TABLE
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
    CALL CACHE.READ(FN.CURRENCY, Y.CURRENCY, R.CURRENCY, Y.ERR)  ;*AUTO R22 CODE CONVERSION
    Y.CCY=R.CURRENCY<EB.CUR.CCY.NAME>


    IF Y.CCY EQ 'PESOS DOMINICANOS' THEN  ;*AUTO R22 CODE CONVERSION

        Y.AMOUNT=R.NEW(TT.TE.AMOUNT.LOCAL.1)
        Y.AMOUNT=FMT(Y.AMOUNT,"R2,#13")
    END
    IF Y.CCY EQ 'USD DOLLAR' THEN ;*AUTO R22 CODE CONVERSION

        Y.AMOUNT=R.NEW(TT.TE.AMOUNT.FCY.1)
        Y.AMOUNT=FMT(Y.AMOUNT,"R2,#13")
    END



RETURN
END
