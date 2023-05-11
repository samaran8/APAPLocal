* @ValidationCode : MjotMTc0MjY4MzE2ODpDcDEyNTI6MTY4MTM3OTUzNTA2ODpJVFNTQk5HOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 13 Apr 2023 15:22:15
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
SUBROUTINE REDO.DS.FX.BUY.SELL.NAME(IN.OUT.PARA)
*------------------------------------------------------------------------------------------------------------
* DESCRIPTION : This deal slip routine should be attached to the DEAL.SLIP.FORMAT, REDO.BUY.SELL.DSLIP
*------------------------------------------------------------------------------------------------------------
* * Input / Output
* --------------
* IN Parameter    : NA
* OUT Parameter   : NA
*--------------------------------------------------------------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : PRADEEP S
* PROGRAM NAME : REDO.DS.FX.BUY.SELL.NAME
*--------------------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
* Date             Author             Reference                   Description
* 27-Jun-2012      Pradeep S         PACS00204543                 Initial Creation
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*13-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*13-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*----------------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FOREX

    GOSUB INIT
    GOSUB PROCESS

RETURN

*****
INIT:
*****
* Initialisation of variables
*
    Y.ID = IN.OUT.PARA


********
PROCESS:
********
* Getthe description for source

    Y.CCY.BOUGHT = R.NEW(FX.CURRENCY.BOUGHT)
    Y.CCY.SOLD   = R.NEW(FX.CURRENCY.SOLD)

    IF Y.CCY.BOUGHT NE LCCY THEN
        IN.OUT.PARA = "Recibo de Compra de Divisas"
    END ELSE
        IN.OUT.PARA = "Recibo de Venta de Divisas"
    END

RETURN

END
