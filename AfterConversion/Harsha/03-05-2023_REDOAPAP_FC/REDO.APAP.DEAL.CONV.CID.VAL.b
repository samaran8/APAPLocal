* @ValidationCode : MjoxNTMyNjU3MTU1OkNwMTI1MjoxNjgyNTAyNjI2OTgxOklUU1M6LTE6LTE6LTU6MTpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 26 Apr 2023 15:20:26
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : -5
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOAPAP
SUBROUTINE REDO.APAP.DEAL.CONV.CID.VAL(Y.VAL)
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :GANESH.R
*Program   Name    :REDO.APAP.DEAL.CONV.CID.VAL
*Reference Number  :ODR-2007-10-0074
*---------------------------------------------------------------------------------

*DESCRIPTION       :This program is used to display N/A text if value is not present
** Date                  who                   Reference              
* 05-04-2023        �CONVERSTION TOOL   �  R22 AUTO CONVERSTION - No Change
* 05-04-2023          ANIL KUMAR B         R22 MANUAL CONVERSTION -NO CHANGES
*LINKED WITH       :
* ----------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FOREX
    GOSUB PROCESS
RETURN

PROCESS:
    LOC.POS = ''
    CALL MULTI.GET.LOC.REF('FOREX','L.FX.LEGAL.ID',LOC.POS)
    VAR.VAL = R.NEW(FX.LOCAL.REF)<1,LOC.POS>
    IF VAR.VAL EQ '' THEN
        Y.VAL = 'N/A'
    END
    ELSE
        Y.VAL = VAR.VAL
    END
RETURN
END
