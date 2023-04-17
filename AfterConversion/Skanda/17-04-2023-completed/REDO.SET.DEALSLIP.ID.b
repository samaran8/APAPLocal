* @ValidationCode : MjotMTIzODI4MDkzNzpDcDEyNTI6MTY4MTcwMjc1MzEwMjpJVFNTOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 17 Apr 2023 09:09:13
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.SET.DEALSLIP.ID
*----------------------------------------------------------------------------------------------------------------------
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: Vignesh Kumaar M R
* PROGRAM NAME: REDO.SET.DEALSLIP.ID
* ODR NO      :
*----------------------------------------------------------------------------------------------------------------------
* DESCRIPTION: This routine is used to store the Contract id for dealslip printing
*
* IN PARAMETER: NA
* OUT PARAMETER: NA
* LINKED WITH: TELLER & FT
*
*----------------------------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
* DATE         WHO                 REFERENCE         DESCRIPTION
* 20/08/2013   Vignesh Kumaar R    PACS00305984      INITIAL CREATION
** 17-04-2023 R22 Auto Conversion no changes
** 17-04-2023 Skanda R22 Manual Conversion - added APAP.TAM
*----------------------------------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_System

    $INSERT I_F.TELLER
    $INSERT I_F.FUNDS.TRANSFER

* Fix for PACS00305984 [CASHIER DEAL SLIP PRINT OPTION]
    Y.APPL = "TELLER"         ;* PACS00648154 -S
    Y.FLD = "L.INITIAL.ID"
    Y.FLD.POS = ''
    CALL APAP.TAM.MULTI.GET.LOC.REF(Y.APPL,Y.FLD,Y.FLD.POS) ;* R22 Manual conversion
    Y.INT.POS = Y.FLD.POS<1,1>
    Y.INT.ID = R.NEW(TT.TE.LOCAL.REF)<1,Y.INT.POS>
    IF Y.INT.ID EQ '' THEN
        Y.INT.ID = ID.NEW
    END   ;* PACS00648154 -E
    CALL System.setVariable("CURRENT.WTM.FIRST.ID",Y.INT.ID)

* End of Fix

RETURN
