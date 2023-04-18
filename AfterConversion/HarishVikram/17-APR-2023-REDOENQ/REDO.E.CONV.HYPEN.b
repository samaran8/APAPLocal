* @ValidationCode : MjozNzYxODg3NDpDcDEyNTI6MTY4MTcyMTEyMTI2NTpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 17 Apr 2023 14:15:21
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.E.CONV.HYPEN

*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :Madhupriya
*Program   Name    :REDO.E.CONV.HYPEN
*Reference Number  : ODR-2009-12-0283
*---------------------------------------------------------------------------------

*DESCRIPTION       :This program is to remove the hypen in the customer name
*
*LINKED WITH       :
* ----------------------------------------------------------------------------------
*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
*
* DATE              WHO                REFERENCE                 DESCRIPTION
* 17-APR-2023     Conversion tool    R22 Auto conversion       No changes
* 17-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.ENQUIRY

    Y.VAL = ''
    Y.VAL = O.DATA

    CHANGE '-' TO " " IN Y.VAL

    O.DATA = Y.VAL

RETURN
END
