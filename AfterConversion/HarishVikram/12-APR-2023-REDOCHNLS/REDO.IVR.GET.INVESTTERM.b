* @ValidationCode : MjotMjEzNTM5ODI2MzpDcDEyNTI6MTY4MTI3Nzg1Nzc0MDpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 12 Apr 2023 11:07:37
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
$PACKAGE APAP.REDOCHNLS
SUBROUTINE REDO.IVR.GET.INVESTTERM
*-------------------------------------------------------------------------
*DESCRIPTION:
*------------
*   To get the number of days as investment term.
*-------------------------------------------------------------------------
* HISTORY:
*---------
*   Date               who           Reference            Description

* 08-MAY-2014       RMONDRAGON      ODR-2011-02-0099     Initial Creation
*
* 11-APR-2023     Conversion tool    R22 Auto conversion       No changes
* 12-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_ENQUIRY.COMMON

    GOSUB PROCESS

RETURN

*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------

    Y.START.DATE = FIELD(O.DATA,'-',1)
    Y.END.DATE = FIELD(O.DATA,'-',2)

    CALL CDD('',Y.START.DATE,Y.END.DATE,Y.DIFF)

    O.DATA = Y.DIFF:'D'

RETURN

*-----------------------------------------------------------------------------
END
