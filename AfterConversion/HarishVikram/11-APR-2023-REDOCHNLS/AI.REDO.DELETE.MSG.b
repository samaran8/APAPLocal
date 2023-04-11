* @ValidationCode : MjotMTIwODExOTMwOTpDcDEyNTI6MTY4MTE5NjQ2NzcyMjpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 11 Apr 2023 12:31:07
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
SUBROUTINE AI.REDO.DELETE.MSG
*-----------------------------------------------------------------------------
*Company   Name     : APAP
*Developed By       : Martin Macias
*Program   Name     : AI.REDO.DELETE.MSG
*-----------------------------------------------------------------------------
*Functionality      : Next Command
*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
*
* DATE              WHO                REFERENCE                 DESCRIPTION
* 11-APR-2023     Conversion tool    R22 Auto conversion       No changes
* 11-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_GTS.COMMON
    $INSERT I_EQUATE

    NEXT.COMMAND = 'OVERRIDE,AI.REDO.DEL.MSG S AI.REDO.DEL.MSG'
    CALL EB.SET.NEXT.TASK(NEXT.COMMAND)

END
