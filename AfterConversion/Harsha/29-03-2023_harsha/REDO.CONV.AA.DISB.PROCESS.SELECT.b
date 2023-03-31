* @ValidationCode : MjotMTA4NTIxNzk0MTpDcDEyNTI6MTY4MDE1Mzg2MzU4NTpJVFNTOi0xOi0xOjA6MTpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 30 Mar 2023 10:54:23
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
$PACKAGE APAP.AA
SUBROUTINE REDO.CONV.AA.DISB.PROCESS.SELECT

*-------------------------------------------------
*Description: This batch routine is to change the arrangement status for inconsistent entries
*-------------------------------------------------
*------------------------------------------------------------------------
* Modification History :
*------------------------------------------------------------------------
*  DATE             WHO                   REFERENCE            
* 29-MAR-2023      Conversion Tool       R22 Auto Conversion - No changes      
* 29-MAR-2023      Harsha                R22 Manual Conversion - No changes                             
*------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.DATES
    $INSERT I_REDO.CONV.AA.DISB.PROCESS.COMMON

    GOSUB PROCESS

RETURN
*-------------------------------------------------
PROCESS:
*-------------------------------------------------

    SEL.CMD = "SELECT ":FN.REDO.DISB.CHAIN:" WITH DATE LT '20170811'"
    SEL.LIST = ''
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NOR,ERR)
    CALL BATCH.BUILD.LIST('',SEL.LIST)

RETURN
END
