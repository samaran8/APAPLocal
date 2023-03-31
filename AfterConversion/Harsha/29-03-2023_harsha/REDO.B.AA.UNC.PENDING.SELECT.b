* @ValidationCode : MjoxNDA4MzUyODM4OkNwMTI1MjoxNjgwMTUzODYxMzYwOklUU1M6LTE6LTE6MDoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 30 Mar 2023 10:54:21
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
SUBROUTINE REDO.B.AA.UNC.PENDING.SELECT

*-------------------------------------------------
*Description: This batch routine is to change the arrangement status for inconsistent entries
*-------------------------------------------------
*------------------------------------------------------------------------
* Modification History :
*------------------------------------------------------------------------
*  DATE             WHO                   REFERENCE   
* 29-MAR-2023      Conversion Tool       R22 Auto Conversion - No changes               
* 29-MAR-2023      Harsha                R22 Manual Conversion - No changes
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.DATES
    $INSERT I_F.REDO.AA.UNC.PENDING
    $INSERT I_REDO.B.AA.UNC.PENDING.COMMON

    GOSUB PROCESS

RETURN
*-------------------------------------------------
PROCESS:
*-------------------------------------------------

    SEL.CMD = "SELECT ":FN.REDO.AA.UNC.PENDING:" WITH ARR.STATUS EQ 'PENDING.CLOSURE'"
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,SEL.ERR)
    CALL BATCH.BUILD.LIST('',SEL.LIST)

RETURN
END
