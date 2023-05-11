* @ValidationCode : MjoxNzQxNTczMDM3OkNwMTI1MjoxNjgxMjc2MDA5NTk4OklUU1M6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 12 Apr 2023 10:36:49
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
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.LY.FILE.GEN.SELECT
*-------------------------------------------------------------------------------------------
*DESCRIPTION:
*             This routine selects the REDO.LY.PROGRAM file ids
* ------------------------------------------------------------------------------------------
* Input/Output:
*--------------
* IN  : -NA-
* OUT : -NA-
*
* Dependencies:
*---------------
* CALLS     : -NA-
* CALLED BY : -NA-
*
* Revision History:
*------------------
*   Date            who           Reference            Description
* 03-MAY-2010   S.Marimuthu  ODR-2009-12-0276      Initial Creation
* 28-Sep-2010   S.Sudharsanan  0DR-2010-09-0012     Modification has done as per the CR-021
* Date                  who                   Reference              
* 12-04-2023        �CONVERSTION TOOL   �  R22 AUTO CONVERSTION - No Change
* 12-04-2023          ANIL KUMAR B         R22 MANUAL CONVERSTION -NO CHANGES
*---------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.LY.PROGRAM
    $INSERT I_F.REDO.LY.POINTS
    $INSERT I_F.DATES
    $INSERT I_REDO.B.LY.FILE.GEN.COMMON
*-----------------------------------------------------------------------------
    GOSUB SELECT.PRGM
    GOSUB PROGRAM.END
*-----------------------------------------------------------------------------
SELECT.PRGM:
*-----------------------------------------------------------------------------

    IF LASTMONTHWDAY EQ 'N' THEN
        SEL.CMD = 'SELECT ':FN.REDO.LY.PROGRAM:' WITH GEN.FREC EQ DIARIO AND POINT.USE EQ 2'
        CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.REC,PRGM.ERR)
        VAR.TOT.REC = NO.REC
        CALL BATCH.BUILD.LIST('',SEL.LIST)
    END

    IF LASTMONTHWDAY EQ 'Y' THEN
        SEL.CMD = 'SELECT ':FN.REDO.LY.PROGRAM:' WITH GEN.FREC EQ MENSUAL AND POINT.USE EQ 2'
        CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.REC,PRGM.ERR)
        VAR.TOT.REC = NO.REC
        CALL BATCH.BUILD.LIST('',SEL.LIST)
    END

RETURN
*-----------------------------------------------------------------------------
PROGRAM.END:
*-----------------------------------------------------------------------------
END
