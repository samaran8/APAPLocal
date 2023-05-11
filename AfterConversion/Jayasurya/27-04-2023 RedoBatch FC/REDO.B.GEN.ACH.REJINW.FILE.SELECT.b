* @ValidationCode : MjotMTczODQ3NjYyOkNwMTI1MjoxNjgyNTc4MzAzMjkyOklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 27 Apr 2023 12:21:43
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.GEN.ACH.REJINW.FILE.SELECT
*-----------------------------------------------------------------------------
*DESCRIPTION:
*-----------------------------------------------------------------------------
* Input/Output:
*-----------------------------------------------------------------------------
* IN  : -NA-
* OUT : -NA-
*
* Dependencies:
*------------------------------------------------------------------------------
* CALLS : -NA-
* CALLED BY : -NA-
*-------------------------------------------------------------------------------
* Revision History:
*--------------------------------------------------------------------------------
*   Date               who           Reference            Description
* 04-Oct-2010        Harish.Y       ODR-2009-12-0290    Initial Creation
* 10-APR-2013        Shesharaj     PERF-CHANGE            Performance Changes
* Date                  who                   Reference
* 11-04-2023        �CONVERSTION TOOL   �  R22 AUTO CONVERSTION - No Change
* 11-04-2023          ANIL KUMAR B         R22 MANUAL CONVERSTION -CALL RTN METHOD ADDED
*--------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.ACH.PROCESS
    $INSERT I_F.REDO.ACH.PROCESS.DET
    $INSERT I_F.REDO.INTERFACE.PARAM
    $INSERT I_F.REDO.ACH.PARAM
    $INSERT I_REDO.B.GEN.ACH.REJINW.FILE.COMMON
    $USING APAP.REDOCHNLS


*!*PERF-CHANGE - Start
*!    SEL.CMD = "SELECT ":FN.REDO.ACH.PROCESS:" WITH EXEC.DATE EQ ":TODAY:" AND PROCESS.TYPE EQ REDO.ACH.INWARD OR PROCESS.TYPE EQ REDO.ACH.REJ.OUTWARD"
    SEL.CMD = "SELECT ":FN.REDO.ACH.PROCESS:" WITH EXEC.DATE EQ ":TODAY : "  AND REJ.GEN NE YES"      ;* Other Conditions are handled in REDO.B.GEN.ACH.REJINW.FILE routine
*!*PERF-CHANGE - End

    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NOR,ERR)
    CALL BATCH.BUILD.LIST('',SEL.LIST)
    IF NOT(NOR) THEN
        INT.CODE = Y.INTERF.ID;
        INT.TYPE = 'BATCH';
        BAT.NO = '1';
        BAT.TOT = '1';
        INFO.OR = 'T24';
        INFO.DE = 'T24';
        ID.PROC = 'ACH004';
        MON.TP = '01';
        DESC = 'There are no files to be processed';
        REC.CON = 'REDO.B.GEN.ACH.REJINW.FILE.SELECT';
        EX.USER = OPERATOR;
        EX.PC = '' ;
*CALL APAP.REDOCHNLS.REDO.INTERFACE.REC.ACT(INT.CODE,INT.TYPE,BAT.NO,BAT.TOT,INFO.OR,INFO.DE,ID.PROC,MON.TP,DESC,REC.CON,EX.USER,EX.PC) ;* MANUAL R22 CODE CONVERSION
        CALL APAP.REDOCHNLS.redoInterfaceRecAct(INT.CODE,INT.TYPE,BAT.NO,BAT.TOT,INFO.OR,INFO.DE,ID.PROC,MON.TP,DESC,REC.CON,EX.USER,EX.PC) ;* MANUAL R22 CODE CONVERSION
    END
RETURN
END
