* @ValidationCode : MjotMTU4NDQ4MDgxODpDcDEyNTI6MTY4NDg1NDM5NDgyMTpJVFNTOi0xOi0xOjI3NzoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 23 May 2023 20:36:34
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 277
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.PURGE.UPD.ALL.IDS
*---------------------------------------------------------------------------------------------
*
* Description           : Batch routine to purge records in REDO.L.ALL.FT.TT.FX.IDS in daily COB
*
* Developed By          : Thenmalar
*
* Development Reference : TC01
*
* Attached To           : Batch -BNK/REDO.B.PURGE.UPD.ALL.IDS
*
* Attached As           : Online Batch Routine to COB
*---------------------------------------------------------------------------------------------
* Input Parameter:
*----------------*
*
*-----------------*
* Output Parameter:
*-----------------*
* Argument#4 : NA
*
*---------------------------------------------------------------------------------------------
*  M O D I F I C A T I O N S
* ***************************
*---------------------------------------------------------------------------------------------
* Defect Reference       Modified By                    Date of Change        Change Details
* NA                     Thenmalar T                      19-Feb-2014           Modified as per clarificaiton received
* Date                  who                   Reference              
* 12-04-2023        �CONVERSTION TOOL   �  R22 AUTO CONVERSTION - No Change
* 12-04-2023          ANIL KUMAR B         R22 MANUAL CONVERSTION -NO CHANGES
*---------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.DATES
    $INSERT I_F.REDO.L.ALL.FT.TT.FX.IDS

    GOSUB INIT
    GOSUB PROCESS
*
RETURN
*---------------------------------------------------------------------------------------------
INIT:
****
*
    Y.LWD = R.DATES(EB.DAT.LAST.WORKING.DAY)
    Y.DISPLACEMENT = '1W'
    CALL CALENDAR.DAY(Y.LWD,"-",Y.DISPLACEMENT)
    Y.LLWD = Y.DISPLACEMENT
*
    Y.RET.VAL = '' ; Y.RET.POS= '' ; Y.POS = ''
*
    FN.REDO.L.ALL.FT.TT.FX.IDS = 'F.REDO.L.ALL.FT.TT.FX.IDS'
    F.REDO.L.ALL.FT.TT.FX.IDS = ''
    CALL OPF(FN.REDO.L.ALL.FT.TT.FX.IDS,F.REDO.L.ALL.FT.TT.FX.IDS)
*
RETURN
*---------------------------------------------------------------------------------------------
PROCESS:
********
*
* Routine runs in daily COB but needs to be executed at first COB in the month

    Y.TODAY = TODAY
    IF Y.TODAY[5,2] NE Y.LLWD[5,2] THEN
        Y.LAST.DATE = Y.LLWD[1,6]:'31'

        SEL.CMD = "SELECT ":FN.REDO.L.ALL.FT.TT.FX.IDS:" WITH DATE LE ":Y.LAST.DATE

        EXECUTE SEL.CMD CAPTURING Y.RET.VAL SETTING Y.RET.POS
        FINDSTR "No" IN Y.RET.VAL SETTING Y.POS THEN
            RETURN
        END

        SEL.CMD1 = "DELETE ":FN.REDO.L.ALL.FT.TT.FX.IDS
        EXECUTE SEL.CMD1
    END

RETURN
*---------------------------------------------------------------------------------------------
END
*--
