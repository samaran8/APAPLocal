* @ValidationCode : MjotMTA5MzcyMzgxMzpDcDEyNTI6MTY4MTIxMDEzNDIzNTpJVFNTOi0xOi0xOjA6MTpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 11 Apr 2023 16:18:54
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.H.REPORTS.PARAM.VALIDATE
*----------------------------------------------------------------------------------------------------------------
*
* Description           : This routine is used to validated the path and file name defined in OUT.DIR and OUT.FILE.NAME fields
*
* Developed By          : Saranraj S
*
* Development Reference : DE04
*
* Attached To           : N/A
*
* Attached As           : Validation Routine
*-----------------------------------------------------------------------------------------------------------------
* Input Parameter:
*----------------*
* Argument#1 : NA
*
*-----------------*
* Output Parameter:
*-----------------*
* Argument#4 : NA
*
*-----------------------------------------------------------------------------------------------------------------
*  M O D I F I C A T I O N S
* ***************************
*-----------------------------------------------------------------------------------------------------------------
* Defect Reference       Modified By                    Date of Change        Change Details
* (RTC/TUT/PACS)                                        (YYYY-MM-DD)
*   -                    Krishnaveni G                   2013-10-17           Validation added to new temp directory field
*
* Date             Who                   Reference      Description
* 11.04.2023       Conversion Tool       R22            Auto Conversion     - FM TO @FM, VM TO @VM
* 11.04.2023       Shanmugapriya M       R22            Manual Conversion   - No changes
*
*-----------------------------------------------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------------------------------------------
* Include files
*-----------------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.H.REPORTS.PARAM

    GOSUB INIT
    GOSUB CHK.DIR.PROCESS
    GOSUB CHK.FILE.NAME

RETURN
*----
INIT:
*----
    FN.REDO.H.REPORTS.PARAM = 'F.REDO.H.REPORTS.PARAM'
    F.REDO.H.REPORTS.PARAM = ''
    CALL OPF(FN.REDO.H.REPORTS.PARAM,F.REDO.H.REPORTS.PARAM)

RETURN
*---------------
CHK.DIR.PROCESS:
*---------------
* (S) 20131017
    Y.DIRECTORY = R.NEW(REDO.REP.PARAM.OUT.DIR)
    Y.OUT.FILE  = R.NEW(REDO.REP.PARAM.OUT.FILE.NAME)
    CHANGE @VM TO '' IN Y.DIRECTORY

    IF Y.DIRECTORY THEN
        OPEN Y.DIRECTORY TO Y.PTR ELSE

            AF    = REDO.REP.PARAM.OUT.DIR
            AV    = 1
            ETEXT = "EB-DIR.DOES.NOT.EXIST"
            CALL STORE.END.ERROR
        END
    END

    Y.DIRECTORY = R.NEW(REDO.REP.PARAM.TEMP.DIR)
    CHANGE @VM TO '' IN Y.DIRECTORY

    IF Y.DIRECTORY THEN
        OPEN Y.DIRECTORY TO Y.PTR ELSE

            AF    = REDO.REP.PARAM.TEMP.DIR
            AV    = 1
            ETEXT = "EB-DIR.DOES.NOT.EXIST"
            CALL STORE.END.ERROR
        END
    END
*(E) 20131017
RETURN
*-------------
CHK.FILE.NAME:
*-------------
    SEL.CMD = "SELECT ":FN.REDO.H.REPORTS.PARAM:" WITH OUT.FILE.NAME EQ ":Y.OUT.FILE
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.RECS,Y.LIST.ERR)
    Y.REC.ID = SEL.LIST
    IF Y.REC.ID NE ID.NEW AND Y.REC.ID NE '' THEN
        AF    = REDO.REP.PARAM.OUT.FILE.NAME
        ETEXT = 'EB-REDO.REP.DUP.FILE.NAME':@FM:Y.REC.ID
        CALL STORE.END.ERROR
    END
RETURN
END
