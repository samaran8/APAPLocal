* @ValidationCode : MjozNTA5OTczOTQ6Q3AxMjUyOjE2ODI1ODUwODYxMjU6SVRTU0JORzotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 27 Apr 2023 14:14:46
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
SUBROUTINE REDO.B.REP.RATES.AND.FEES.POST
*-----------------------------------------------------------------------------------------------------------------
*
* Description           : This routine reads TEMP directory and generates a flat file.
*
* Developed By          : Thilak Kumar K
*
* Development Reference :
*
* Attached To           : BATCH>BNK/REDO.B.REP.RATES.AND.FEES
*
* Attached As           : Batch Routine
*-----------------------------------------------------------------------------------------------------------------
*------------------------
* Input Parameter:
* ---------------*
* Argument#1 : NA
* Argument#2 : NA
* Argument#3 : NA
*-----------------------------------------------------------------------------------------------------------------
*-----------------*
* Output Parameter:
* ----------------*
* Argument#4 : NA
* Argument#5 : NA
* Argument#6 : NA
*-----------------------------------------------------------------------------------------------------------------
*  M O D I F I C A T I O N S
* ***************************
*-----------------------------------------------------------------------------------------------------------------
* Defect Reference       Modified By                    Date of Change        Change Details
*(RTC/TUT/PACS)                                        (YYYY-MM-DD)
*-----------------------------------------------------------------------------------------------------------------
*XXXX                   <<name of modifier>>                                 <<modification details goes here>>
* Date                   who                   Reference
* 13-04-2023         CONVERSTION TOOL     R22 AUTO CONVERSTION - FM TO @FM AND VM TO @VM
* 13-04-2023          ANIL KUMAR B        R22 MANUAL CONVERSTION -CALL RTN METHOD ADDED
*-----------------------------------------------------------------------------------------------------------------
* Include files
*-----------------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.DATES
    $INSERT I_BATCH.FILES
    $INSERT I_F.REDO.H.REPORTS.PARAM
    $INSERT I_REDO.B.REP.RATES.AND.FEES.COMMON
    $USING APAP.REDOCHNLS
*
    GOSUB INITIALIZE
    GOSUB PROCESS
*
RETURN
*-----------------------------------------------------------------------------------------------------------------
INITIALIZE:
*----------
*Initialize the variables
*Open the neccessary files
*-----------------------------------------------------------------------------------------------------------------
    FN.REDO.H.REPORTS.PARAM = "F.REDO.H.REPORTS.PARAM"
    F.REDO.H.REPORTS.PARAM = ""
*
    CALL OPF(FN.REDO.H.REPORTS.PARAM,F.REDO.H.REPORTS.PARAM)
*
    Y.PARAM.ID = BATCH.DETAILS<3,1,1>
    CALL CACHE.READ(FN.REDO.H.REPORTS.PARAM,Y.PARAM.ID,R.REDO.H.REPORTS.PARAM,PARAM.ERR)
*
    IF R.REDO.H.REPORTS.PARAM NE '' THEN
        Y.OUT.DIRECTORY = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.OUT.DIR>
        Y.OUT.FILE.ID   = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.OUT.FILE.NAME>
        Y.TEMP.DIR      = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.TEMP.DIR>
        Y.OUT.FILE.PATH = CHANGE(Y.OUT.DIRECTORY,@VM,'')
        Y.OUT.FILE.NAME = Y.OUT.FILE.ID:".":R.DATES(EB.DAT.LAST.WORKING.DAY):'.txt'
    END
*
    Y.TEMP.ID = ''
    FN.REDO.REPORT.TEMP = Y.TEMP.DIR
    F.REDO.REPORT.TEMP = ""
    CALL OPF(FN.REDO.REPORT.TEMP,F.REDO.REPORT.TEMP)
*
    OPENSEQ Y.OUT.FILE.PATH,Y.OUT.FILE.NAME TO Y.SEQ.PTR ELSE
        CREATE Y.OUT.FILE.NAME ELSE
            Y.ERR.MSG = "Unable to Open '":Y.OUT.FILE.NAME:"'"
            GOSUB RAISE.ERR.C.22
            RETURN
        END
    END
*
RETURN
*-----------------------------------------------------------------------------------------------------------------
PROCESS:
*-------
*
*Frame Loop and Remove the id
*Read APAP.REPORT.TEMP If Record Exits then store it to an array
*Else Raise
*-----------------------------------------------------------------------------------------------------------------

    SEL.CMD = "SELECT ":FN.REDO.REPORT.TEMP:" WITH @ID LIKE ":"...":Y.OUT.FILE.ID:"..."
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,RET.CODE)
    LOOP
        REMOVE Y.TEMP.ID FROM SEL.LIST SETTING TEMP.POS
    WHILE Y.TEMP.ID:TEMP.POS
        CALL F.READ(FN.REDO.REPORT.TEMP,Y.TEMP.ID,R.REDO.REPORT.TEMP,F.REDO.REPORT.TEMP,TEMP.ERR)
        IF R.REDO.REPORT.TEMP NE '' THEN
            Y.RECORD = R.REDO.REPORT.TEMP
            CALL F.DELETE(FN.REDO.REPORT.TEMP,Y.TEMP.ID)
            GOSUB WRITE.FILE
        END
    REPEAT
*
RETURN
*-----------------------------------------------------------------------------------------------------------------
WRITE.FILE:
*----------
    CHANGE @FM TO CHARX(13):CHARX(10) IN Y.RECORD
    WRITESEQ Y.RECORD APPEND TO Y.SEQ.PTR ELSE
        Y.ERR.MSG = "Unable to Write '":Y.OUT.FILE.NAME:"'"
        GOSUB RAISE.ERR.C.22
        RETURN
    END


RETURN
*-----------------------------------------------------------------------------------------------------------------
RAISE.ERR.C.22:
*--------------
*Handling error process
*---------------
    MON.TP = "04"
    Y.ERR.MSG = "Record not found"
    REC.CON = "Receperciones-":Y.ERR.MSG
    DESC = "Receperciones-":Y.ERR.MSG
    INT.CODE = 'REP001'
    INT.TYPE = 'ONLINE'
    BAT.NO   = ''
    BAT.TOT  = ''
    INFO.OR  = ''
    INFO.DE  = ''
    ID.PROC  = ''
    EX.USER  = ''
    EX.PC    = ''
* CALL APAP.REDOCHNLS.REDO.INTERFACE.REC.ACT(INT.CODE,INT.TYPE,BAT.NO,BAT.TOT,INFO.OR,INFO.DE,ID.PROC,MON.TP,DESC,REC.CON,EX.USER,EX.PC) ;*MANUAL R22 CODE CONVERSION
    CALL APAP.REDOCHNLS.redoInterfaceRecAct(INT.CODE,INT.TYPE,BAT.NO,BAT.TOT,INFO.OR,INFO.DE,ID.PROC,MON.TP,DESC,REC.CON,EX.USER,EX.PC) ;*MANUAL R22 CODE CONVERSION
*
RETURN
*-----------------------------------------------------------------------------------------------------------------
END
