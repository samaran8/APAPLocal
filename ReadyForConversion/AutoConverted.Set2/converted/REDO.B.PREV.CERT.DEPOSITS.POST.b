SUBROUTINE REDO.B.PREV.CERT.DEPOSITS.POST
*-----------------------------------------------------------------------------------------------------------------
*
* Description           : This routine reads REDO.B.PREV.CERT.DEPOSITS and generates a flat file.
*
* Developed By          : Amaravathi Krithika B
*
* Development Reference : 198_CA01
*
* Attached To           : BATCH>BNK/REDO.B.PREV.CERT.DEPOSITS
*
* Attached As           : Batch Routine
*-----------------------------------------------------------------------------------------------------------------
*------------------------
* Input Parameter:
* ---------------*
* Argument#3 : NA
*-----------------*
* Output Parameter:
* ----------------*
* Argument#6 : NA
*-----------------------------------------------------------------------------------------------------------------
*  M O D I F I C A T I O N S
* ***************************
*-----------------------------------------------------------------------------------------------------------------
* Defect Reference       Modified By                    Date of Change        Change Details
*(RTC/TUT/PACS)                                        (YYYY-MM-DD)
*-----------------------------------------------------------------------------------------------------------------
*XXXX                   <<name of modifier>>                                 <<modification details goes here>>
*-----------------------------------------------------------------------------------------------------------------
* Include files

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.DATES
    $INSERT I_F.REDO.H.REPORTS.PARAM

    SLEEP 30
    GOSUB INITIALIZE
    GOSUB PROCESS
RETURN

INITIALIZE:
*----------
    FN.REDO.H.REPORT.PARAM = "F.REDO.H.REPORTS.PARAM"
    F.REDO.H.REPORT.PARAM = ""
    CALL OPF(FN.REDO.H.REPORT.PARAM,F.REDO.H.REPORT.PARAM)
    FN.DR.REG.CA01.WORKFILE = 'F.DR.REG.CA01.WORKFILE'
    F.DR.REG.CA01.WORKFILE = ''
    CALL OPF(FN.DR.REG.CA01.WORKFILE,F.DR.REG.CA01.WORKFILE)
    Y.PARAM.ID = "REDO.CA01"
    R.REDO.H.REPORTS.PARAM = ''; PARAM.ERR = ''
    CALL CACHE.READ(FN.REDO.H.REPORT.PARAM,Y.PARAM.ID,R.REDO.H.REPORT.PARAM,PARAM.ERR)
    IF R.REDO.H.REPORT.PARAM NE '' THEN
        Y.OUT.FILE.NAME = R.REDO.H.REPORT.PARAM<REDO.REP.PARAM.OUT.FILE.NAME>
        Y.OUT.FILE.PATH = R.REDO.H.REPORT.PARAM<REDO.REP.PARAM.OUT.DIR>
    END ELSE
        Y.ERR.MSG   = "REDO.H.REPORT.PARAM record ":Y.PARAM.ID:" not found"
        GOSUB RAISE.ERR.C.22
        RETURN
    END
*
    FN.CHK.DIR=R.REDO.H.REPORT.PARAM<REDO.REP.PARAM.OUT.DIR>
    F.CHK.DIR = ''
    CALL OPF(FN.CHK.DIR,F.CHK.DIR)
    Y.FILE.NAME = Y.OUT.FILE.NAME:'.':R.DATES(EB.DAT.LAST.WORKING.DAY):'.txt'
    CALL F.READ(FN.CHK.DIR,Y.FILE.NAME,R.FIL,F.CHK.DIR,READ.FIL.ERR)
    IF R.FIL THEN
        DELETE F.CHK.DIR,Y.FILE.NAME
    END
RETURN
*
PROCESS:
*-------
    R.FILE.DATA = ''
    SEL.CMD = "SELECT ":FN.DR.REG.CA01.WORKFILE
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,RET.CODE)
    LOOP
        REMOVE Y.TEMP.ID FROM SEL.LIST SETTING TEMP.POS
    WHILE Y.TEMP.ID:TEMP.POS
        R.REDO.REPORT.TEMP = '';        TEMP.ERR = ''
        CALL F.READ(FN.DR.REG.CA01.WORKFILE,Y.TEMP.ID,R.REDO.REPORT.TEMP,F.DR.REG.CA01.WORKFILE,TEMP.ERR)
        IF R.REDO.REPORT.TEMP THEN
            R.FILE.DATA<-1> = R.REDO.REPORT.TEMP
        END
    REPEAT
    CHANGE @FM TO CHARX(13):CHARX(10) IN R.FILE.DATA
    WRITE R.FILE.DATA ON F.CHK.DIR, Y.FILE.NAME ON ERROR
        CALL OCOMO("Unable to write to the file":F.CHK.DIR)
    END
RETURN
*
RAISE.ERR.C.22:
*-------------
    MON.TP = "04"
    REC.CON = "CA01-":Y.ERR.MSG
    DESC = "CA01-":Y.ERR.MSG
    INT.CODE = 'REP001'
    INT.TYPE = 'ONLINE'
    BAT.NO = ''
    BAT.TOT = ''
    INFO.OR = ''
    INFO.DE = ''
    ID.PROC = ''
    EX.USER = ''
    EX.PC = ''
    CALL REDO.INTERFACE.REC.ACT(INT.CODE,INT.TYPE,BAT.NO,BAT.TOT,INFO.OR,INFO.DE,ID.PROC,MON.TP,DESC,REC.CON,EX.USER,EX.PC)
RETURN
END
