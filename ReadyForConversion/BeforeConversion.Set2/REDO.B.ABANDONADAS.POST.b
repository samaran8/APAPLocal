*-----------------------------------------------------------------------------
* <Rating>-35</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE REDO.B.ABANDONADAS.POST
*-------------------------------------------------------------------------------
* Company Name      : PAGE SOLUTIONS, INDIA
* Developed By      : Nirmal.P
* Reference         :
*-------------------------------------------------------------------------------
* Subroutine Type   : B
* Attached to       :
* Attached as       : Multi threaded Batch Routine.
*-------------------------------------------------------------------------------
* Input / Output :
*----------------
* IN     :
* OUT    :
*-------------------------------------------------------------------------------
* Description: This is a .POST Subroutine
*
*-------------------------------------------------------------------------------
* Modification History
*-----------------------------------------------------------------------------------------------------------------
* Defect Reference       Modified By                    Date of Change        Change Details
*(RTC/TUT/PACS)                                        (YYYY-MM-DD)
*-----------------------------------------------------------------------------------------------------------------
* PACS00392015          Ashokkumar.V.P                  19/11/2014           Changes based on mapping.
*-----------------------------------------------------------------------------------------------------------------

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT TAM.BP I_F.REDO.H.REPORTS.PARAM
    $INSERT LAPAP.BP I_REDO.B.ABANDONADAS.COMMON
    $INSERT T24.BP I_F.DATES

    GOSUB PROCESS.PARA
    GOSUB PROCESS
    RETURN

PROCESS.PARA:
    REDO.H.REPORTS.PARAM.ID = "REDO.ABAN"
    Y.TODAY = R.DATES(EB.DAT.LAST.WORKING.DAY)
*
    FN.REDO.H.REPORTS.PARAM = 'F.REDO.H.REPORTS.PARAM'
    F.REDO.H.REPORTS.PARAM = ''
    CALL OPF(FN.REDO.H.REPORTS.PARAM,F.REDO.H.REPORTS.PARAM)

    FN.DR.REG.ABANDON.WORKFILE = 'F.DR.REG.ABANDON.WORKFILE'
    F.DR.REG.ABANDON.WORKFILE = ''
    CALL OPF(FN.DR.REG.ABANDON.WORKFILE,F.DR.REG.ABANDON.WORKFILE)
    R.REDO.H.REPORTS.PARAM = ''; REDO.PARAM.ERR = ''
    CALL CACHE.READ(FN.REDO.H.REPORTS.PARAM,REDO.H.REPORTS.PARAM.ID,R.REDO.H.REPORTS.PARAM,REDO.PARAM.ERR)
    IF R.REDO.H.REPORTS.PARAM THEN
        Y.OUT.FILE.NAME = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.OUT.FILE.NAME>
        FN.CHK.DIR = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.OUT.DIR>
    END
    F.CHK.DIR = ''
    CALL OPF(FN.CHK.DIR,F.CHK.DIR)

    EXTRACT.FILE.ID = Y.OUT.FILE.NAME:'.':Y.TODAY:'.txt'
    R.FIL = ''; READ.FIL.ERR = ''
    CALL F.READ(FN.CHK.DIR,EXTRACT.FILE.ID,R.FIL,F.CHK.DIR,READ.FIL.ERR)
    IF R.FIL THEN
        DELETE F.CHK.DIR,EXTRACT.FILE.ID
    END
    EXTRACT.ZEROFILE.ID = Y.OUT.FILE.NAME:'.ZERO.':Y.TODAY:'.txt'
    R.FIL = ''; READ.FIL.ERR = ''
    CALL F.READ(FN.CHK.DIR,EXTRACT.ZEROFILE.ID,R.FIL,F.CHK.DIR,READ.FIL.ERR)
    IF R.FIL THEN
        DELETE F.CHK.DIR,EXTRACT.ZEROFILE.ID
    END
    RETURN

PROCESS:
*-----------------------------------------------------------------------------------------------------------------
*Select REDO.REPORT.TEMP
*Frame Loop and Remove the id
*Read REDO.REPORT.TEMP If Record Exits then store it to an array
*Else Raise error C.22
*-----------------------------------------------------------------------------------------------------------------
    Y.SEQ.NO = 1
    SEL.CMD = "SSELECT ":FN.DR.REG.ABANDON.WORKFILE
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,RET.CODE)
    LOOP
        REMOVE Y.TEMP.ID FROM SEL.LIST SETTING TEMP.POS
    WHILE Y.TEMP.ID:TEMP.POS
        R.REDO.REPORT.TEMP = ''; TEMP.ERR = ''; YZERO.VAL = ''
        CALL F.READ(FN.DR.REG.ABANDON.WORKFILE,Y.TEMP.ID,R.REDO.REPORT.TEMP,F.DR.REG.ABANDON.WORKFILE,TEMP.ERR)
        YZERO.VAL = FIELD(Y.TEMP.ID,'-',3)
        IF R.REDO.REPORT.TEMP AND NOT(YZERO.VAL) THEN
            FINAL.ARRAY<-1> = R.REDO.REPORT.TEMP
        END
        IF R.REDO.REPORT.TEMP AND YZERO.VAL THEN
            FINAL.ARRAY.ZRO<-1> = R.REDO.REPORT.TEMP
        END
    REPEAT

    CHANGE FM TO CHARX(13):CHARX(10) IN FINAL.ARRAY
    WRITE FINAL.ARRAY ON F.CHK.DIR, EXTRACT.FILE.ID ON ERROR
        Y.ERR.MSG = "Unable to Write '":F.CHK.DIR:"'"
        GOSUB RAISE.ERR.C.22
    END
    CHANGE FM TO CHARX(13):CHARX(10) IN FINAL.ARRAY.ZRO
    WRITE FINAL.ARRAY.ZRO ON F.CHK.DIR, EXTRACT.ZEROFILE.ID ON ERROR
        Y.ERR.MSG = "Unable to Write '":F.CHK.DIR:"'"
        GOSUB RAISE.ERR.C.22
    END
    RETURN

*-----------------------------------------------------------------------------------------------------------------
RAISE.ERR.C.22:
*-----------------------------------------------------------------------------------------------------------------
*Handling error process
*-----------------------------------------------------------------------------------------------------------------
    MON.TP = "04"
    REC.CON = "ABAN.":Y.ERR.MSG
    DESC = "ABAN.":Y.ERR.MSG
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
*------------------------------------------------------------------Final End---------------------------------------------------------------------------
END
