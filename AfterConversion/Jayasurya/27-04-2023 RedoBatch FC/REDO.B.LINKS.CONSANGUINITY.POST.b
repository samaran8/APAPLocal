* @ValidationCode : MjotMjA3MzI5NjAwMjpDcDEyNTI6MTY4MjU3ODkwNjg2OTpJVFNTQk5HOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 27 Apr 2023 12:31:46
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
SUBROUTINE REDO.B.LINKS.CONSANGUINITY.POST
*----------------------------------------------------------------------------------------------------------------
*
* Description           : This routine reads TEMP.DIR and generates a flat file.
*
* Developed By          : Kalyani L K, Capgemini
*
* Development Reference : REGN6-GR05
*
* Attached To           : Batch - BNK/REDO.B.LINKS.CONSANGUINITY
*
* Attached As           : Post Routine
*-----------------------------------------------------------------------------------------------------------------
* Input Parameter:
*----------------*
* Argument#1 : NA
*
*-----------------*
* Output Parameter:
*-----------------*
* Argument#2 : NA
*
*-----------------------------------------------------------------------------------------------------------------
*  M O D I F I C A T I O N S
* ***************************
*-----------------------------------------------------------------------------------------------------------------
* Defect Reference       Modified By                    Date of Change        Change Details
* (RTC/TUT/PACS)                                        (YYYY-MM-DD)
*-----------------------------------------------------------------------------------------------------------------
* REGN6-GR05             Kalyani L K                     2014-02-18           Initial Draft
*                        Rashmitha M                     2014-03-19         Following is incorporated: First field
*                                                                           should contain seven characters. If the
*                                                                           number of characters is less than seven,
*                                                                           then "0" should be displayed as prefix.
* PACS00361957           Ashokkumar.V.P                  19/02/2015         Optimized the relation between the customer
* Date                   who                   Reference
* 11-04-2023         CONVERSTION TOOL     R22 AUTO CONVERSTION - VM TO @VM AND FM TO @FM AND ! TO *
* 11-04-2023          ANIL KUMAR B        R22 MANUAL CONVERSTION -CALL RTN METHOD ADDED
*-----------------------------------------------------------------------------------------------------------------
* Include files
*-----------------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.DATES
    $INSERT I_F.REDO.H.REPORTS.PARAM
    $INSERT I_REDO.B.LINKS.CONSANGUINITY.COMMON
    $USING APAP.REDOCHNLS
*-----------------------------------------------------------------------------------------------------------------
**********
MAIN.PARA:
**********
* This is the para of the program, from where the execution of the code starts
**
    GOSUB OPEN.PARA
    GOSUB PROCESS.PARA

RETURN
*-----------------------------------------------------------------------------------------------------------------
**********
OPEN.PARA:
**********
* In this para of the program, the files are opened
**
    FINAL.ARRAY = ''
    FN.REDO.H.REPORTS.PARAM = 'F.REDO.H.REPORTS.PARAM'
    F.REDO.H.REPORTS.PARAM  = ''
    CALL OPF(FN.REDO.H.REPORTS.PARAM,F.REDO.H.REPORTS.PARAM)

RETURN
*-----------------------------------------------------------------------------------------------------------------
*************
PROCESS.PARA:
*************
* In this para of the program, the main processing starts
**
    GOSUB GET.PARAM.VALUES
    CURR.MONTH = TODAY[5,2]
    CURR.MONTH = CURR.MONTH + 1 - 1

    LOCATE CURR.MONTH IN FIELD.GEN.VAL<1> SETTING FOUND.POS ELSE
        RETURN
    END
    GOSUB GET.REPORT.LINES
RETURN
*-----------------------------------------------------------------------------------------------------------------
*****************
GET.PARAM.VALUES:
*****************
* In this para of the program, the values from REDO.H.REPORTS.PARAM are fetched
**
    REDO.H.REPORTS.PARAM.ID = BATCH.DETAILS<3,1,1>
    GOSUB READ.REDO.H.REPORTS.PARAM

    FILE.NAME = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.OUT.FILE.NAME>
    OUT.DIR   = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.OUT.DIR>
    TEMP.PATH = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.TEMP.DIR>

    LOCATE 'REP.GEN.MONTH' IN R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.FIELD.NAME,1> SETTING GEN.FOUND.POS THEN
        FIELD.GEN.VAL = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.FIELD.VALUE,GEN.FOUND.POS>
        FIELD.GEN.VAL = RAISE(FIELD.GEN.VAL)
        FIELD.GEN.VAL = RAISE(FIELD.GEN.VAL)
    END

    CHANGE @VM TO '' IN OUT.DIR

    FINAL.OUT.FILE.NAME = FILE.NAME :'_': R.DATES(EB.DAT.LAST.WORKING.DAY) :'.csv'
    GOSUB OPEN.SEQ.FILE

RETURN
*-----------------------------------------------------------------------------------------------------------------
*****************
GET.REPORT.LINES:
*****************
* In this para of the program, the report lines are fecthed from TEMP directory
**
    OPEN TEMP.PATH TO TEMP.PTR ELSE

        INT.CODE = "REP001"
        INT.TYPE = "ONLINE"
        MON.TP   = 04
        REC.CON  = "GR05"
        DESC     = "GR05"
*CALL APAP.REDOCHNLS.REDO.INTERFACE.REC.ACT(INT.CODE,INT.TYPE,BAT.NO,BAT.TOT,INFO.OR,INFO.DE,ID.PROC,MON.TP,DESC,REC.CON,EX.USER,EX.PC) ;*MANUAL R22 CODE CONVERSION
        CALL APAP.REDOCHNLS.redoInterfaceRecAct(INT.CODE,INT.TYPE,BAT.NO,BAT.TOT,INFO.OR,INFO.DE,ID.PROC,MON.TP,DESC,REC.CON,EX.USER,EX.PC) ;*MANUAL R22 CODE CONVERSION
    END

*20140319 (S)
    SEQ.NO=1
*20140319 (E)
    SEL.CMD = "SELECT ":TEMP.PATH:" LIKE ":FILE.NAME:"..."
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,RET.CODE)

    IF NOT(SEL.LIST) THEN
        RETURN
    END

    LOOP
        REMOVE REC.ID FROM SEL.LIST SETTING REC.POS
    WHILE REC.ID:REC.POS
*20140319 (S)
        R.TEMP.PATH=''; ERR.MSG=''
        CALL F.READ(TEMP.PATH,REC.ID,R.TEMP.PATH,TEMP.PTR,ERR.MSG)
        IF ERR.MSG NE '' THEN
*20140319 (E)
            INT.CODE = "REP001"
            INT.TYPE = "ONLINE"
            MON.TP   = 04
            REC.CON  = "GR05"
            DESC     = "GR05"
*CALL APAP.REDOCHNLS.REDO.INTERFACE.REC.ACT(INT.CODE,INT.TYPE,BAT.NO,BAT.TOT,INFO.OR,INFO.DE,ID.PROC,MON.TP,DESC,REC.CON,EX.USER,EX.PC) ;*MANUAL R22 CODE CONVERSION
            CALL APAP.REDOCHNLS.redoInterfaceRecAct(INT.CODE,INT.TYPE,BAT.NO,BAT.TOT,INFO.OR,INFO.DE,ID.PROC,MON.TP,DESC,REC.CON,EX.USER,EX.PC) ;*MANUAL R22 CODE CONVERSION
        END

        REPORT.LINES = R.TEMP.PATH
*20140319 (S)
        CALL F.DELETE(TEMP.PATH,REC.ID)
        GOSUB APPEND.SEQ.NUM
*20140319 (E)

    REPEAT
RETURN
*-----------------------------------------------------------------------------------------------------------------
***************
APPEND.SEQ.NUM:
***************
* In this para of the program, the sequence number is added to thereport lines
**

    LOOP
        REMOVE REPORT.LINE FROM REPORT.LINES SETTING REP.POS
    WHILE REPORT.LINE : REP.POS
*20140319 (S)
        SEQ.NO = FMT(SEQ.NO,"R%7")
*20140319 (E)
        IF FINAL.ARRAY THEN
            FINAL.ARRAY = FINAL.ARRAY :@FM: SEQ.NO:REPORT.LINE
        END ELSE
            FINAL.ARRAY = SEQ.NO:REPORT.LINE
        END

        SEQ.NO += 1
    REPEAT

    IF FINAL.ARRAY THEN
        GOSUB WRITE.TO.FILE
    END

RETURN
*-----------------------------------------------------------------------------------------------------------------
**************************
READ.REDO.H.REPORTS.PARAM:
**************************
* In this para of the program, file REDO.H.REPORTS.PARAM is read
**
    R.REDO.H.REPORTS.PARAM  = ''
    REDO.H.REPORTS.PARAM.ER = ''
    CALL CACHE.READ(FN.REDO.H.REPORTS.PARAM,REDO.H.REPORTS.PARAM.ID,R.REDO.H.REPORTS.PARAM,REDO.H.REPORTS.PARAM.ER)

RETURN
*-----------------------------------------------------------------------------------------------------------------
**************
OPEN.SEQ.FILE:
**************
* In this para of the program, the out file is opened
**
    OPENSEQ OUT.DIR, FINAL.OUT.FILE.NAME TO OUT.PTR ELSE
        INT.CODE = "REP001"
        INT.TYPE = "ONLINE"
        MON.TP   = 04
        REC.CON  = "GR05"
        DESC     = "GR05"
*CALL APAP.REDOCHNLS.REDO.INTERFACE.REC.ACT(INT.CODE,INT.TYPE,BAT.NO,BAT.TOT,INFO.OR,INFO.DE,ID.PROC,MON.TP,DESC,REC.CON,EX.USER,EX.PC) ;*MANUAL R22 CODE CONVERSION
        CALL APAP.REDOCHNLS.redoInterfaceRecAct(INT.CODE,INT.TYPE,BAT.NO,BAT.TOT,INFO.OR,INFO.DE,ID.PROC,MON.TP,DESC,REC.CON,EX.USER,EX.PC) ;*MANUAL R22 CODE CONVERSION
    END

RETURN
*-----------------------------------------------------------------------------------------------------------------
**************
WRITE.TO.FILE:
**************
* In this para of the program, the final array is written to the file
**
    CHANGE @FM TO CHARX(13):CHARX(10) IN FINAL.ARRAY

    WRITESEQ FINAL.ARRAY APPEND TO OUT.PTR ELSE
        INT.CODE = "REP001"
        INT.TYPE = "ONLINE"
        MON.TP   = 04
        REC.CON  = "GR05"
        DESC     = "GR05"
*CALL APAP.REDOCHNLS.REDO.INTERFACE.REC.ACT(INT.CODE,INT.TYPE,BAT.NO,BAT.TOT,INFO.OR,INFO.DE,ID.PROC,MON.TP,DESC,REC.CON,EX.USER,EX.PC) ;*MANUAL R22 CODE CONVERSION
        CALL APAP.REDOCHNLS.redoInterfaceRecAct(INT.CODE,INT.TYPE,BAT.NO,BAT.TOT,INFO.OR,INFO.DE,ID.PROC,MON.TP,DESC,REC.CON,EX.USER,EX.PC) ;*MANUAL R22 CODE CONVERSION
    END

RETURN
*-----------------------------------------------------------------------------------------------------------------
END       ;*End of program
