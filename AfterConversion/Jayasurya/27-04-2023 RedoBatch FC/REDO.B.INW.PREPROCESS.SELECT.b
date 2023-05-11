* @ValidationCode : Mjo4MTI4MTc3MjA6Q3AxMjUyOjE2ODI0ODU1NTQ3MTc6SVRTU0JORzotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 26 Apr 2023 10:35:54
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
SUBROUTINE REDO.B.INW.PREPROCESS.SELECT
*-----------------------------------------------------------------------------
* Description:
* This routine is a multithreaded routine to select the records in the mentioned applns
*------------------------------------------------------------------------------------------
* * Input / Output
*
* --------------
* IN     : -NA-
* OUT    : -NA-
*------------------------------------------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : ganesh r
* PROGRAM NAME : REDO.B.INW.PREPROCESS.SELECT
*------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
* DATE             WHO            REFERENCE         DESCRIPTION
* 21.09.2010  ganesh r           ODR-2010-09-0148   INITIAL CREATION
* Date                  who                   Reference
* 11-04-2023        �CONVERSTION TOOL   �  R22 AUTO CONVERSTION - VM TO @VM AND ++ TO += 1
* 11-04-2023          ANIL KUMAR B         R22 MANUAL CONVERSTION -CALL RTN METHOD ADDED
*
*------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.REDO.APAP.CLEARING.INWARD
    $INSERT I_F.REDO.MAPPING.TABLE
    $INSERT I_F.REDO.CLEARING.PROCESS
    $INSERT I_BATCH.FILES
    $INSERT I_REDO.B.INW.PREPROCESS.COMMON
*------------------------------------------------------------------------------------------

    GOSUB PROCESS
RETURN

PROCESS:
*------------------------------------------------------------------------------------------
    INT.CODE = ''
    ID.PROC = ''
    BAT.NO =''
    BAT.TOT =''
    INFO.OR =''
    INFO.DE =''
    ID.PROC = ''
    MON.TP =''
    DESC = ''
    REC.CON = ''
    EX.USER = ''
    EX.PC = ''

    VAR.APERTA.ID = TODAY:'.':VAR.FILE.NAME
    SEL.CMD = "SELECT ":VAR.FILE.PATH
    CALL EB.READLIST(SEL.CMD,FILE.LIST,'',NO.OF.REC,RET.ERR)
    IF NO.OF.REC GT 0 THEN
        Y.INIT = 1
        LOOP
            REMOVE VAR.APERTA.ID FROM FILE.LIST SETTING FILE.POS
        WHILE Y.INIT LE NO.OF.REC
            CALL F.READU(FN.APERTA,VAR.APERTA.ID,R.APERTA,F.APERTA,'',APERTA.ERR)
            Y.POST.ID = VAR.APERTA.ID
            IF R.APERTA THEN
                R.APERTA.LIST<-1> = R.APERTA
            END
            Y.INIT += 1
        REPEAT
        Y.CRLF = CHARX(013):CHARX(010)
        Y.CR = CHARX(013)
        Y.LF = CHARX(010)
        CHANGE Y.CRLF TO @FM IN R.APERTA.LIST          ;* Convert NewLine/Carriage Return Charecter to FM
        CHANGE Y.CR TO @FM IN R.APERTA.LIST
        CHANGE Y.LF TO @FM IN R.APERTA.LIST
        CHANGE @FM:'':@FM TO @FM IN R.APERTA.LIST
        CALL BATCH.BUILD.LIST('', R.APERTA.LIST)
    END
    ELSE
        INT.CODE ='APA002'
        INT.TYPE ='BATCH'
        BAT.NO =''
        BAT.TOT =''
        INFO.OR =''
        INFO.DE =''
        ID.PROC = VAR.APERTA.ID
        MON.TP  = '03'
        DESC    = 'NO RECORDS'
        REC.CON = ''
        EX.USER = ''
        EX.PC = ''
        CALL APAP.REDOCHNLS.REDO.INTERFACE.REC.ACT(INT.CODE,INT.TYPE,BAT.NO,BAT.TOT,INFO.OR,INFO.DE,ID.PROC,MON.TP,DESC,REC.CON,EX.USER,EX.PC) ;*MANUAL R22 CODE CONVERSION
    END
RETURN
END
