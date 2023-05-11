* @ValidationCode : MjoxMzc5NDA4MTYxOkNwMTI1MjoxNjgyNTEyOTU1MzQwOklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 26 Apr 2023 18:12:35
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
SUBROUTINE REDO.B.CLEAR.RETURN.SELECT
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
* PROGRAM NAME : REDO.B.CLEAR.RETURN.SELECT
*------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
* DATE             WHO            REFERENCE         DESCRIPTION
* 21.09.2010  ganesh r           ODR-2010-09-0251   INITIAL CREATION
* Date                  who                   Reference
* 10-04-2023        �CONVERSTION TOOL   �  R22 AUTO CONVERSTION FM TO @FM
* 10-04-2023          ANIL KUMAR B         R22 MANUAL CONVERSTION -CALL RTN METHOD ADDED
*------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.REDO.MAPPING.TABLE
    $INSERT I_F.REDO.CLEARING.PROCESS
    $INSERT I_BATCH.FILES
    $INSERT I_REDO.B.CLEAR.RETURN.COMMON
    $USING APAP.REDOCHNLS
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
    TEMP.APERTA.ID = VAR.APERTA.ID
    SEL.CMD = "SELECT ":VAR.FILE.PATH
    CALL EB.READLIST(SEL.CMD,FILE.LIST,'',NO.OF.REC,RET.ERR)
    R.APERTA.FINAL = ''

    LOOP
        REMOVE VAR.APERTA.ID FROM FILE.LIST SETTING FILE.POS
    WHILE VAR.APERTA.ID:FILE.POS
        CALL F.READ(FN.APERTA,VAR.APERTA.ID,R.APERTA,F.APERTA,APERTA.ERR)
        IF R.APERTA THEN
            Y.CRLF = CHARX(013):CHARX(010)
            Y.CR = CHARX(013)
            Y.LF = CHARX(010)
            CHANGE Y.CRLF TO @FM IN R.APERTA   ;* Convert NewLine/Carriage Return Charecter to FM
            CHANGE Y.CR TO @FM IN R.APERTA
            CHANGE Y.LF TO @FM IN R.APERTA
            CHANGE @FM:'':@FM TO @FM IN R.APERTA

            CALL OCOMO("Appending the file in Select - ":VAR.APERTA.ID )
            R.APERTA.FINAL<-1> = R.APERTA
*CALL BATCH.BUILD.LIST('',R.APERTA)
        END
        ELSE
            INT.CODE ='APA005'
            INT.TYPE ='BATCH'
            BAT.NO =''
            BAT.TOT =''
            INFO.OR =''
            INFO.DE =''
            ID.PROC = VAR.APERTA.ID
            MON.TP ='03'
            DESC = APERTA.ERR
            REC.CON = ''
            EX.USER = ''
            EX.PC = ''
* CALL APAP.REDOCHNLS.REDO.INTERFACE.REC.ACT(INT.CODE,INT.TYPE,BAT.NO,BAT.TOT,INFO.OR,INFO.DE,ID.PROC,MON.TP,DESC,REC.CON,EX.USER,EX.PC) ;*MANUAL R22 CODE CONVERSION
            CALL APAP.REDOCHNLS.redoInterfaceRecAct(INT.CODE,INT.TYPE,BAT.NO,BAT.TOT,INFO.OR,INFO.DE,ID.PROC,MON.TP,DESC,REC.CON,EX.USER,EX.PC) ;*MANUAL R22 CODE CONVERSION
            CALL OCOMO("Log Interface has been called since file is empty")
        END
    REPEAT
    CHANGE @FM:'':@FM TO @FM IN R.APERTA.FINAL
    TOTAL.LINES = DCOUNT(R.APERTA.FINAL,@FM)

    CALL OCOMO("No of lines to be build for process - ": TOTAL.LINES)
    CALL BATCH.BUILD.LIST('',R.APERTA.FINAL)

RETURN

END
