* @ValidationCode : MjotMTc1ODI5OTE1MDpDcDEyNTI6MTY4MDc1ODk5MDk2MzpJVFNTQk5HOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 06 Apr 2023 10:59:50
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.REDOFCFI
SUBROUTINE REDO.FI.INT.PAYROLL.FILE(IN.TXT.MSG,OUT.ERR.MSG)
*******************************************************************************************************
*
*    Routine processing TXT PayRoll file from the External Associated Companies
*    Parameters:
*        IN.TXT.MSG:   Input parameter to recive the TXT message from the principal routine
*        OUT.ERR.MSG:  Output parameter to send the ERROR message get in the process
*
*=====================================================================================================
*
*    First Release : Ana Noriega
*    Developed for : APAP
*    Developed by  : Ana Noriega
*    Date          : 2010/Nov/08
*=====================================================================================================
* Modifications:
* 27/01/2012 - avelasco@temenos.com
*              APAP C18 :
*              Modifications
* 20/04/2012 - cherrera@temenos.com
*              APAP C18 Issues:
*                                BUG-C18-NOMINAS-20120201-1359, BUG-C18-NOMINAS-20120229-1942,
*                                BUG-C18-NOMINAS-20120302-1619, BUG-C18-NOMINAS-20120307-0938
*                                BUG-C18-NOMINAS-20120603-1627, BUG-C18-NOMINAS-20120603-1914,
*                                MANTIS0002302, MANTIS0002303
*              Modifications:
*               - @ID for REDO.FI.CONTROL record is calculated in REDO.FI.PAYROLL.VALIDATE
*               - Control for duplicate files
*               - Change corporate debit account using REDO.INTERFACE.PARAM which the same Company ID
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*04-04-2023            CONVERSION TOOL                AUTO R22 CODE CONVERSION           VM TO @VM ,FM TO @FM SM TO @SM and I++ to I=+1
*04-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES

*=====================================================================================================
*
    $INSERT I_COMMON
    $INSERT I_EQUATE
*
    $INSERT I_REDO.FI.VARIABLES.COMMON
    $INSERT I_F.REDO.FI.CONTROL
    $INSERT I_F.REDO.INTERFACE.PARAM
    $INSERT I_F.REDO.APAP.PARAM.EMAIL
    $INSERT I_F.REDO.ISSUE.EMAIL
    $INSERT I_F.REDO.TEMP.FI.CONTROL
    $INSERT I_F.REDO.NOMINA.DET
    $INSERT I_F.FT.COMMISSION.TYPE
    $INSERT I_F.FT.CHARGE.TYPE
    $INSERT I_F.FT.TXN.TYPE.CONDITION

*
*******************************************************************************************************
*

    GOSUB INITIALISE

    GOSUB CHECK.PRELIM.CONDITIONS

    IF PROCESS.GOAHEAD THEN
        GOSUB PROCESS
    END
*
RETURN
*
* ======
PROCESS:
* ======
*
*   Principal Process
*
*   Validate File

    CALL REDO.FI.PAYROLL.VALIDATE(IN.TXT.MSG,Y.VAR.WORK,Y.NEW.TXT.MSG,Y.ERR.MSG)
*   Message Error

    GOSUB CONTROL.MSG.ERROR

    FI.CTA.DESTINO  = Y.VAR.WORK<2>
    R.REDO.NOMINA.DET<RE.NM.DET.CTA.DESTINO>   = FI.CTA.DESTINO
    R.REDO.NOMINA.DET<RE.NM.DET.INTERFACE.TYPE>= 'INTNOMINA'
    R.REDO.NOMINA.DET<RE.NM.DET.INTERFACE.NAME>= FI.INTERFACE
    Y.TO.MAIL.VALUE = Y.VAR.WORK<3>

    GOSUB RECORD.FI.CONTROL
*   SaveFileToProcess
    IF Y.ERR.MSG EQ ''  THEN
        GOSUB SAVE.FILE.IN.DIR
        FI.W.REDO.FI.CONTROL<REDO.FI.CON.PROC.STATUS>       = Y.FINAL.STATUS
        CALL REDO.FI.RECORD.CONTROL(Y.ERR.MSG)
        CALL F.WRITE(FN.REDO.NOMINA.TEMP,FI.W.REDO.FI.CONTROL.ID,R.REDO.NOMINA.TEMP)
        CALL F.WRITE(FN.REDO.NOMINA.DET,FI.W.REDO.FI.CONTROL.ID,R.REDO.NOMINA.DET)
    END
    IF Y.ERR.MSG THEN
        GOSUB MAIL.GENERATION
    END
*
RETURN
*
* ---------------
MAIL.GENERATION:
* ---------------

    Y.TRANS.DATE = TODAY
    Y.TRANS.TIME= OCONV(TIME(), "MT")
    CHANGE ":" TO '' IN Y.TRANS.TIME
    Y.UNIQUE.ID = FI.W.REDO.FI.CONTROL.ID:"_":Y.TRANS.DATE:"_":Y.TRANS.TIME
    Y.REQUEST.FILE = Y.UNIQUE.ID:'.TXT'
    Y.ATTACH.FILENAME = 'ATTACHMENT':'_':Y.UNIQUE.ID:'.TXT'
    R.RECORD1 = ''
    Y.REF.FILE.NAME =  'INTERNAL NOMINA STATUS'
    CALL REDO.FI.MAIL.FORMAT.GEN(FI.W.REDO.FI.CONTROL.ID,Y.MAIL.DESCRIPTION)
    R.RECORD1 = Y.FROM.MAIL.ADD.VAL:"#":Y.TO.MAIL.VALUE:'#':Y.REF.FILE.NAME:'#':Y.REF.FILE.NAME
    IF Y.MAIL.MSG THEN
        WRITE Y.MAIL.DESCRIPTION TO F.HRMS.ATTACH.FILE,Y.ATTACH.FILENAME
    END ELSE
        WRITE IN.TXT.MSG TO F.HRMS.ATTACH.FILE,Y.ATTACH.FILENAME
    END
    WRITE R.RECORD1 TO F.HRMS.DET.FILE,Y.REQUEST.FILE

RETURN

* ----------------
RECORD.FI.CONTROL:
* ----------------
*
*   Total Records
    Y.COUNT.NEW.TXT = DCOUNT(Y.NEW.TXT.MSG,@FM)
* TAM - EC: cherrera
* The correct ID was generated in REDO.FI.PAYROLL.VALIDATE routine
    FI.W.REDO.FI.CONTROL.ID  = Y.VAR.WORK<1>

*   Redo.Fi.Control
    FI.W.REDO.FI.CONTROL<REDO.FI.CON.FILE.NAME>        = FI.FILE.ID
    FI.W.REDO.FI.CONTROL<REDO.FI.CON.FILE.DIR>         = FI.WORK.DIR
    FI.W.REDO.FI.CONTROL<REDO.FI.CON.PROCESS.CONFIRM>  = FI.REDO.INTERFACE.PARAM<REDO.INT.PARAM.FI.AUT.AVAIL.FUNDS>
    FI.W.REDO.FI.CONTROL<REDO.FI.CON.PROC.DATE>        = TODAY
    FI.W.REDO.FI.CONTROL<REDO.FI.CON.PROC.TIME>        = FIELD(TIMEDATE()," ",1)
    FI.W.REDO.FI.CONTROL<REDO.FI.CON.TOT.RECORD.FILE>  = FI.DATO.NUM.REG
    FI.W.REDO.FI.CONTROL<REDO.FI.CON.TOT.AMOUNT.FILE>  = FI.DATO.MONTO.TOTAL
    FI.W.REDO.FI.CONTROL<REDO.FI.CON.TOT.RECORD.CALC>  = FI.CALC.NUM.REG
    FI.W.REDO.FI.CONTROL<REDO.FI.CON.TOT.AMOUNT.CALC>  = FI.CALC.MONTO.TOTAL
    FI.W.REDO.FI.CONTROL<REDO.FI.CON.PROC.STATUS>      = "CARGA NO FALLIDA"
*   If there is a Msg Error set the status and description of the cause
    IF Y.ERR.MSG THEN
        FI.W.REDO.FI.CONTROL<REDO.FI.CON.PROC.STATUS>      = "CARGA FALLIDA"
        FI.W.REDO.FI.CONTROL<REDO.FI.CON.PROC.STAT.D>      = Y.ERR.MSG
        FI.W.REDO.FI.CONTROL<REDO.FI.CON.TOT.RECORDS.OK>   = 0
        FI.W.REDO.FI.CONTROL<REDO.FI.CON.AMOUNT.PROC.OK>   = 0
        FI.W.REDO.FI.CONTROL<REDO.FI.CON.TOT.RECORD.PROC>  = 0
        FI.W.REDO.FI.CONTROL<REDO.FI.CON.AMOUNT.PROC>      = 0
        FI.W.REDO.FI.CONTROL<REDO.FI.CON.TOT.RECORDS.FAIL> = 0
        FI.W.REDO.FI.CONTROL<REDO.FI.CON.AMOUNT.PROC.FAIL> = 0
    END
*   Save in Redo.Fi.Control
    CALL REDO.FI.RECORD.CONTROL(Y.ERR.MSG)

RETURN
*
* ---------------
SAVE.FILE.IN.DIR:
* ---------------
*
*   Open OutPut Directory
*

    W.TOT.FT.ERR  = 0
    CONTADO.EXTRA = Y.COUNT.NEW.TXT + 1
    W.TOL.FILE.AMT = FIELD(IN.TXT.MSG<1>,',',4)
* Debit to enterprise account

    R.PARAM<1>  = "NULL"
    R.PARAM<2>  = "N"
    R.PARAM<3>  = W.TOL.FILE.AMT
    R.PARAM<4>  = FI.FILE.ID
    R.PARAM<5>  = FI.INTERFACE
    R.PARAM<6>  = FI.CTA.DESTINO
    R.PARAM<7>  = W.TOL.FILE.AMT
    R.PARAM<8>  = "DOP"
    R.PARAM<9>  = FI.W.REDO.FI.CONTROL.ID
    R.PARAM<10> = "S"
    R.PARAM<11> = FI.CTA.INTERMEDIA
    R.PARAM<12> = "NOMINAINT"
    R.PARAM<13> = DR.TXN.CODE

    WRECORD.NUMBER        = CONTADO.EXTRA
    W.ADDITIONAL.INFO     = ""
    W.ADDITIONAL.INFO<1>  = FI.BATCH.ID : "." : WRECORD.NUMBER          ;* THEIR.REFERENCE
    W.ADDITIONAL.INFO<2>  = FI.BATCH.ID : "." : WRECORD.NUMBER          ;* OUR.REFERENCE
    W.ADDITIONAL.INFO<3>  = FI.INTERFACE : "." : FI.BATCH.ID  ;* NARRATIVE
    W.ADDITIONAL.INFO<4>  = FI.W.REDO.FI.CONTROL.ID ;* TRANS.REFERENCE
    W.ADDITIONAL.INFO<5>  = WRECORD.NUMBER          ;* BATCH.ID
    W.ADDITIONAL.INFO     = CHANGE(W.ADDITIONAL.INFO,@FM,",")


    Y.STT.REC        = ""
    Y.IN.MSG         = FI.CTA.DESTINO:",":FI.DATO.MONTO.TOTAL:",DOP,-"
    Y.NEW.TXT.MSG<CONTADO.EXTRA> = Y.IN.MSG:",":W.ADDITIONAL.INFO
    CALL REDO.FI.MSG.FORMAT(FI.INTERFACE,Y.NEW.TXT.MSG<CONTADO.EXTRA>,DATO.OUT)

    IF W.TOL.FILE.AMT GT 0 THEN
        GOSUB FT.PROCESS
        FI.W.REDO.FI.CONTROL<REDO.FI.CON.PARENT.FT.REF> = OUT.RESP

        IF FIELD(OUT.ERR,'/',1)[1,2] NE 'FT' THEN

            Y.ERR.MSG = OUT.ERR
            Y.STT.REC = "FALLIDO"
            Y.FINAL.STATUS = "FALLIDO"
            FI.W.REDO.FI.CONTROL<REDO.FI.CON.PROC.STAT.D> =  OUT.ERR<1>
            GOSUB CONTROL.MSG.ERROR
            RETURN
        END ELSE
            Y.STT.REC = "PROCESADO"
            Y.FINAL.STATUS = "PROCESADO"
        END


    END ELSE
*        E = "EB-ERROR.NONE.FT.APPLIED"
        Y.ERR.MSG = "EB-ERROR.NONE.FT.APPLIED"
        Y.STT.REC = "FALLIDO"
        Y.FINAL.STATUS = "FALLIDO"
        FI.W.REDO.FI.CONTROL<REDO.FI.CON.PROC.STAT.D>  = Y.ERR.MSG
        W.TOT.FT.ERR = 0
        W.TOT.FT.ERR += FI.DATO.MONTO.TOTAL
        GOSUB CONTROL.MSG.ERROR
        RETURN
    END

    I.VAR = CONTADO.EXTRA
*    GOSUB REPORT.FORMAT

* Credit for customer account

    FOR I.VAR = 1 TO Y.COUNT.NEW.TXT
        OUT.RESP = ''
        OUT.RESP<1> = ''

        GOSUB APPEND.ADDITIONAL.INFO
        GOSUB MESSAGE.FORMAT
    NEXT
RETURN

* ---------------------
APPEND.ADDITIONAL.INFO:
* ---------------------
*
* Paragraph that add data to send to the FT
*

    WRECORD.NUMBER        = I.VAR
    W.ADDITIONAL.INFO     = ""
    W.ADDITIONAL.INFO<1>  = FI.BATCH.ID : "." : WRECORD.NUMBER          ;* THEIR.REFERENCE
    W.ADDITIONAL.INFO<2>  = FI.BATCH.ID : "." : WRECORD.NUMBER          ;* OUR.REFERENCE
    W.ADDITIONAL.INFO<3>  = FI.INTERFACE : "." : FI.BATCH.ID  ;* NARRATIVE
    W.ADDITIONAL.INFO<4>  = FI.W.REDO.FI.CONTROL.ID ;* TRANS.REFERENCE
    W.ADDITIONAL.INFO<5>  = WRECORD.NUMBER          ;* BATCH.ID
    W.ADDITIONAL.INFO     = CHANGE(W.ADDITIONAL.INFO,@FM,",")
*
RETURN
*
* =============
MESSAGE.FORMAT:
* =============
*
*   Paragraph to set message format to AC.ENTRY.PARAM
*

    Y.STT.REC        = ""
    Y.IN.MSG         = Y.NEW.TXT.MSG<I.VAR>
    Y.NEW.TXT.MSG<I.VAR> = Y.NEW.TXT.MSG<I.VAR>:",":W.ADDITIONAL.INFO
    CALL REDO.FI.MSG.FORMAT(FI.INTERFACE,Y.NEW.TXT.MSG<I.VAR>,DATO.OUT)
    R.PARAM<1>  = "NULL"
    R.PARAM<2>  = "N"
    R.PARAM<3>  = FIELD(DATO.OUT,"|",3)
    R.PARAM<4>  = FI.FILE.ID
    R.PARAM<5>  = FI.INTERFACE
    R.PARAM<6>  = FI.CTA.INTERMEDIA
    R.PARAM<7>  = FIELD(DATO.OUT,"|",3)
    R.PARAM<8>  = FIELD(DATO.OUT,"|",4)
    R.PARAM<9>  = FI.W.REDO.FI.CONTROL.ID
    R.PARAM<10> = "N"
    R.PARAM<11> = FIELD(DATO.OUT,"|",2)
    R.PARAM<12> = "NOMINA"
    R.PARAM<13> = CR.TXN.CODE

*    GOSUB FT.PROCESS

    Y.TEMP.ID=TODAY:FI.FILE.ID:STR("0",(5-LEN(I.VAR))):I.VAR
    R.REDO.NOMINA.TEMP<-1>=Y.TEMP.ID
    CHANGE @FM TO @VM IN R.PARAM
    R.REDO.TEMP.FI.CONTROL<FI.TEMP.PARAM.VAL> =R.PARAM
    R.REDO.TEMP.FI.CONTROL<FI.TEMP.STATUS>    =''
    R.REDO.TEMP.FI.CONTROL<FI.TEMP.MAIL.MSG>  =IN.TXT.MSG<I.VAR>
    R.REDO.TEMP.FI.CONTROL<FI.TEMP.INTER.TYPE>='INTNOMINA'
    CALL F.WRITE(FN.REDO.TEMP.FI.CONTROL,Y.TEMP.ID,R.REDO.TEMP.FI.CONTROL)

*
RETURN
*
* ============
REPORT.FORMAT:
* ============
*
*   Paragraph to set report message to REDO.FI.E.REPORT
*   and save log in REDO.INTERFACE.REC.ACT
*
*
    IF Y.ERR.MSG NE "" THEN
        W.STATUS = "04"
        W.ERROR.MSG = Y.ERR.MSG
        Y.MAIL.MSG<-1> = IN.TXT.MSG<I.VAR+1>:'-':'FAILURE'
    END ELSE
        W.STATUS = "01"
        W.ERROR.MSG = "OK"
        W.TOTAL.TRANSACC += R.PARAM<3>
        Y.MAIL.MSG<-1> = IN.TXT.MSG<I.VAR+1>:'-':'SUCCESS'
    END

    Y.ID.RCL  = FI.INTERFACE:".R"         ;*The id is NOMINA.R
    Y.TOT.REC = Y.COUNT - 1
    Y.IN.MSG  = WRECORD.NUMBER:",":Y.IN.MSG:",":W.STATUS:",":Y.ERR.MSG
*
*  Call RCL to set data to send report
*   CALL REDO.FI.MSG.FORMAT(Y.ID.RCL,Y.IN.MSG,Y.OUT.MSG)
*  Save record in log tabla REDO.INTERFACE.REC.ACT
    FI.INTERFACES = FI.INTERFACE:".INT"
    FI.INT.ACT.ID = "INM001"


* Y.OUT.MSG = Y.OUT.MSG :"@":W.STATUS:"@":W.ERROR.MSG:"@":OUT.RESP<1>
    Y.OUT.MSG = Y.OUT.MSG :"@":W.ERROR.MSG:"@":OUT.RESP<1>

    IF Y.ERR.MSG NE "" THEN
*    CALL REDO.INTERFACE.REC.ACT(FI.INT.ACT.ID,"BATCH",WRECORD.NUMBER,Y.TOT.REC,FI.W.REDO.FI.CONTROL.ID,FI.FILE.ID,"","04",Y.ERR.MSG,Y.OUT.MSG,"","")
        IF R.PARAM<10> EQ "N" THEN
            W.REC.FT.ERR += 1
            W.TOT.FT.ERR +=R.PARAM<7>
        END
    END ELSE
*    CALL REDO.INTERFACE.REC.ACT(FI.INT.ACT.ID,"BATCH",WRECORD.NUMBER,Y.TOT.REC,FI.W.REDO.FI.CONTROL.ID,FI.FILE.ID,WRECORD.NUMBER,"01","OK",Y.OUT.MSG,"","")
        IF R.PARAM<10> EQ "N" THEN
            W.REC.FT.OK += 1
            W.TOT.FT.OK += R.PARAM<7>
            FI.DATO.MONTO.TOTAL = W.TOT.FT.OK
        END
    END
*
RETURN
*
* ----------------
CONTROL.MSG.ERROR:
* ----------------
*
*   Paragraph that record the error message and move the file to the rejected directory renaming the file
*
    IF Y.ERR.MSG THEN
        ETEXT            = Y.ERR.MSG
        OUT.ERR.MSG      = Y.ERR.MSG
        Y.NAME.DIR       = FI.PATH.REJ
    END
*
    IF ETEXT THEN
        PROCESS.GOAHEAD = 0
        OUT.ERR.MSG     = ""
        CALL STORE.END.ERROR
        OUT.ERR.MSG     = ETEXT
        ETEXT           = ""
    END
*

    IF OUT.ERR.MSG NE "" THEN
        Y.FILE.ID = CHANGE(FI.FILE.ID,".txt",".rej")
        OPENSEQ FI.PATH.REJ, Y.FILE.ID TO Y.SEQ.FILE.POINTER ELSE
            CREATE Y.SEQ.FILE.POINTER ELSE
                OUT.ERR.MSG = "Error.Opening.Output.File"
            END
        END

        OPENSEQ FI.PATH.REJ, Y.FILE.ID TO Y.SEQ.FILE.POINTER ELSE
            CREATE Y.SEQ.FILE.POINTER ELSE
                OUT.ERR.MSG = "Error.Opening.Output.File"
            END
        END

        WRITESEQ IN.TXT.MSG TO Y.SEQ.FILE.POINTER ELSE
            OUT.ERR.MSG = "Error.Writing.To.Output.File"
        END
        WEOFSEQ   Y.SEQ.FILE.POINTER        ;* Writes an EOF
        CLOSESEQ  Y.SEQ.FILE.POINTER
    END
*
RETURN
*
* ----------------
FT.PROCESS:
* ----------------
*
* Paragraph that record the error message and move the file to the rejected directory renaming the file
*

    CALL REDO.FI.EXT.DEBIT.PROCES(R.PARAM, OUT.RESP, OUT.ERR)

    IF FIELD(OUT.ERR,'/',1)[1,2] NE 'FT' THEN
        Y.ERR.MSG = OUT.ERR
        Y.STT.REC = "FALLIDO"
    END ELSE
        Y.ERR.MSG = ''
        Y.STT.REC = "PROCESADO"
    END


RETURN
*
* ==============
TXN.DETAILS.UPD:
* ==============

    CALL REDO.FI.RECORD.TXN.DETAILS(R.PARAM,OUT.ERR,OUT.RESP,OUT.ARRAY)

RETURN

* ---------
INITIALISE:
* ---------
*   General Variables
    FI.W.REDO.FI.CONTROL = ''
    PROCESS.GOAHEAD      = 1
    LOOP.CNT             = 1
    MAX.LOOPS            = 1
*   MessageTXT
    Y.SEPARATOR.REC      = CHARX(13):CHARX(10)        ;*CHAR(13) is CR, CHAR(13) is LF ;*AUTO R22 CODE CONVERSION CHAR TO CHARX
    IN.TXT.MSG           = CHANGE(IN.TXT.MSG,Y.SEPARATOR.REC,@FM)
    Y.COUNT              = COUNT(IN.TXT.MSG,@FM)
    Y.COUNT.NEW.TXT      = 0
*   Parameters to send
    Y.VAR.WORK           = ""
    Y.NEW.TXT.MSG        = ""
*   Save.File.In.Dir
    Y.ERR.MSG            = ""
    Y.FILE.ID            = ""
    Y.SEQ.FILE.POINTER   = ""
    DATO.OUT             = ""
    R.PARAM              = ""
*
    Y.IN.MSG             = ""
    Y.OUT.MSG            = ""
    Y.STT.REC            = "PROCESADO"
    W.STATUS             = "01"
*
    FI.INTERFACES        = ""
    OUT.ERR.MSG          = ""
*
    FN.REDO.INTERFACE.PARAM = "F.REDO.INTERFACE.PARAM"
    F.REDO.INTERFACE.PARAM = ""
*
    FN.REDO.APAP.PARAM.EMAIL = "F.REDO.APAP.PARAM.EMAIL"
    F.REDO.APAP.PARAM.EMAIL  = ""

    CALL CACHE.READ(FN.REDO.APAP.PARAM.EMAIL,'SYSTEM',R.EMAIL,MAIL.ERR)
    Y.FILE.PATH   = R.EMAIL<REDO.PRM.MAIL.IN.PATH.MAIL>
    Y.ATTACH.PATH = R.EMAIL<REDO.PRM.MAIL.ATTACH.PATH.MAIL>

    FN.HRMS.DET.FILE        = Y.FILE.PATH
    F.HRMS.DET.FILE         = ""
    CALL OPF(FN.HRMS.DET.FILE,F.HRMS.DET.FILE)

    FN.HRMS.ATTACH.FILE        = Y.ATTACH.PATH
    F.HRMS.ATTACH.FILE         = ""
    CALL OPF(FN.HRMS.ATTACH.FILE,F.HRMS.ATTACH.FILE)

    FN.REDO.ISSUE.MAIL = 'F.REDO.ISSUE.EMAIL'
    F.REDO.ISSUE.MAIL = ''
    R.REDO.ISSUE.MAIL = ''
    Y.ISSUE.EMAIL.ERR = ''
    CALL OPF(FN.REDO.ISSUE.MAIL,F.REDO.ISSUE.MAIL)
*  CALL F.READ(FN.REDO.ISSUE.MAIL,'SYSTEM',R.REDO.ISSUE.MAIL,F.REDO.ISSUE.MAIL,Y.ISSUE.EMAIL.ERR) ;*Tus Start
    CALL CACHE.READ(FN.REDO.ISSUE.MAIL,'SYSTEM',R.REDO.ISSUE.MAIL,Y.ISSUE.EMAIL.ERR) ; * Tus End
    IF R.REDO.ISSUE.MAIL THEN
        Y.FROM.MAIL.ADD.VAL =  R.REDO.ISSUE.MAIL<ISS.ML.MAIL.ID>
    END

    FN.FT.TXN.TYPE.CONDITION = 'F.FT.TXN.TYPE.CONDITION'
    F.FT.TXN.TYPE.CONDITION = ''
    R.FT.TXN.TYPE.CONDITION = ''
    CALL OPF(FN.FT.TXN.TYPE.CONDITION,F.FT.TXN.TYPE.CONDITION)

    FN.FT.COMMISSION.TYPE = 'F.FT.COMMISSION.TYPE'
    F.FT.COMMISSION.TYPE  = ''
    R.FT.COMMISSION.TYPE  = ''
    CALL OPF(FN.FT.COMMISSION.TYPE,F.FT.COMMISSION.TYPE)

    FN.FT.CHARGE.TYPE = 'F.FT.CHARGE.TYPE'
    F.FT.CHARGE.TYPE  = ''
    R.FT.CHARGE.TYPE  = ''
    CALL OPF(FN.FT.CHARGE.TYPE,F.FT.CHARGE.TYPE)

    CALL OPF(FN.REDO.INTERFACE.PARAM,F.REDO.INTERFACE.PARAM)
    CALL CACHE.READ(FN.REDO.INTERFACE.PARAM, FI.INTERFACE, R.REDO.INTERFACE.PARAM, Y.ERR)
    IF Y.ERR THEN
        PROCESS.GOAHEAD = 0
        E = "EB-PARAMETER.MISSING"
        CALL ERR
    END

    RIP.PARAM        = R.REDO.INTERFACE.PARAM<REDO.INT.PARAM.PARAM.TYPE>
    RIP.VALUE        = R.REDO.INTERFACE.PARAM<REDO.INT.PARAM.PARAM.VALUE>
    DR.TXN.CODE     = R.REDO.INTERFACE.PARAM<REDO.INT.PARAM.DR.TXN.CODE>
    CR.TXN.CODE    = R.REDO.INTERFACE.PARAM<REDO.INT.PARAM.CR.TXN.CODE>
    RET.TXN.CODE    = R.REDO.INTERFACE.PARAM<REDO.INT.PARAM.RET.TXN.CODE>
    RET.TAX.CODE   = R.REDO.INTERFACE.PARAM<REDO.INT.PARAM.RET.TAX.CODE>


    CALL CACHE.READ(FN.FT.TXN.TYPE.CONDITION, DR.TXN.CODE, R.FT.TXN.TYPE.CONDITION, FT.TXN.TYPE.CONDITION.ERR) ;*AUTO R22 CODE CONVERSION
    Y.PAYROLL.COMM.TYPE       = R.FT.TXN.TYPE.CONDITION<FT6.COMM.TYPES>
    Y.PAYROLL.CHG.TYPE  = R.FT.TXN.TYPE.CONDITION<FT6.CHARGE.TYPES>
    IF Y.PAYROLL.CHG.TYPE THEN
        Y.PAYROLL.CHG.TYPE<2,-1> = 'CHG'
    END

    IF Y.PAYROLL.COMM.TYPE THEN
        CALL CACHE.READ(FN.FT.COMMISSION.TYPE, Y.PAYROLL.COMM.TYPE, R.FT.COMMISSION.TYPE, FT.COMMISSION.TYPE.ERR) ;*AUTO R22 CODE CONVERSION
        Y.PAY.COM.FLAT.AMT = R.FT.COMMISSION.TYPE<FT4.FLAT.AMT,1>
        Y.PAY.COM.PERCENT = R.FT.COMMISSION.TYPE<FT4.PERCENTAGE,1,1>
        Y.PAY.CATEG.ACCOUNT = R.FT.COMMISSION.TYPE<FT4.CATEGORY.ACCOUNT>
    END


    CTA.INTERMEDIA = "CTA.INTERMEDIA"
*    CTA.DESTINO = "CTA.DESTINO"
* The bussiness account for debits is token from the concern record in REDO.INTERFACE.PARAM wich use the same ID as the company
    WPARAM.POS = 1
    LOCATE CTA.INTERMEDIA IN RIP.PARAM<1,WPARAM.POS> SETTING PARAM.POS THEN
        FI.CTA.INTERMEDIA = RIP.VALUE<1,PARAM.POS>
        WPARAM.POS   = PARAM.POS + 1
    END ELSE
        WERROR.MSG = "&.Intermediate.Account.not.defined.in.&":@FM:CTA.INTERMEDIA
    END


    FN.REDO.NOMINA.TEMP='F.REDO.NOMINA.TEMP'
    F.REDO.NOMINA.TEMP =''
    CALL OPF(FN.REDO.NOMINA.TEMP,F.REDO.NOMINA.TEMP)

    FN.REDO.NOMINA.DET ='F.REDO.NOMINA.DET'
    F.REDO.NOMINA.DET  =''
    CALL OPF(FN.REDO.NOMINA.DET,F.REDO.NOMINA.DET)

    FN.REDO.TEMP.FI.CONTROL='F.REDO.TEMP.FI.CONTROL'
    F.REDO.TEMP.FI.CONTROL =''
    CALL OPF(FN.REDO.TEMP.FI.CONTROL,F.REDO.TEMP.FI.CONTROL)

RETURN
*-----------------------
CHECK.PRELIM.CONDITIONS:
*-----------------------
*
    LOOP.CNT  = 1   ;   MAX.LOOPS = 1
*
    LOOP
    WHILE LOOP.CNT LE MAX.LOOPS AND PROCESS.GOAHEAD DO
        BEGIN CASE
            CASE LOOP.CNT EQ 1        ;*    Validate TXT file exists
                IF IN.TXT.MSG EQ "" THEN
                    Y.ERR.MSG =  "File.Is.Empty"
                END
        END CASE
*Message Error
        GOSUB CONTROL.MSG.ERROR
*Increase
        LOOP.CNT +=1
    REPEAT
*
RETURN

END
