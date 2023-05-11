* @ValidationCode : MjotMTk0MTE5MTA6Q3AxMjUyOjE2ODI1OTExNjYxNjQ6SVRTU0JORzotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 27 Apr 2023 15:56:06
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
SUBROUTINE REDO.BCR.REPORT.EXEC.PROCESS

*-----------------------------------------------------------------------------
*** RUN routine FOR REDO.BCR.REPORT.EXEC
* @author hpasquel@temenos.com
* @stereotype RUN
* @package infra.eb
* @description
*
* 2011-08-28 : PACS00060197 - C.22 Integration, PASS to BCR.REPORT.GEN service the ID of the current REDO.INTERFACE.PARAM to use
*              hpasquel@temenos.com
*-------------------------------------------------------------------------------------
*Modification
* Date                  who                   Reference
* 17-04-2023         CONVERSTION TOOL      R22 AUTO CONVERSTION - VM TO @VM AND ! TO *
* 17-04-2023          ANIL KUMAR B         R22 MANUAL CONVERSTION -NO CHANGES
*-------------------------------------------------------------------------------------
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.STANDARD.SELECTION
    $INSERT I_F.COMPANY
    $INSERT I_GTS.COMMON
    $INSERT I_F.TSA.SERVICE
*
    $INSERT I_F.REDO.BCR.REPORT.EXEC
    $USING APAP.TAM
*-----------------------------------------------------------------------------

    GOSUB INITIALISE
    GOSUB PROCESS

RETURN
*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------

    IF Y.PROCESS.CONTINUE EQ '1' AND R.NEW(REDO.BCR.REP.EXE.RUN.PROCESS) EQ 'SI' THEN
        yProcToExec = R.NEW(REDO.BCR.REP.EXE.PROC.TO.EXEC)
        BEGIN CASE
            CASE yProcToExec EQ 'GET'
                GOSUB GET.DATA
            CASE yProcToExec EQ 'GENERATE'
                GOSUB GENERATE.FILE
            CASE yProcToExec EQ 'SEND'
                GOSUB SEND.FILE
        END CASE
        R.NEW(REDO.BCR.REP.EXE.RUN.PROCESS) = 'NO'
    END
RETURN

*-----------------------------------------------------------------------------
INITIALISE:
*-----------------------------------------------------------------------------
* << TSA.SERVICE REDO.BCR.REPORT.GEN must be stopped to allow execute a new action
    FN.TSA.SERVICE = 'F.TSA.SERVICE'
    F.TSA.SERVICE  = ''
    CALL OPF(FN.TSA.SERVICE, F.TSA.SERVICE)

*CALL APAP.TAM.REDO.R.BCR.REPORT.CHECK.TSA(Y.RESPONSE) ;* MANUAL R22 CODE CONVERSION
    CALL APAP.TAM.redoRBcrReportCheckTsa(Y.RESPONSE) ;* MANUAL R22 CODE CONVERSION
    IF Y.RESPONSE EQ '1' THEN
        E = 'SERVICIO DE OBTENCION DE DATOS EN EJECUCION, ESPERE SU FINALIZACION'
    END
    IF E NE '' THEN   ;* May be, the record does not exist into TSA.SERVICE
        RETURN
    END
* >>

* List of Fields to copy
    fieldNamesToCopy = ''
    fieldNamesToCopy<-1> = 'NAME'
    fieldNamesToCopy<-1> = 'SEND.METHOD'
    fieldNamesToCopy<-1> = 'ENCRIPTATION'
    fieldNamesToCopy<-1> = 'ENCRIP.KEY'
    fieldNamesToCopy<-1> = 'ENCRIP.MET'
    fieldNamesToCopy<-1> = 'DIR.PATH'
    fieldNamesToCopy<-1> = 'FILE.NAME'
    fieldNamesToCopy<-1> = 'PARAM.TYPE'
    fieldNamesToCopy<-1> = 'PARAM.VALUE'

* Check if the process must be executed
    Y.PROCESS.CONTINUE = ""
    BEGIN CASE
* OFS.VAL.ONLY is equals to 1 when the user has press the button VALIDATION
        CASE OFS.VAL.ONLY EQ '1'
            Y.PROCESS.CONTINUE = '0'
        CASE V$FUNCTION MATCHES 'I' : @VM : 'C'
            Y.PROCESS.CONTINUE = '1'
        CASE 1
            Y.PROCESS.CONTINUE = '0'
    END CASE

RETURN
*-----------------------------------------------------------------------------
GET.DATA:
*-----------------------------------------------------------------------------
*
    Y.TSA.SERVICE.ID = R.COMPANY(EB.COM.MNEMONIC) : "^REDO.BCR.REPORT.GEN"
    Y.OFS.MESSAGE = "TSA.SERVICE,APAP,," : Y.TSA.SERVICE.ID : ",SERVICE.CONTROL=START,ATTRIBUTE.TYPE:1=REDO.PARAM.ID,ATTRIBUTE.VALUE:1=" : ID.NEW
    Y.OFS.ID  = ""
    Y.OPTIONS = ""
    CALL OFS.POST.MESSAGE(Y.OFS.MESSAGE, Y.OFS.ID, "BCR.PARAM", Y.OPTIONS)

RETURN

*-----------------------------------------------------------------------------
GENERATE.FILE:
*-----------------------------------------------------------------------------
* Convertir R.NEW en un tipo de registro R.REDO.INT.PARAM
* Invocar a REDO.R.BCR.REPORT.BUILD
    GOSUB GET.PARAM

    IF ID.NEW EQ 'BCR002' THEN
        Y.TSA.SERVICE.ID = R.COMPANY(EB.COM.MNEMONIC) : "^REDO.B.BCR.GENERATE.FILE.BCR2"
        Y.TSA.ID=R.COMPANY(EB.COM.MNEMONIC) : "/REDO.B.BCR.GENERATE.FILE.BCR2"


        R.TSA.SERVICE = ''
        Y.TSA.SERVICE.ID = Y.TSA.ID ;* Tus Start
        R.TSA.SERVICE.ERR = '';*Tus End

*    READ R.TSA.SERVICE FROM F.TSA.SERVICE, Y.TSA.SERVICE.ID ELSE ;*Tus Start
        CALL F.READ(FN.TSA.SERVICE,Y.TSA.SERVICE.ID,R.TSA.SERVICE,F.TSA.SERVICE,R.TSA.SERVICE.ERR)
        IF R.TSA.SERVICE.ERR THEN  ;* Tus End
            E = "REDO.BCR.RECORD.NOT.FOUND"
            E<2> = Y.TSA.SERVICE.ID : @VM : FN.TSA.SERVICE
            RETURN
        END

    END ELSE
        Y.TSA.SERVICE.ID = R.COMPANY(EB.COM.MNEMONIC) : "^REDO.B.BCR.GENERATE.FILE.BCR1"
        Y.TSA.ID=R.COMPANY(EB.COM.MNEMONIC) : "/REDO.B.BCR.GENERATE.FILE.BCR1"

        R.TSA.SERVICE = ''
        Y.TSA.SERVICE.ID = Y.TSA.ID ;* Tus Start
        R.TSA.SERVICE.ERR = '' ; * Tus End
*    READ R.TSA.SERVICE FROM F.TSA.SERVICE, Y.TSA.SERVICE.ID ELSE ;*Tus Start
        CALL F.READ(FN.TSA.SERVICE,Y.TSA.SERVICE.ID,R.TSA.SERVICE,F.TSA.SERVICE,R.TSA.SERVICE.ERR)
        IF R.TSA.SERVICE.ERR THEN  ;* Tus End
            E = "REDO.BCR.RECORD.NOT.FOUND"
            E<2> = Y.TSA.SERVICE.ID : @VM : FN.TSA.SERVICE
            RETURN
        END


    END

    IF R.TSA.SERVICE<TS.TSM.SERVICE.CONTROL> EQ 'START' THEN
        Y.RESPONSE = '1'
    END

    IF Y.RESPONSE EQ '1' THEN
        E = 'SERVICIO DE OBTENCION DE DATOS EN EJECUCION, ESPERE SU FINALIZACION'
    END
    IF E NE '' THEN   ;* May be, the record does not exist into TSA.SERVICE
        RETURN
    END


    CHANGE '/' TO '^' IN Y.TSA.SERVICE.ID
    Y.OFS.MESSAGE = "TSA.SERVICE,APAP,," : Y.TSA.SERVICE.ID : ",SERVICE.CONTROL=START,ATTRIBUTE.TYPE:1=REDO.PARAM.ID,ATTRIBUTE.VALUE:1=" : ID.NEW
    Y.OFS.ID  = ""
    Y.OPTIONS = ""
    CALL OFS.POST.MESSAGE(Y.OFS.MESSAGE, Y.OFS.ID, "BCR.PARAM", Y.OPTIONS)

*   RETURN

*    CALL REDO.R.BCR.REPORT.BUILD(ID.NEW,'ONLINE',R.REDO.INT.PARAM)
RETURN

*-----------------------------------------------------------------------------
SEND.FILE:
*-----------------------------------------------------------------------------
* Convertir R.NEW en un tipo de registro R.REDO.INT.PARAM
* Invocar a REDO.R.BCR.REPORT.DELIVERY
    GOSUB GET.PARAM

*    Y.TSA.SERVICE.ID = R.COMPANY(EB.COM.MNEMONIC) : "^REDO.B.BCR.GENERATE.FILE.BCR1"
*    Y.OFS.MESSAGE = "TSA.SERVICE,APAP,," : Y.TSA.SERVICE.ID : ",SERVICE.CONTROL=START,ATTRIBUTE.TYPE:1=REDO.PARAM.ID,ATTRIBUTE.VALUE:1=" : ID.NEW
*    Y.OFS.ID  = ""
*    Y.OPTIONS = ""
*    CALL OFS.POST.MESSAGE(Y.OFS.MESSAGE, Y.OFS.ID, "BCR.PARAM", Y.OPTIONS)

*CALL APAP.TAM.REDO.R.BCR.REPORT.DELIVERY(ID.NEW,'ONLINE',R.REDO.INT.PARAM) ;* MANUAL R22 CODE CONVERSION
    CALL APAP.TAM.redoRBcrReportDelivery(ID.NEW,'ONLINE',R.REDO.INT.PARAM) ;* MANUAL R22 CODE CONVERSION
RETURN

*-----------------------------------------------------------------------------


*-----------------------------------------------------------------------------
GET.PARAM:
*-----------------------------------------------------------------------------

    fieldName = ''
    R.REDO.INT.PARAM=''
    LOOP
        REMOVE fieldName FROM fieldNamesToCopy SETTING yPos
    WHILE fieldName : yPos
        fieldValue = ""
        fieldNoFrom    = 0
*CALL TAM.R.FIELD.NAME.TO.NUMBER(APPLICATION, fieldName, fieldNoFrom) ;*manual r22 code conversion
        CALL APAP.TAM.tamRFieldNameToNumber(APPLICATION, fieldName, fieldNoFrom) ;*manual r22 code conversion
        IF fieldNoFrom EQ 0 THEN
            E    = "ST-REDO.BCR.FIELD.NON.EXIST"
            E<2> = fieldName : @VM : "REDO.INTERFACE.PARAM"
            RETURN
        END
        fieldNoTo = 0
*CALL TAM.R.FIELD.NAME.TO.NUMBER("REDO.INTERFACE.PARAM", fieldName, fieldNoTo)  ;*manual r22 code conversion
        CALL APAP.TAM.tamRFieldNameToNumber("REDO.INTERFACE.PARAM", fieldName, fieldNoTo)  ;*manual r22 code conversion
        IF fieldNoTo EQ 0 THEN
            E    = "ST-REDO.BCR.FIELD.NON.EXIST"
            E<2> = fieldNoTo : @VM : APPLICATION
            RETURN
        END
        R.REDO.INT.PARAM<fieldNoTo> = R.NEW(fieldNoFrom)
    REPEAT
RETURN

*-----------------------------------------------------------------------------
END
