* @ValidationCode : MjotMTA3MzE2ODU2NzpDcDEyNTI6MTY4MTg5MDEzNjgwMjpzYW1hcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 19 Apr 2023 13:12:16
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : samar
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.USER.CHG.PASS.SENT.EMAIL

******************************************************************
******************************************************************
*Modification History
*DATE                       WHO                         REFERENCE                                   DESCRIPTION
*19-04-2023            Conversion Tool             R22 Auto Code conversion                         VM TO @VM,= TO EQ,<> TO NE, IF CONDITION ADDED,F.READ TO CACHE.READ,CHAR TO CHARX
*19-04-2023              Samaran T                R22 Manual Code conversion                         No Changes
*---------------------------------------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.EB.EXTERNAL.USER
    $INSERT I_System
    $INSERT I_F.CUSTOMER
    $INSERT I_F.REDO.CONFIG.EMAIL.AI
    $INSERT I_F.REDO.CONFIRM.PASSWORD

    GOSUB INIT
    GOSUB GET.EBEU.DET
    GOSUB GET.CUS.DET
    GOSUB GET.MAIL.DET
    GOSUB SEND.MAIL

RETURN

*************
GET.EBEU.DET:
*************
* OBTIENE LOS VALORES DEL USUARIO EXTERNO *

    ID.USER = System.getVariable("EXT.EXTERNAL.USER")
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN      ;*R22 AUTO CODE CONVERSION.START
        ID.USER = ""       ;*R22 AUTO CODE CONVERSION
    END    ;*R22 AUTO CODE CONVERSION.END
    PASS.TXT = R.NEW (LOC.CP.NEW.PASSWORD)

    R.EXT.USER = '' ; EXT.USER.ERR = ''
    CALL CACHE.READ(FN.EXT.USER, ID.USER, R.EXT.USER, EXT.USER.ERR)       ;*R22 AUTO CODE CONVERSION
    IF R.EXT.USER THEN
        ID.CUS = R.EXT.USER<EB.XU.CUSTOMER>
        USER.STATUS = R.EXT.USER<EB.XU.STATUS>
        Y.PROD = R.EXT.USER<EB.XU.LOCAL.REF><1,PROD.USED.POS>
        Y.EMAIL.REG = R.EXT.USER<EB.XU.LOCAL.REF><1,L.CORP.EMAIL.POS>
    END

    IF R.EXT.USER<EB.XU.CHANNEL> NE "INTERNET" THEN      ;*R22 AUTO CODE CONVERSION
        E = "EB-REDO.CHANN.INTERNET"
        RETURN
    END

RETURN

************
GET.CUS.DET:
************
* OBTIENE LOS VALORES DEL CLIENTE *

    IF Y.PROD EQ 'PERSONAL' THEN
        R.CUSTOMER = '' ; CUST.ERR = ''
        CALL F.READ(FN.CUST,ID.CUS,R.CUSTOMER,FV.CUST,CUST.ERR)
        IF R.CUSTOMER EQ "" THEN
            E = "EB-CUSTOMER.NOT.EXIST"
            RETURN
        END ELSE
            EMAIL.TO = R.CUSTOMER<EB.CUS.EMAIL.1>
        END
    END ELSE
        EMAIL.TO = Y.EMAIL.REG
    END

    IF EMAIL.TO EQ "" THEN
        E = "EB-REDO.NOT.MAIL"
        RETURN
    END

RETURN

*************
GET.MAIL.DET:
*************
* OBTIENE LOS VALORES DE ENVIO DEL CORREO *

    Y.REC.MSG = '1'
    CALL F.READ(FN.EMAIL,Y.REC.MSG,R.EMAIL,FV.EMAIL,F.ERR)
    IF R.EMAIL THEN
        EMAIL.SUBJECT = R.EMAIL<LOC.CE.SUBJECT>
        EMAIL.BODY.1 = R.EMAIL<LOC.CE.BODY>
        EMAIL.FROM = R.EMAIL<LOC.CE.MAILFROM>
    END

    IF EMAIL.BODY.1 NE "" THEN
        CRLF = CHARX(13):CHARX(10)         ;*R22 AUTO CODE CONVERSION
        EMAIL.BODY = CHANGE(EMAIL.BODY.1,"%%",PASS.TXT,1)
        CHANGE @VM TO CRLF IN EMAIL.BODY
    END

RETURN

**********
SEND.MAIL:
**********
* INVOKES CALLJ TO SEND EMAIL *

    param = EMAIL.FROM
    param = param : "NEXTPARAM" : EMAIL.SUBJECT
    param = param : "NEXTPARAM" : EMAIL.BODY
    param = param : "NEXTPARAM" : EMAIL.TO

    IF EMAIL.FROM NE "" AND EMAIL.SUBJECT NE "" AND EMAIL.BODY NE "" AND EMAIL.TO NE "" THEN        ;*R22 AUTO CODE CONVERSION
        CALLJ classNamePwd, methodName, param SETTING ret ON ERROR GOTO errHandler
    END

RETURN

***********
errHandler:
***********

    err = SYSTEM(0)

    BEGIN CASE
        CASE err EQ 1
            E = "EB-REDO.CALLJ.ERROR.1"
            RETURN
        CASE err EQ 2
            E = "EB-REDO.CALLJ.ERROR.2"
            RETURN
        CASE err EQ 3
            E = "EB-REDO.CALLJ.ERROR.3"
            RETURN
        CASE err EQ 4
            E = "EB-REDO.CALLJ.ERROR.4"
            RETURN
        CASE err EQ 5
            E = "EB-REDO.CALLJ.ERROR.5"
            RETURN
        CASE err EQ 6
            E = "EB-REDO.CALLJ.ERROR.6"
            RETURN
        CASE err EQ 7
            E = "EB-REDO.CALLJ.ERROR.7"
            RETURN
        CASE @TRUE
            E = "EB-REDO.CALLJ.ERROR.8"
            RETURN
    END CASE

RETURN

*****
INIT:
*****
* ABRE LAS TABLAS NECESARIAS *

    FN.CUST = 'F.CUSTOMER'
    FV.CUST = ''
    CALL OPF(FN.CUST,FV.CUST)

    FN.EXT.USER = 'F.EB.EXTERNAL.USER'
    FV.EXT.USER = ''
    CALL OPF(FN.EXT.USER,FV.EXT.USER)

    FN.EMAIL = 'F.REDO.CONFIG.EMAIL.AI'
    FV.EMAIL = ''
    CALL OPF(FN.EMAIL,FV.EMAIL)


* VALORES DE LA CLASE PARA EL CALLJ *

    classNamePwd = 'REDOEnvioCorreo'
    methodName = '$EnviarCorreo'

    LREF.APP = 'EB.EXTERNAL.USER'
    LREF.FIELDS = 'PROD.USED':@VM:'L.CORP.EMAIL'
    LREF.POS = ''
    CALL MULTI.GET.LOC.REF(LREF.APP,LREF.FIELDS,LREF.POS)
    PROD.USED.POS = LREF.POS<1,1>
    L.CORP.EMAIL.POS = LREF.POS<1,2>

RETURN

END
