*-----------------------------------------------------------------------------
* <Rating>-89</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.V.VAL.CED.IDENT
*******************************************************************************************************************
*Company   Name    : Asociaciopular de Ahorros y Pramos Bank
*Developed By      : APAP
*Date              : 14.02.2020
*Program   Name    : LAPAP.V.VAL.CED.IDENT
*------------------------------------------------------------------------------------------------------------------

*Description       : Basada en la logica de la rutina de TEMENOS: REDO.V.VAL.CED.IDENT This subroutine validates the customers age and check whether the customer is major or a minor
*Linked With       : This routine has to be attached as an input routine to the versions of CUSTOMER,REDO.OPEN.PROSP.PF.TEST &
*                    CUSTOMER,REDO.OPEN.CL.MINOR.TEST & CUSTOMER,REDO.OPEN.CLIENTE.PF.TEST & CUSTOMER,REDO.MOD.CL.MINOR.TEST
*                    CUSTOMER,REDO.ACT.MOD.CL.PF.TEST

*In  Parameter     : -NA-
*Out Parameter     : -NA-
*-------------------------------------------------------------------------
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.CUSTOMER
    $INSERT T24.BP I_GTS.COMMON
    $INSERT T24.BP I_BROWSER.TAGS
    $INSERT TAM.BP I_REDO.V.VAL.CED.IDENT.COMMON
    $INSERT JBC.h
    GOSUB INIT
    GOSUB OPEN.FILES
    GOSUB CHECK.CIDENT.FORMAT

    IF NOT(OFS$BROWSER) THEN
        GOSUB PROCESS
    END ELSE
        IF OFS.VAL.ONLY EQ 1 AND MESSAGE EQ '' AND OFS$OPERATION EQ 'BUILD' AND OFS$HOT.FIELD EQ 'L.CU.CIDENT' THEN
            GOSUB PROCESS
        END
    END
    RETURN
*-------------------------------------------------------------------------------------------------------------------
INIT:
*****
    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    LOC.INTERFACE = ''
    FLAG.SET = 1  ; FLAG = ''
    LREF.FIELDS = 'L.CU.CIDENT':VM:'L.CU.TIPO.CL':VM:'L.CUSTOMER.IMG':VM:'L.CU.AGE':VM:'L.CU.NOUNICO':VM:'L.CU.ACTANAC':VM:'L.CU.DAT.END.CO'
    Y.LREF.POS = ''
    CALL MULTI.GET.LOC.REF('CUSTOMER',LREF.FIELDS,Y.LREF.POS)
    CIDENT.POS = Y.LREF.POS<1,1>
    TIPO.POS= Y.LREF.POS<1,2>
    REF.IMG= Y.LREF.POS<1,3>
    AGE.POS = Y.LREF.POS<1,4>
    NOUN.POS  = Y.LREF.POS<1,5>
    ACT.POS   = Y.LREF.POS<1,6>
    DAT.END = Y.LREF.POS<1,7>
    Y.TIPO.CL = R.NEW(EB.CUS.LOCAL.REF)<1,TIPO.POS>
    CIDENT.OLD = R.OLD(EB.CUS.LOCAL.REF)<1,CIDENT.POS>
    CIDENT.NEW = R.NEW(EB.CUS.LOCAL.REF)<1,CIDENT.POS>
    NOUN.NEW  = R.NEW(EB.CUS.LOCAL.REF)<1,NOUN.POS>
    ACT.NEW  = R.NEW(EB.CUS.LOCAL.REF)<1,ACT.POS>
    LEGAL.NEW = R.NEW(EB.CUS.LEGAL.ID)<1,1>
    VAR.CURR.NO  = R.NEW(EB.CUS.CURR.NO)
    RETURN
*-------------------------------------------------------------------------------------------------------------------
OPEN.FILES:
***********
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)
    RETURN
*-------------------------------------------------------------------------------------------------------------------
CHECK.CIDENT.FORMAT:
**********************
* The local reference field L.CU.CIDENT can take only 11 characters.If the length is less than 11 characters,
* then set the common variable, ETEXT to the error code, EB-INVALID.DOC.FORMAT.Else call subroutine LAPAP.S.CALC.DIGIT.PADRON
* If the returned value is FAIL, then set  ETEXT as 'EB-REDO.INCORRECT.CHECKDIGIT'
*---------------------------------------------------------------------------------------------------------------------

    Y.L.CU.CIDENT = COMI

    Y.LEN.L.CU.CIDENT = LEN(Y.L.CU.CIDENT)
    IF Y.L.CU.CIDENT NE '' THEN
        IF Y.LEN.L.CU.CIDENT NE 11 AND Y.LEN.L.CU.CIDENT THEN
            GOSUB CHECK.COUNT
        END
        IF Y.LEN.L.CU.CIDENT EQ 11 THEN
            CHECK.NUMERIC  = NUM(Y.L.CU.CIDENT)
            IF CHECK.NUMERIC EQ 1 THEN
                GOSUB CHECK.DIGIT
            END ELSE
                GOSUB CHECK.COUNT
            END
        END
    END
    RETURN
*-------------------------------------------------------------------------------------------------------------------
PROCESS:
*******
* This para is used to check the given value s available in PADRONE Interface
*----------------------------------------------------------------------------------------------------------------------
    IF Y.L.CU.CIDENT EQ '' AND OFS$BROWSER THEN
        Y.LOC.SPARE=''
        GOSUB CHECK.NAMES
        RETURN
    END
    IF Y.L.CU.CIDENT EQ 'PASS' THEN
        Cedule = "padrone$":COMI
        Param1 = "com.padrone.ws.util.MainClass"
        Param2 = "callPadrone"
        Param3 = Cedule
        Ret = ""
        ACTIVATION = "APAP_PADRONES_WEBSERVICES"
        INPUT_PARAM=Cedule
        ERROR.CODE = CALLJEE(ACTIVATION,INPUT_PARAM)
*PACS00157018 - S
        IF NOT(ERROR.CODE) THEN
            Ret=INPUT_PARAM
        END
*PACS00157018 - E
    END
    IF Y.L.CU.CIDENT EQ 'FAIL' THEN
        AF = EB.CUS.LOCAL.REF
        AV = CIDENT.POS
        ETEXT = 'EB-REDO.INCORRECT.CHECK.DIGIT'
        CALL STORE.END.ERROR
    END
    VAR.NAME = Ret
    INT.CODE = 'CIDENT'
    INT.TYPE = 'ONLINE'
    BAT.NO = ''
    BAT.TOT = ''
    INFO.OR = ''
    INFO.DE = ''
    ID.PROC = ''
    MON.TP = ''
    DESC = ''
    REC.CON = ''
    EX.USER = ''
    EX.PC = ''
    CHANGE '::' TO FM IN VAR.NAME
    REC.CON = VAR.NAME<2>
    DESC = VAR.NAME<3>
    IF Ret THEN
        GOSUB PADRONE.CHECK
        IF VAL.NAME<1> EQ 'FAILURE' THEN
            MON.TP = '04'
            CALL REDO.INTERFACE.REC.ACT(INT.CODE,INT.TYPE,BAT.NO,BAT.TOT,INFO.OR,INFO.DE,ID.PROC,MON.TP,DESC,REC.CON,EX.USER,EX.PC)
            IF COMI NE Y.LOC.SPARE THEN
                GOSUB CHECK.NAMES
            END
            Y.LOC.SPARE=COMI
            R.NEW(EB.CUS.LOCAL.REF)<1,REF.IMG>=IMAGENAME
            RETURN
        END

        IF FLAG EQ '1' THEN
* Changes for CR.008 - Format the date accordingly when it's retrieved from Web-Service/Database - Start
            FINDSTR "-" IN FECHANACIMIENTO SETTING POS1 THEN
                YEAR = FIELD(FECHANACIMIENTO,'-',1)
                MONTH = FIELD(FECHANACIMIENTO,'-',2)
                Y.DATE = FIELD(FECHANACIMIENTO,'-',3)
                Y.DATE = FIELD(Y.DATE,' ',1)
            END ELSE
                YEAR = FIELD(FECHANACIMIENTO,'/',3)
                MONTH = FIELD(FECHANACIMIENTO,'/',2)
                Y.DATE = FIELD(FECHANACIMIENTO,'/',1)
            END
* Changes for CR.008 - Format the date accordingly when it's retrieved from Web-Service/Database - End
            R.NEW(EB.CUS.DATE.OF.BIRTH)= YEAR:MONTH:Y.DATE
            R.NEW(EB.CUS.FAMILY.NAME)=APELLIDOS
            R.NEW(EB.CUS.GIVEN.NAMES)=NOMBRE
            R.NEW(EB.CUS.LOCAL.REF)<1,REF.IMG>=IMAGENAME
        END
        IF FLAG.SET EQ 1 THEN
            VAR.APELLIDOS = R.NEW(EB.CUS.FAMILY.NAME)
            VAR.NOMBRE = R.NEW(EB.CUS.GIVEN.NAMES)
            IF Y.TIPO.CL NE 'PERSONA JURIDICA' THEN
                R.NEW(EB.CUS.NAME.1)= VAR.NOMBRE:" ":VAR.APELLIDOS
            END
        END
    END
    RETURN
***************
PADRONE.CHECK:
****************
    CHANGE '$' TO '' IN VAR.NAME
    CHANGE '#' TO FM IN VAR.NAME
    VAL.NAME = VAR.NAME<1>
    CHANGE ':' TO FM IN VAL.NAME
    IF VAL.NAME<1> EQ 'SUCCESS' THEN
        IF VAL.TEXT EQ '' THEN
            APELLIDOS = VAR.NAME<2>
            FECHANACIMIENTO = VAR.NAME<3>
* NOMBRE = VAR.NAME<5>
            NOMBRE = VAR.NAME<4>
* IMAGEPATH=VAR.NAME<8>
            IMAGEPATH=VAR.NAME<5>
            SLASHC=DCOUNT(IMAGEPATH,'/')
            IMAGENAME=FIELD(IMAGEPATH,'/',SLASHC)
            C$SPARE(500) = "SUCCESS"
            FLAG = '1'
        END ELSE
            FLAG = '2'
        END
    END
    RETURN
************
CHECK.COUNT:
************
    AF = EB.CUS.LOCAL.REF
    AV = CIDENT.POS
    ETEXT = 'EB-REDO.INVALID.DOC.FORMAT'
    CALL STORE.END.ERROR
    RETURN
************
CHECK.DIGIT:
************
    VAR.CED=Y.L.CU.CIDENT
    CALL LAPAP.S.CALC.DIGIT.PADRON(Y.L.CU.CIDENT)
    RETURN
*************
CHECK.NAMES:
*************
    IF OFS$BROWSER THEN
        R.NEW(EB.CUS.DATE.OF.BIRTH)= ''
    END
    R.NEW(EB.CUS.FAMILY.NAME)= ''
    R.NEW(EB.CUS.GIVEN.NAMES)= ''
    R.NEW(EB.CUS.LOCAL.REF)<1,AGE.POS>= ''
    FLAG.SET = ''
    RETURN
*-------------------------------------------------------------------------
END
*------------------------------------------------------------------------
