SUBROUTINE L.APAP.POL.INSUREIN
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.AA.PAYMENT.SCHEDULE
    $INSERT I_F.USER
    $INSERT I_F.APAP.H.INSURANCE.DETAILS
    $INSERT I_F.REDO.INTERFACE.PARAM
    $INSERT I_F.REDO.H.REPORTS.PARAM

    GOSUB INITIALISE
    GOSUB LEER.USUARIOS
    GOSUB LEER.ARCHIVO.SAVELIST
RETURN
LEER.ARCHIVO.SAVELIST:
    POLIZA.ID = ID.NEW.LAST
    GOSUB GET.LEER.POLIZA
    AA.ID = Y.ID
    GOSUB GET.LEER.ARR
    CHANGE @VM TO @FM IN Y.PROPIEDADES
    CHANGE @SM TO @FM IN Y.PROPIEDADES
    IF Y.TIPO.MANEJO NE 'INCLUIR EN CUOTA' THEN
        RETURN
    END

    FINDSTR 'INSURANCE' IN Y.PAYMENT.TYPE2 SETTING App, Vpp THEN
        Y.COUNT =   DCOUNT(Y.CHARGE,@VM)
        FOR  I.VAR = 1  TO Y.COUNT
            Y.FECHAINICIO_PR = ''; Y.FECHAFINAL_PR = ''
            LOCATE Y.CHARGE<1,I.VAR> IN Y.SEGURO.FIELD<1> SETTING FPOS THEN
                LOCATE Y.CHARGE<1,I.VAR> IN Y.PROPIEDADES SETTING FTPOS THEN
                    CHANGE @VM TO @FM IN Y.FECHAINICIO_P
                    CHANGE @SM TO @FM IN Y.FECHAINICIO_P
                    CHANGE @VM TO @FM IN Y.FECHAFINAL_P
                    CHANGE @SM TO @FM IN Y.FECHAFINAL_P
                    FTPOS -= 1
                    Y.FECHAINICIO_PR = Y.FECHAINICIO_P<FTPOS,1>
                    Y.FECHAFINAL_PR = Y.FECHAFINAL_P<FTPOS,1>
                END
                IF Y.FECHAINICIO EQ Y.FECHAINICIO_PR AND Y.FECHAFINAL EQ Y.FECHAFINAL_PR THEN
                    GOSUB CONCATENAR.OFS
                    GOSUB PROCESS
                END
            END
        NEXT I.VAR
    END
RETURN

GET.LEER.ARR:
    R.AA = '' ; AA.ARR.ERR = ''
    CALL F.READ(FN.AA,AA.ID,R.AA,F.AA,AA.ARR.ERR)
    Y.CO.CODE = ''; Y.CO.CODE = R.AA<AA.ARR.CO.CODE>
    Y.TODAY = ''; Y.TODAY = TODAY
    CALL REDO.B.CON.LNS.BY.DEBTOR.AA.RECS(AA.ID,OUT.RECORD)
    R.AA.PAYMENT.SCHEDULE.APP = FIELD(OUT.RECORD,"*",3)
    Y.PAYMENT.TYPE = R.AA.PAYMENT.SCHEDULE.APP<AA.PS.PAYMENT.TYPE,1,1>
    Y.PAYMENT.METHOD  = R.AA.PAYMENT.SCHEDULE.APP<AA.PS.PAYMENT.METHOD,1,1>
    Y.PAYMENT.FREQ = R.AA.PAYMENT.SCHEDULE.APP<AA.PS.PAYMENT.FREQ,1,1>
    Y.PROPERTY = "ACCOUNT"
    Y.DUE.FREQ = R.AA.PAYMENT.SCHEDULE.APP<AA.PS.DUE.FREQ,1,1>
    Y.PROPERTY2 = "PRINCIPALINT"
    Y.DUE.FREQ2 = R.AA.PAYMENT.SCHEDULE.APP<AA.PS.DUE.FREQ,1,2>
    Y.END.DATE =R.AA.PAYMENT.SCHEDULE.APP<AA.PS.END.DATE,1,1>
    Y.AA.ID = AA.ID
    Y.PAYMENT.TYPE2 = R.AA.PAYMENT.SCHEDULE.APP<AA.PS.PAYMENT.TYPE>
    Y.PROPIEDADES = R.AA.PAYMENT.SCHEDULE.APP<AA.PS.PROPERTY>
    Y.FECHAINICIO_P = R.AA.PAYMENT.SCHEDULE.APP<AA.PS.START.DATE>
    Y.FECHAFINAL_P = R.AA.PAYMENT.SCHEDULE.APP<AA.PS.END.DATE>

RETURN

CONCATENAR.OFS:
    Y.OFS.QUEUE = ''
*Y.OFS.QUEUE = "AA.ARRANGEMENT.ACTIVITY,AA.PRESTAMO/I/PROCESS//0/,":Y.USUARIO.LOGIN:"/":Y.CONTRASENA:"/":Y.CO.CODE:",,ARRANGEMENT::=":Y.AA.ID:",ACTIVITY::=LENDING-CHANGE-REPAYMENT.SCHEDULE,EFFECTIVE.DATE:1:1=":Y.TODAY:",PROPERTY:1:1=REPAYMENT.SCHEDULE,FIELD.NAME:1:1=PAYMENT.TYPE:1:1,FIELD.NAME:1:2=PAYMENT.METHOD:1:1,FIELD.NAME:1:3=PAYMENT.FREQ:1:1,FIELD.NAME:1:4=PROPERTY:1:1,FIELD.NAME:1:5=DUE.FREQ:1:1,FIELD.NAME:1:6=PROPERTY:1:2,FIELD.NAME:1:7=DUE.FREQ:1:2,FIELD.NAME:1:8=END.DATE:1:1,FIELD.VALUE:1:1=":Y.PAYMENT.TYPE:",FIELD.VALUE:1:2=":Y.PAYMENT.METHOD:",FIELD.VALUE:1:3=":Y.PAYMENT.FREQ:",FIELD.VALUE:1:4=":Y.PROPERTY:",FIELD.VALUE:1:5=":Y.DUE.FREQ:",FIELD.VALUE:1:6=":Y.PROPERTY2:",FIELD.VALUE:1:7=":Y.DUE.FREQ2:",FIELD.VALUE:1:8=":Y.END.DATE:",FIELD.NAME:1:9=PAYMENT.TYPE:":FTPOS:":1,FIELD.VALUE:1:9=|-|,"
    Y.ACTIVITY = 'LENDING-CHANGE-REPAYMENT.SCHEDULE'
    Y.OFS.QUEUE = "AA.ARRANGEMENT.ACTIVITY,AA.PRESTAMO/I/PROCESS///,//":Y.CO.CODE:",,ARRANGEMENT::=":Y.AA.ID:",ACTIVITY::=LENDING-CHANGE-REPAYMENT.SCHEDULE,EFFECTIVE.DATE:1:1=":Y.TODAY:",PROPERTY:1:1=REPAYMENT.SCHEDULE,FIELD.NAME:1:1=PAYMENT.TYPE:1:1,FIELD.NAME:1:2=PAYMENT.METHOD:1:1,FIELD.NAME:1:3=PAYMENT.FREQ:1:1,FIELD.NAME:1:4=PROPERTY:1:1,FIELD.NAME:1:5=DUE.FREQ:1:1,FIELD.NAME:1:6=PROPERTY:1:2,FIELD.NAME:1:7=DUE.FREQ:1:2,FIELD.NAME:1:8=END.DATE:1:1,FIELD.VALUE:1:1=":Y.PAYMENT.TYPE:",FIELD.VALUE:1:2=":Y.PAYMENT.METHOD:",FIELD.VALUE:1:3=":Y.PAYMENT.FREQ:",FIELD.VALUE:1:4=":Y.PROPERTY:",FIELD.VALUE:1:5=":Y.DUE.FREQ:",FIELD.VALUE:1:6=":Y.PROPERTY2:",FIELD.VALUE:1:7=":Y.DUE.FREQ2:",FIELD.VALUE:1:8=":Y.END.DATE:",FIELD.NAME:1:9=PAYMENT.TYPE:":FTPOS:":1,FIELD.VALUE:1:9=|-|,"

RETURN
********************************************************************************
INITIALISE:

    options = ''; OFS.REQ = ''; theResponse = ''
    options<1> = 'TAG'
    FN.AA = 'F.AA.ARRANGEMENT'
    F.AA = ''
    CALL OPF(FN.AA,F.AA)
    FN.USER = 'F.USER'
    F.USER = ''

*Y.USER = OPERATOR
*OPERATOR = 'APOLIZASEGURO'
*CALL OPF(FN.USER,F.USER)
    FN.INSURANCE = 'F.APAP.H.INSURANCE.DETAILS'
    F.INSURANCE = ''
    CALL OPF(FN.INSURANCE,F.INSURANCE)
    FN.REDO.INTERFACE.PARAM = 'F.REDO.INTERFACE.PARAM'
    F.REDO.INTERFACE.PARAM = ''
    CALL OPF(FN.REDO.INTERFACE.PARAM,F.REDO.INTERFACE.PARAM)
    FN.REDO.H.REPORTS.PARAM = "F.REDO.H.REPORTS.PARAM" ; F.REDO.H.REPORTS.PARAM = ""
    CALL OPF(FN.REDO.H.REPORTS.PARAM,F.REDO.H.REPORTS.PARAM)
    Y.REPORT.PARAM.ID = 'INSURE'
    CALL CACHE.READ(FN.REDO.H.REPORTS.PARAM,Y.REPORT.PARAM.ID,R.REDO.H.REPORTS.PARAM,PARAM.ERR)
    Y.FIELD.NME.ARR = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.FIELD.NAME>
    Y.FIELD.VAL.ARR = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.FIELD.VALUE>
    LOCATE "SEGURO" IN Y.FIELD.NME.ARR<1,1> SETTING COD.POS THEN
        Y.SEGURO.FIELD = Y.FIELD.VAL.ARR<1,COD.POS>
        CHANGE @VM TO @FM IN Y.SEGURO.FIELD
        CHANGE @SM TO @FM IN Y.SEGURO.FIELD
    END

RETURN
********************************************************************************
LEER.USUARIOS:
    ID.PARAM = 'OFS.INSURE'
    CALL F.READ(FN.REDO.INTERFACE.PARAM,ID.PARAM,R.REDO.INTERFACE.PARAM,F.REDO.INTERFACE.PARAM,REDO.INTERFACE.PARAM.ERR)
    Y.USUARIO.LOGIN = R.REDO.INTERFACE.PARAM<REDO.INT.PARAM.FI.USER.SIGN.ON>
    Y.CONTRASENA = R.REDO.INTERFACE.PARAM<REDO.INT.PARAM.FI.USER.PWD>
RETURN
GET.LEER.POLIZA:
    R.INSURANCE = '' ; INSURANCE.ERR = ''
    CALL F.READ(FN.INSURANCE,POLIZA.ID,R.INSURANCE,F.INSURANCE,INSURANCE.ERR)
    Y.ID = R.INSURANCE<INS.DET.ASSOCIATED.LOAN>
    Y.CHARGE = R.INSURANCE<INS.DET.CHARGE>
    CHANGE @FM TO @VM IN Y.CHARGE
    CHANGE @SM TO @VM IN Y.CHARGE
    Y.FECHAINICIO = R.INSURANCE<INS.DET.INS.START.DATE,1>
    Y.FECHAFINAL = R.INSURANCE<INS.DET.INS.END.DATE,1>
    Y.TIPO.MANEJO = R.INSURANCE<INS.DET.MANAGEMENT.TYPE>

RETURN
PROCESS:
    CRT "Processing string = ":Y.AA.ID
    CALL OFS.CALL.BULK.MANAGER(options,Y.OFS.QUEUE,RESP,COMM)
    CRT "Response ":RESP
RETURN
END
