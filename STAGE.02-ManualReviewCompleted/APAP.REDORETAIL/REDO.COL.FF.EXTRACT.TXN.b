* @ValidationCode : MjotMTE5MzM0MDc2MTpDcDEyNTI6MTY4MTg5MDM0NDI5NTpJVFNTOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 19 Apr 2023 13:15:44
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDORETAIL
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*12-04-2023            CONVERSION TOOL                AUTO R22 CODE CONVERSION           VM TO @VM ,FM TO @FM and I++ to I=+1
*12-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
SUBROUTINE REDO.COL.FF.EXTRACT.TXN(Y.PROCESS.DATE, Y.AA.ID, R.STATIC.MAPPING, Y.ACCOUNT.ID, Y.PRODUCT.ID, Y.AGENCY.CODE, Y.CREDIT.TXN)
*******************************************************************************
*
*    REDO COLLECTOR EXTRACT AA TRANSACTION
*    Used from REDO.COL.EXTRACT Service. Allows to create the INSERTS statements
*    for TMPMOVIMIENTOS table
* =============================================================================
*
*    First Release : Paul Pasquel
*    Developed for : TAM
*    Developed by  : TAM
*    Date          : 2010-11-11
*
*=======================================================================
*
* Input/Ouput
* --------------
*             Y.PROCESS.DATE                (in)          Process Date
*             Y.AA.ID                       (in)          Arrangement Id
*             R.STATIC.MAPPING              (in)          Static Mapping Values, gotten from RAD.CONDUIT.LINEAR
*             Y.ACCOUNT.ID                  (in)          Contract Number
*             Y.PRODUCT.ID                  (in)          Producto Number
*             Y.AGENCY.CODE                 (in)          Agency Code
*             Y.CREDIT.TXN                  (out)         <1>List of insert <2> Details
*
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.TELLER
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_F.AA.ACTIVITY.HISTORY
    $INSERT I_F.STMT.ENTRY
    $INSERT I_F.TRANSACTION
*
    $INSERT I_REDO.COL.CUSTOMER.COMMON
*   $INSERT I_F.AA.ACTIVITY.HISTORY ;* R22 Auto conversion
*
*************************************************************************
*
    Y.START.TIME = TIME()
    GOSUB INITIALISE
    GOSUB CHECK.PRELIM.CONDITIONS
    IF PROCESS.GOAHEAD THEN
        GOSUB PROCESS
    END
    Y.ELAPSED.TIME = TIME()- Y.START.TIME ;* How long the select took
    MSG = 'REDO.COL.EXTRACT.TXN( ' : Y.AA.ID : ') time=':Y.ELAPSED.TIME:'secs'
    CALL OCOMO(MSG)

*
RETURN
*
* ======
PROCESS:
* ======
*
* << PACS00169639

    CALL CACHE.READ('F.AA.ACTIVITY.HISTORY',Y.AA.ID, R.AA.ACT.HIST, YERR)
    Y.AA.ED.TOTAL = DCOUNT(R.AA.ACT.HIST<AA.AH.EFFECTIVE.DATE>, @VM)
    Y.AA.ED.INDX = 0
    LOOP
        Y.AA.ED.INDX += 1
    WHILE PROCESS.GOAHEAD AND Y.AA.ED.INDX LE Y.AA.ED.TOTAL
        IF R.AA.ACT.HIST<AA.AH.EFFECTIVE.DATE,Y.AA.ED.INDX> EQ Y.PROCESS.DATE THEN
            GOSUB PROCESS.ACTIVITY
        END
    REPEAT
* >> PACS00169639

RETURN

* -----------------------------------------------------------------------------
PROCESS.ACTIVITY:
* -----------------------------------------------------------------------------
    Y.AA.REF.TOTAL = DCOUNT(R.AA.ACT.HIST<AA.AH.ACTIVITY.REF,Y.AA.ED.INDX>, @SM)
    Y.AA.REF.INDX = 0
    LOOP
*    REMOVE Y.AA.ACT.ID FROM Y.AA.ACT.MAIN.LIST SETTING Y.MARK
        Y.AA.REF.INDX += 1
    WHILE PROCESS.GOAHEAD AND Y.AA.REF.INDX LE Y.AA.REF.TOTAL

        IF R.AA.ACT.HIST<AA.AH.ACT.STATUS, Y.AA.ED.INDX, Y.AA.REF.INDX> NE 'AUTH' THEN
            NULL          ;* PACS.X
        END ELSE
            Y.AA.ACT.CLASS =  R.AA.ACT.HIST<AA.AH.ACTIVITY,Y.AA.ED.INDX, Y.AA.REF.INDX>
            Y.IS.APPLYPAYMENT    = Y.AA.ACT.CLASS["-",1,2] EQ 'LENDING-APPLYPAYMENT'
            Y.IS.CR.ARRANGEMENT  = Y.AA.ACT.CLASS["-",1,3] EQ 'LENDING-CREDIT-ARRANGEMENT'
            Y.IS.PAYOFF          = Y.AA.ACT.CLASS["-",1,3] EQ 'LENDING-SETTLE-PAYOFF'
            IF NOT(Y.IS.APPLYPAYMENT) AND NOT(Y.IS.CR.ARRANGEMENT) AND NOT(Y.IS.PAYOFF)  THEN
                NULL
            END ELSE
                GOSUB PROCESS.ACTIVITY.1        ;* PACS.X
            END
            IF E THEN     ;* PACS.X
                RETURN
            END
        END
    REPEAT
RETURN

* -----------------------------------------------------------------------------
PROCESS.ACTIVITY.1:
* -----------------------------------------------------------------------------
    Y.AA.ACT.ID = R.AA.ACT.HIST<AA.AH.ACTIVITY.REF, Y.AA.ED.INDX, Y.AA.REF.INDX>
    CALL CACHE.READ('F.AA.ARRANGEMENT.ACTIVITY',Y.AA.ACT.ID,R.AA.ACT,YERR)        ;* PACS00169639

    IF NOT(R.AA.ACT<AA.ARR.ACT.TXN.SYSTEM.ID>) MATCHES "FT" AND  NOT(R.AA.ACT<AA.ARR.ACT.TXN.SYSTEM.ID>) MATCHES "TT"  THEN
        RETURN
    END

    Y.TYPE.TXN = "PAYOFF.TXN"

    GOSUB INITIALISE.VARS

    GOSUB GET.AMOUNTS.DETAILS

    Y.TXN.CONTRACT = R.AA.ACT<AA.ARR.ACT.TXN.CONTRACT.ID>
    Y.TXT.SYSTEM.ID = R.AA.ACT<AA.ARR.ACT.TXN.SYSTEM.ID>

*         Y.TMPMOVSECUENCIAL = Y.MVMT.COUNTER                                 ;* Por confirmar
*         Y.MVMT.COUNTER++

    IF R.AA.ACT<AA.ARR.ACT.EFFECTIVE.DATE> EQ '' THEN
        E = yValueMantatory
        E<2> = "AA.ARRANGEMENT.ACTITY>EFFECTIVE.DATE" : @VM : Y.AA.ACT.ID
        GOSUB TRACE.ERROR
        RETURN
    END
    Y.TMPMOVFECHA = TRIM(R.AA.ACT<AA.ARR.ACT.EFFECTIVE.DATE>,"","B")

* This was already validated on parameters conditions check
*         IF NOT(Y.TMPCREDITONUMEROCONTRATO) THEN
*            E = yValueMantatory
*            E<2> = "AA.ARRANGEMENT>LINKED.APPL.ID" : VM : Y.AA.ID
*            GOSUB TRACE.ERROR
*            RETURN
*         END

    Y.TMPMOVMONTO = R.AA.ACT<AA.ARR.ACT.ORIG.TXN.AMT>

    Y.TMPMOVUSUARIO = R.AA.ACT<AA.ARR.ACT.INPUTTER,1>[1,15]

    Y.TMPMOVHORA = TRIM(R.AA.ACT<AA.ARR.ACT.DATE.TIME,1>,"","B")

    GOSUB GET.TXN.DETAILS
    IF E THEN
        RETURN
    END

    Y.TMPMOVTIPOTRNCODIGO = Y.COLL.TXN.CODE
    Y.TMPMOVTIPOTRNDESCRIPCION = ""

    GOSUB INSERT.STMT

    Y.CREDIT.TXN<-1> = Y.INS.VALUES : @VM : Y.TYPE.TXN



RETURN
*
*
* ---------
INITIALISE:
* ---------
*
* Variables that allow the comparision
    Y.LEN.ACCOUNT = LEN("ACCOUNT")
    Y.LEN.PRINCIPALINT = LEN("PRINCIPALINT")

    Y.CONTADOR = 1
    P.TYPE_PROC = "DELIVERY"
    P.STATUS.ERR = "20"
    P.TABLE  = "TMPMOVIMIENTOS"
*    C.INT.CODE = REDO.INTERFACE.PARAM.ID
*    C.INT.TYPE='BATCH'
*    C.BAT.TOT= 0
*    C.INFO.OR=''
*    C.INFO.DE=''
*    C.ID.PROC=REDO.INTERFACE.PARAM.ID
*    C.MON.TP='08'
    C.DESC=''
*    C.REC.CON=''
*    C.EX.USER=OPERATOR
*    C.EX.PC=TNO

*ADDED FOR PACS00200999 JV06182012
    FN.AA.ARRANGEMENT.ACTIVITY = 'F.AA.ARRANGEMENT.ACTIVITY'
    F.AA.ARRANGEMENT.ACTIVITY  =  ''
    Y.AA.ACT.CHILD.ID          =  ''
    R.AA.ACT.CHILD             =  ''
*JV
    PROCESS.GOAHEAD = 1

*    Y.INS.STMT = "INSERT INTO TMPMOVIMIENTOS ("
*    Y.INS.STMT := "PARCOMPANIACODIGO,TMPMOVSECUENCIAL,TMPMOVFECHA,TMPCREDITONUMEROCONTRATO,"
*    Y.INS.STMT := "TMPMOVSISTEMACODIGO,TMPMOVPRODUCTO,TMPMOVMONTO,TMPMOVMONTOPAGOCAPITAL,"
*    Y.INS.STMT := "TMPMOVMONTOPAGOINTERES,TMPMOVMONTOPAGOOTROS,TMPMOVMONTOMONEDAORIGEN,"
*    Y.INS.STMT := "TMPMOVUSUARIO,TMPMOVHORA,TMPMOVTASACAMBIO,"
*    Y.INS.STMT := "TMPMOVMONEDACODIGO,TMPMOVTIPOTRNCODIGO,TMPMOVTIPOTRNDESCRIPCION,"
*    Y.INS.STMT := "TMPAGENCIACODIGO"
*    Y.INS.STMT := ") VALUES("

    Y.TMPMOVPRODUCTO  = Y.PRODUCT.ID
    Y.MAP.VALUE = Y.PRODUCT.ID
    Y.MAP.TYPE  = "PRODUCT.GROUP"
    GOSUB GET.STATIC.MAPPING
    Y.TMPMOVPRODUCTO  = Y.MAP.VALUE

    GOSUB GET.TXN.CODE.LIST
RETURN
*
*
* ---------
INITIALISE.VARS:
* ---------
*
*
    Y.COLL.TXN.CODE = ""
    Y.TMPAGENCIACODIGO = Y.AGENCY.CODE
    Y.TMPCREDITONUMEROCONTRATO = Y.ACCOUNT.ID
    Y.PARCOMPANIACODIGO = "1"
    Y.TMPMOVSECUENCIAL = ""
    Y.TMPMOVFECHA = ""
*      Y.TMPCREDITONUMEROCONTRATO = ""
    Y.TMPMOVSISTEMACODIGO = "BPR"
    Y.TMPMOVMONTO = ""
    Y.TMPMOVMONTOPAGOCAPITAL = 0
    Y.TMPMOVMONTOPAGOINTERES = 0
    Y.TMPMOVMONTOPAGOOTROS = 0
*      Y.TMPMOVMONTOMONEDAORIGEN = ""
    Y.TMPMOVUSUARIO = ""
    Y.TMPMOVHORA = ""
*      Y.TMPMOVCODIGOGESTOR = ""
    Y.TMPMOVTASACAMBIO = ""
    Y.TMPMOVMONEDACODIGO = ""
    Y.TMPMOVTIPOTRNCODIGO = ""
    Y.TMPMOVTIPOTRNDESCRIPCION = ""

    yIsPartialPayment = @FALSE
    yIsPayOff         = @FALSE
    yIsPayment        = @FALSE

    START.TIME = TIME()         ;* Start the clock
RETURN
* ---------
INSERT.STMT:
* ---------
*    Y.PARCOMPANIACODIGO = redoOracleNull(Y.PARCOMPANIACODIGO)
    Y.TMPMOVSECUENCIAL =TRIM(Y.TMPMOVSECUENCIAL,"","B")
*     Y.TMPMOVFECHA = redoOracleNull("'" : Y.TMPMOVFECHA : "'")
    Y.TMPCREDITONUMEROCONTRATO = TRIM(Y.TMPCREDITONUMEROCONTRATO,"","B")
*    Y.TMPMOVSISTEMACODIGO = redoOracleNull("'" : Y.TMPMOVSISTEMACODIGO : "'")
    Y.TMPMOVPRODUCTO = TRIM(Y.TMPMOVPRODUCTO,"","B")
    Y.TMPMOVMONTO = TRIM(Y.TMPMOVMONTO,"","B")
    Y.TMPMOVMONTOPAGOCAPITAL = TRIM(Y.TMPMOVMONTOPAGOCAPITAL,"","B")
    Y.TMPMOVMONTOPAGOINTERES = TRIM(Y.TMPMOVMONTOPAGOINTERES,"","B")
    Y.TMPMOVMONTOPAGOOTROS = TRIM(Y.TMPMOVMONTOPAGOOTROS,"","B")
    Y.TMPMOVMONTOMONEDAORIGEN = TRIM(Y.TMPMOVMONTOMONEDAORIGEN,"","B")
    Y.TMPMOVUSUARIO = TRIM(Y.TMPMOVUSUARIO,"","B")
*     Y.TMPMOVHORA = redoOracleNull("'" : Y.TMPMOVHORA : "'")
*     Y.TMPMOVCODIGOGESTOR = redoOracleNull("'" : Y.TMPMOVCODIGOGESTOR : "'")
    Y.TMPMOVTASACAMBIO =TRIM(Y.TMPMOVTASACAMBIO,"","B")
    Y.TMPMOVMONEDACODIGO = TRIM(Y.TMPMOVMONEDACODIGO,"","B")
    Y.TMPMOVTIPOTRNCODIGO = TRIM(Y.TMPMOVTIPOTRNCODIGO,"","B")
    Y.TMPMOVTIPOTRNDESCRIPCION = TRIM(Y.TMPMOVTIPOTRNDESCRIPCION,"","B")
    Y.TMPAGENCIACODIGO = TRIM(Y.TMPAGENCIACODIGO,"","B")
*     Y.TMPMOVIMIENTOSCAPITALVENCIDO = redoOracleNull("'" : Y.TMPMOVIMIENTOSCAPITALVENCIDO : "'")
*     Y.TMPMOVIMIENTOSINTERESVENCIDO = redoOracleNull("'" : Y.TMPMOVIMIENTOSINTERESVENCIDO : "'")
*     Y.TMPMOVIMIENTOSINTERESMORATORIO = redoOracleNull("'" : Y.TMPMOVIMIENTOSINTERESMORATORIO : "'")
*     Y.TMPMOVIMIENTOSOTROSVENCIDO = redoOracleNull("'" : Y.TMPMOVIMIENTOSOTROSVENCIDO : "'")
*     Y.TMPMOVIMIENTOSREFERENCIA = redoOracleNull("'" : Y.TMPMOVIMIENTOSREFERENCIA : "'")
*     Y.TMPMOVIMIENTOSPORCOMISION = redoOracleNull("'" : Y.TMPMOVIMIENTOSPORCOMISION : "'")
*     Y.TMPMOVIMIENTOSMONTOCOMISION = redoOracleNull("'" : Y.TMPMOVIMIENTOSMONTOCOMISION : "'")
*     Y.TMPMOVIMIENTOSFECHACALCULO = redoOracleNull("'" : Y.TMPMOVIMIENTOSFECHACALCULO : "'")
*     Y.TMPMOVIMIENTOSDIASATRASO = redoOracleNull("'" : Y.TMPMOVIMIENTOSDIASATRASO : "'")
*     Y.TMPMOVIMIENTOSESTADO = redoOracleNull("'" : Y.TMPMOVIMIENTOSESTADO : "'")
*     Y.TMPMOVIMIENTOSFECHAANULADO = redoOracleNull("'" : Y.TMPMOVIMIENTOSFECHAANULADO : "'")
*     Y.TMPMOVCARTERACLIENTECODIGO = redoOracleNull("'" : Y.TMPMOVCARTERACLIENTECODIGO : "'")

    Y.INS.VALUES = ""
    Y.DELIM='~'
    Y.INS.VALUES := Y.PARCOMPANIACODIGO: Y.DELIM : Y.TMPMOVSECUENCIAL: Y.DELIM : Y.TMPMOVFECHA: Y.DELIM
    Y.INS.VALUES := Y.TMPCREDITONUMEROCONTRATO: Y.DELIM : Y.TMPMOVSISTEMACODIGO: Y.DELIM : Y.TMPMOVPRODUCTO: Y.DELIM
    Y.INS.VALUES := Y.TMPMOVMONTO: Y.DELIM : Y.TMPMOVMONTOPAGOCAPITAL: Y.DELIM: Y.TMPMOVMONTOPAGOINTERES: Y.DELIM
    Y.INS.VALUES := Y.TMPMOVMONTOPAGOOTROS: Y.DELIM: Y.TMPMOVMONTOMONEDAORIGEN: Y.DELIM : Y.TMPMOVUSUARIO: Y.DELIM
*     Y.INS.VALUES := Y.TMPMOVMONTOPAGOOTROS: "," : Y.TMPMOVUSUARIO: ","
*     Y.INS.VALUE := Y.TMPMOVHORA: "," : Y.TMPMOVCODIGOGESTOR: "," : Y.TMPMOVTASACAMBIO: "," :
    Y.INS.VALUES := Y.TMPMOVHORA: Y.DELIM: Y.TMPMOVTASACAMBIO: Y.DELIM
    Y.INS.VALUES := Y.TMPMOVMONEDACODIGO: Y.DELIM : Y.TMPMOVTIPOTRNCODIGO: Y.DELIM : Y.TMPMOVTIPOTRNDESCRIPCION: Y.DELIM
    Y.INS.VALUES := Y.TMPAGENCIACODIGO
*     Y.INS.VALUE := Y.TMPAGENCIACODIGO: "," : Y.TMPMOVIMIENTOSCAPITALVENCIDO: "," : Y.TMPMOVIMIENTOSINTERESVENCIDO: ","
*     Y.INS.VALUE := Y.TMPMOVIMIENTOSINTERESMORATORIO: "," : Y.TMPMOVIMIENTOSOTROSVENCIDO: "," : Y.TMPMOVIMIENTOSREFERENCIA: ","
*     Y.INS.VALUE := Y.TMPMOVIMIENTOSPORCOMISION: "," : Y.TMPMOVIMIENTOSMONTOCOMISION: "," : Y.TMPMOVIMIENTOSFECHACALCULO: ","
*     Y.INS.VALUE := Y.TMPMOVIMIENTOSDIASATRASO: "," : Y.TMPMOVIMIENTOSESTADO: "," : Y.TMPMOVIMIENTOSFECHAANULADO: ","
*     Y.INS.VALUE := Y.TMPMOVCARTERACLIENTECODIGO: ","
*    Y.INS.VALUES = Y.INS.STMT : Y.INS.VALUES : ")"


RETURN

*-----------------------------------------------------------------------------
GET.STATIC.MAPPING:
*-----------------------------------------------------------------------------

    E = ""
    CALL APAP.TAM.REDO.R.COL.GET.MAPPING(C.ID.STATIC.MAPPING, R.STATIC.MAPPING, 1, R.STATIC.MAPPING, Y.MAP.TYPE, Y.MAP.VALUE);* R22 Manual conversion - CALL method format changed
    IF E THEN
        GOSUB TRACE.ERROR
    END

RETURN

*
*-----------------------
CHECK.PRELIM.CONDITIONS:
*-----------------------
*
    LOOP.CNT  = 1   ;   MAX.LOOPS = 3

    LOOP
    WHILE LOOP.CNT LE MAX.LOOPS AND PROCESS.GOAHEAD DO
        BEGIN CASE
            CASE LOOP.CNT EQ 1
                IF Y.PRODUCT.ID EQ "" THEN
                    PROCESS.GOAHEAD = 0
                    E = yRoutineParamRequired : @FM : "Y.PRODUCT.ID" : @VM : "REDO.COL.EXTRACT.TXN"
                END
            CASE LOOP.CNT EQ 2
                IF Y.ACCOUNT.ID EQ "" THEN
                    PROCESS.GOAHEAD = 0
                    E = yRoutineParamRequired : @FM : "Y.ACCOUNT.ID" : @VM : "REDO.COL.EXTRACT.TXN"
                END
            CASE LOOP.CNT EQ 3
                IF Y.AGENCY.CODE EQ "" THEN
                    PROCESS.GOAHEAD = 0
                    E = yRoutineParamRequired : @FM : "Y.AGENCY.CODE" : @VM : "REDO.COL.EXTRACT.TXN"
                END
*
        END CASE
        LOOP.CNT +=1
    REPEAT

RETURN
*
*-----------------------------------------------------------------------------
GET.TXN.DETAILS:
*-----------------------------------------------------------------------------
    Y.CCY.RATE = 1
    Y.CCY      = ""
    PROCESS.GOAHEAD = 1
    LOOP
    WHILE PROCESS.GOAHEAD
        IF Y.TXT.SYSTEM.ID EQ "AA" THEN
            R.AA.ACT.NEW = ''
            CALL CACHE.READ('F.AA.ARRANGEMENT.ACTIVITY',Y.TXN.CONTRACT,R.AA.ACT.NEW,YERR) ;

            IF R.AA.ACT.NEW<AA.ARR.ACT.TXN.SYSTEM.ID> MATCHES "FT" : @VM : "TT"  THEN  ;* @MG
                Y.TXT.SYSTEM.ID = R.AA.ACT.NEW<AA.ARR.ACT.TXN.SYSTEM.ID>
                Y.TXN.CONTRACT = R.AA.ACT.NEW<AA.ARR.ACT.TXN.CONTRACT.ID>
                PROCESS.GOAHEAD = 0
            END ELSE
                Y.TXN.CONTRACT = R.AA.ACT.NEW<AA.ARR.ACT.TXN.CONTRACT.ID>
            END
        END ELSE
            PROCESS.GOAHEAD = 0
        END

    REPEAT
    BEGIN CASE
*
        CASE Y.TXT.SYSTEM.ID EQ "TT"
            R.TT = ""
            CALL CACHE.READ('F.TELLER',Y.TXN.CONTRACT, R.TT, YERR)
            IF YERR THEN
                YERR = ""
                CALL CACHE.READ('F.TELLER$HIS',Y.TXN.CONTRACT : ";1", R.TT, YERR)         ;* PP
                IF YERR THEN
                    E = yRecordNotFound
                    E<2> = Y.TXN.CONTRACT : ";1" : @VM : "F.TELLER"
                    GOSUB TRACE.ERROR
                    RETURN
                END
            END

            IF R.TT<TT.TE.DEAL.RATE> NE "" THEN
                Y.CCY.RATE = R.TT<TT.TE.DEAL.RATE>
            END
            Y.CCY      = R.TT<TT.TE.CURRENCY.1>
            Y.TMPMOVSECUENCIAL = "1" : Y.TXN.CONTRACT[3,99]
        CASE Y.TXT.SYSTEM.ID EQ "FT"
            R.FT = ""
            CALL CACHE.READ('F.FUNDS.TRANSFER',Y.TXN.CONTRACT, R.FT, YERR)
            IF YERR THEN
                CALL CACHE.READ('F.FUNDS.TRANSFER$HIS',Y.TXN.CONTRACT : ";1", R.FT, YERR)
                IF YERR THEN
                    E = yRecordNotFound
                    E<2> = Y.TXN.CONTRACT : ";1" : @VM : "F.FUNDS.TRANSFER"
                    GOSUB TRACE.ERROR
                    RETURN
                END
            END
            IF R.FT<FT.CUSTOMER.RATE> NE "" THEN
                Y.CCY.RATE = R.FT<FT.CUSTOMER.RATE>
            END
            Y.CCY = R.FT<FT.DEBIT.CURRENCY>
            Y.TMPMOVSECUENCIAL = "2" : Y.TXN.CONTRACT[3,99]
*
        CASE 1
            E = "ST-REDO.COL.TXN.SYSTEM.NO.DEF" : @VM : "TXT.CONTRAT.ID & WAS NOT VALID SYSTEM.ID -&-, MUST BE TT OR FT"
            E<2> = Y.TXN.CONTRACT : @VM : Y.TXT.SYSTEM.ID
            GOSUB TRACE.ERROR
            RETURN
    END CASE

    Y.MAP.VALUE = Y.CCY
    Y.MAP.TYPE  = "CURRENCY"
    GOSUB GET.STATIC.MAPPING
*    Y.TMPMOVMONTOMONEDAORIGEN = Y.MAP.VALUE
    Y.TMPMOVMONEDACODIGO=Y.MAP.VALUE
    IF E NE "" THEN
        RETURN
    END

    Y.TMPMOVTASACAMBIO = Y.CCY.RATE

RETURN
*-----------------------------------------------------------------------------
* Allows to get the details of each amount that was paid with this Activity
GET.AMOUNTS.DETAILS:
*-----------------------------------------------------------------------------
    R.AA.ACT.CHILD = R.AA.ACT
    GOSUB GET.AA.ACTIVITY.DETAIL
    Y.AA.ACT.LIST = R.AA.ACT<AA.ARR.ACT.CHILD.ACTIVITY>
    CALL OPF(FN.AA.ARRANGEMENT.ACTIVITY,F.AA.ARRANGEMENT.ACTIVITY)
    LOOP
        REMOVE Y.AA.ACT.CHILD.ID FROM Y.AA.ACT.LIST SETTING Y.MARK.AA
    WHILE Y.MARK.AA : Y.AA.ACT.CHILD.ID
        CALL F.READ(FN.AA.ARRANGEMENT.ACTIVITY,Y.AA.ACT.CHILD.ID, R.AA.ACT.CHILD,F.AA.ARRANGEMENT.ACTIVITY,YERR)
        IF YERR THEN
            CALL OCOMO("ERROR READ AA.ACTIVITTY " : Y.AA.ACT.CHILD.ID )
        END
        ELSE
            GOSUB GET.AA.ACTIVITY.DETAIL
        END
    REPEAT

RETURN


*-----------------------------------------------------------------------------
* Allows to get the details of each amount that was paid with this Activity
GET.AA.ACTIVITY.DETAIL:
*-----------------------------------------------------------------------------

    Y.STMT.ID.ST = R.AA.ACT.CHILD<AA.ARR.ACT.STMT.NOS,1>
    Y.STMT.POS.START = R.AA.ACT.CHILD<AA.ARR.ACT.STMT.NOS,2>["-",1,1]
    Y.STMT.POS.END   = R.AA.ACT.CHILD<AA.ARR.ACT.STMT.NOS,2>["-",2,1]
    Y.STMT.POS = Y.STMT.POS.START
    LOOP    ;*CHANGE "FOR" STATEMENT FOR "WHILE" PACS00200999 JV06182012
    WHILE Y.STMT.POS LE Y.STMT.POS.END

        GOSUB READ.STMT.ENTRY
        IF Y.BALANCE.TYPE NE V$NULL THEN
            GOSUB EVAL.BALANCE.TYPE
        END
        Y.STMT.POS += 1
    REPEAT
RETURN

*-----------------------------------------------------------------------------
* Read STMT.ENTRY
* input : Y.STMT.ID, Y.STMT.POS ouput: Y.AMOUNT, Y.BALANCE.TYPE
READ.STMT.ENTRY:
*-----------------------------------------------------------------------------
    Y.AMOUNT = 0
    Y.BALANCE.TYPE = ""
    Y.STMT.POS = FMT(Y.STMT.POS,"R%4")
    Y.STMT.ID = Y.STMT.ID.ST : Y.STMT.POS
    YERR = ""

    CALL CACHE.READ('F.STMT.ENTRY',Y.STMT.ID,R.STMT.ENTRY,YERR)
    IF R.STMT.ENTRY THEN
        Y.AMOUNT = R.STMT.ENTRY<AC.STE.AMOUNT.FCY>
        IF Y.AMOUNT EQ "" THEN
            Y.AMOUNT = R.STMT.ENTRY<AC.STE.AMOUNT.LCY>
        END
        Y.BALANCE.TYPE = R.STMT.ENTRY<AC.STE.BALANCE.TYPE>
* This is a not PayOff activity ?
        IF NOT(yIsPayOff) THEN
            Y.TXN.CODE = R.STMT.ENTRY<AC.STE.TRANSACTION.CODE>
            GOSUB GET.TXN.CODE
        END
    END



RETURN
*-----------------------------------------------------------------------------
* Allows to get the type of Transaction to be report to Collector
GET.TXN.CODE:
*-----------------------------------------------------------------------------

    Y.QUERY.TXN.CODE = 0
    FIND Y.TXN.CODE IN Y.TXN.CODE.LIST<1> SETTING Y.FM.POS.TXN, Y.VM.POS.TXN, Y.SM.POS.TXN THEN
        Y.FOUND = @TRUE
        IF Y.TXN.TYPE.LIST<Y.FM.POS.TXN,Y.VM.POS.TXN> EQ "PAYOFF.TXN" THEN
            Y.TYPE.TXN = "PAYOFF.TXN"         ;* This is a PAY.OFF Txn
            yIsPayOff = @TRUE
            Y.QUERY.TXN.CODE = 1
        END
        IF Y.TXN.TYPE.LIST<Y.FM.POS.TXN,Y.VM.POS.TXN> EQ "PAY.TXN" THEN
            Y.TYPE.TXN = "PAYOFF.TXN"         ;* Payments must be reported as PAYOFF group
            yIsPayment = @TRUE
            Y.QUERY.TXN.CODE = 1
        END
    END

* Was not a payOff and not a Payment
    IF NOT(yIsPayOff) AND NOT(yIsPayment) THEN      ;*AND Y.TXN.TYPE.LIST<Y.FM.POS.TXN,Y.VM.POS.TXN> EQ "PARTIALPAY.TXN"
* This is a PARTIAL PAYMENT Txn
        Y.TYPE.TXN = "PARTIALPAY.TXN"
        yIsPartialPayment = @TRUE
        Y.QUERY.TXN.CODE = 1
    END

    IF Y.QUERY.TXN.CODE THEN

        CALL CACHE.READ('F.TRANSACTION',Y.TXN.CODE,R.TXN,YERR)
        IF R.TXN<AC.TRA.LOCAL.REF,Y.COL.TXN.CODE.ID> EQ "" THEN
*            CALL OCOMO("COLLECTOR TXN CODE TO SEND COLLECTOR NOT DEF FOR TXN.ID " :Y.TXN.CODE)
        END ELSE
            Y.COLL.TXN.CODE = R.TXN<AC.TRA.LOCAL.REF,Y.COL.TXN.CODE.ID>
            Y.TMPMOVTIPOTRNDESCRIPCION = R.TXN<AC.TRA.NARRATIVE,1>[1,100]
        END
    END

RETURN
*-----------------------------------------------------------------------------
* Get the list of transaction associated with Each Entry (Payoff, partialpayment, pay)
*
GET.TXN.CODE.LIST:
*-----------------------------------------------------------------------------
    Y.TXN.TYPE.LIST = "PAYOFF.TXN" : @VM : "PAY.TXN" : @VM : "PARTIALPAY.TXN"
    Y.TXN.CODE.LIST = ""
    LOOP
        REMOVE Y.TXN.TYPE.ID FROM Y.TXN.TYPE.LIST SETTING Y.TXN.TYPE.MARK
    WHILE Y.TXN.TYPE.ID : Y.TXN.TYPE.MARK
        FIND Y.TXN.TYPE.ID IN R.STATIC.MAPPING SETTING Y.FM.POS, Y.VM.POS THEN
            Y.TXN.CODE.LIST.1 = R.STATIC.MAPPING<Y.FM.POS>
            DEL Y.TXN.CODE.LIST.1<1,1>
            Y.TXN.CODE.LIST<1,-1> = LOWER(Y.TXN.CODE.LIST.1)
        END
    REPEAT

RETURN

*-----------------------------------------------------------------------------
* Record the error message
TRACE.ERROR:
*-----------------------------------------------------------------------------
*    C.DESC = E
*    CALL TXT(C.DESC)
*    C.DESC = C.DESC : "-" : Y.AA.ID
*    CALL REDO.R.COL.PROCESS.TRACE(P.TYPE_PROC, P.STATUS.ERR, Y.CONTADOR, P.TABLE, Y.TYPE.TXN, C.DESC)
*    CALL REDO.R.COL.EXTRACT.ERROR(C.DESC, "REDO.COL.EXTRACT.TXN",P.TABLE)
*     C.MON.TP='08'
*     C.ID.PROC = REDO.INTERFACE.PARAM.ID
*    Y.PROCESS.FLAG.TABLE<1,4> = ""
*    GOSUB STORE.MSG.ON.QUEUE
*CALL REDO.INTERFACE.REC.ACT(C.INT.CODE,C.INT.TYPE,Y.CONTADOR,C.BAT.TOT,C.INFO.OR,C.INFO.DE,C.ID.PROC,C.MON.TP,C.DESC,C.REC.CON,C.EX.USER,C.EX.PC)
*    PROCESS.GOAHEAD = 0
RETURN

*------------------------------------------------------------------------------
STORE.MSG.ON.QUEUE:
*------------------------------------------------------------------------------
*   Y.MSG.QUEUE.ID = ''
*   CALL ALLOCATE.UNIQUE.TIME(Y.MSG.QUEUE.ID)
*   Y.MSG.QUEUE.ID = ID.COMPANY:'.':TODAY:'.':Y.MSG.QUEUE.ID
*   R.REDO.COL.MSG.QUEUE = C.DESC
*   WRITE R.REDO.COL.MSG.QUEUE TO F.REDO.COL.MSG.QUEUE, Y.MSG.QUEUE.ID ON ERROR CALL OCOMO("NO REGISTRO EL SUCESO" : C.DESC)

RETURN

*-----------------------------------------------------------------------------
EVAL.BALANCE.TYPE:
*-----------------------------------------------------------------------------
    BEGIN CASE
        CASE Y.BALANCE.TYPE[Y.LEN.ACCOUNT] EQ "ACCOUNT"
            Y.TMPMOVMONTOPAGOCAPITAL += Y.AMOUNT
        CASE Y.BALANCE.TYPE[Y.LEN.ACCOUNT] EQ "PRINCIPALINT"
            Y.TMPMOVMONTOPAGOINTERES += Y.AMOUNT
        CASE 1
            Y.TMPMOVMONTOPAGOOTROS += Y.AMOUNT
    END CASE
RETURN
END
