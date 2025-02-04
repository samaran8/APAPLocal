* @ValidationCode : MjotODIyNjYwMTAyOkNwMTI1MjoxNjgyNjAwOTgzNTExOnNhbWFyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 27 Apr 2023 18:39:43
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
$PACKAGE APAP.REDORETAIL
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*12-04-2023            CONVERSION TOOL                AUTO R22 CODE CONVERSION           VM TO @VM ,FM TO @FM SM TO @SM and I++ to I=+1
*12-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            CALL RTN METHOD ADDED
SUBROUTINE REDO.COL.FF.EXTRACT.CREDIT(Y.CUSTOMER.ID, Y.CREDIT, Y.CREDIT.TXN)
**-----------------------------------------------------------------------------
* Name : REDO.COLLECTOR.EXTRACT.TO.CREDIT
*      : Allows to mapping the values from AA to INSERT for Collector Data Base
*
* -----------------------------------------------------------------------------------------
* This development uses and Static Mapping defined on RAD.CONDUIT.LINEAR with @id = REDO.COL.MAP.STATIC
*------------------------------------------------------------------------------
* @author hpasquel@temenos.com
* @stereotype subroutine
* @package REDO.COL
*
* @Parameters:
* ----------------------------------------------------------------------------
*             Y.CUSTOMER.ID   (in)  Customer to process
*             Y.CREDIT        (out) The insert instructions, separated by FM for TMPCREDITO
*             Y.CREDIT.TXN    (out) The insert instructions, separated by FM for TMPMOVIMIENTOS
* note.- For tracing the process we must keep the details data, this details will be returned in the second VM
* INPUT/OUTPUT
* ----------------------------------------------------------------------------
*             E               (out) The message error
*-----------------------------------------------------------------------------
*  HISTORY CHANGES:
*                  2011-11-30 - PACS00169639
*                               hpasquel@temenos.com        To improve SELECT statements
*
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.COMPANY
    $INSERT I_F.AA.ACCOUNT.DETAILS
    $INSERT I_F.ACCOUNT
    $INSERT I_F.ACCT.ACTIVITY
    $INSERT I_F.EB.CONTRACT.BALANCES
    $INSERT I_F.AA.INTEREST.ACCRUALS
    $INSERT I_F.AA.ACTIVITY.HISTORY
    $INSERT I_F.CUSTOMER
    $INSERT I_F.AA.CHARGE
    $INSERT I_F.AA.REFERENCE.DETAILS
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_F.AA.INTEREST
    $INSERT I_F.DATES
*
    $INSERT I_REDO.COL.CUSTOMER.COMMON
    $INSERT I_REDO.COL.EXTRACT.CREDIT.COMMON
*
    $INSERT I_F.REDO.CUSTOMER.ARRANGEMENT
    $INSERT I_F.REDO.H.PROVISION.PARAMETER
    $INSERT I_F.REDO.H.CUSTOMER.PROVISIONING
    $USING APAP.TAM
    $USING APAP.AA
    $USING APAP.REDOBATCH
    
*-----------------------------------------------------------------------------

    Y.START.TIME = TIME()
    E = ""
    GOSUB INITIALISE
    IF E THEN
        GOSUB TRACE.ERROR
        RETURN
    END
    GOSUB PROCESS
    Y.ELAPSED.TIME = TIME()- Y.START.TIME         ;* How long the select took
    MSG = 'tracking execution time REDO.COL.EXTRACT.CREDIT( ' : Y.CUSTOMER.ID : ') time=' :  Y.ELAPSED.TIME : 'secs'
    CALL OCOMO("")  ;     CALL OCOMO(MSG)     ;    CALL OCOMO("")
RETURN

*-----------------------------------------------------------------------------
* Just Porcess the list of AA for the current Customer
PROCESS:
*-----------------------------------------------------------------------------
* << PACS00169639
    CALL F.READ(FN.REDO.CUSTOMER.ARRANGEMENT,Y.CUSTOMER.ID,R.CUS.ARR,F.REDO.CUSTOMER.ARRANGEMENT,CUS.ARR.ERR)
    AA.LIST = R.CUS.ARR<CUS.ARR.OWNER>
* >> PACS00169639

    E = ""
    PROCESSED.AA.IDS = ""
    LOOP
        REMOVE AA.ID FROM AA.LIST SETTING AA.MARK
    WHILE AA.ID : AA.MARK AND PROCESS.GOAHEAD
        LOCATE AA.ID IN PROCESSED.AA.IDS<1> SETTING AA.POS THEN
            CONTINUE
        END
        PROCESSED.AA.IDS<-1> = AA.ID
        GOSUB INITIALISE.VARS
        E = ""
        GOSUB READ.MAIN.FILES
        IF NOT(E) AND Y.CONTINUE.AA THEN
            GOSUB PROCESS.AA
        END
        IF E NE "" THEN
            GOSUB TRACE.ERROR
        END
    REPEAT

RETURN

*-----------------------------------------------------------------------------
* Read Main records from AA files
READ.MAIN.FILES:
*-----------------------------------------------------------------------------
* Read Main Files
*CALL APAP.REDORETAIL.REDO.COL.EXTRACT.CREDIT.READ(AA.ID,R.AA,R.ACT.HIST,R.PRINCIPALINT,R.MORA.CHARGE,R.AA.ACCOUNT.DETAILS)
    CALL APAP.REDORETAIL.redoColExtractCreditRead(AA.ID,R.AA,R.ACT.HIST,R.PRINCIPALINT,R.MORA.CHARGE,R.AA.ACCOUNT.DETAILS);* MANUAL R22 CODE CONVERSION
    IF E NE "" THEN
        RETURN
    END
* << PACS00169639
    Y.CONTINUE.AA = @FALSE
    GOSUB CHECK.SELECTION.CRITERIA.AA
    IF Y.CONTINUE.AA EQ @FALSE THEN ;* AUTO R22 CODE CONVERSION
        RETURN
    END
* >> PACS00169639

*    Y.CHK.DATE=R.DATES(EB.DAT.LAST.WORKING.DAY)
*    IF R.AA<AA.ARR.START.DATE> GT Y.CHK.DATE THEN
*        Y.CONTINUE.AA = @FALSE
*        RETURN
*    END

* Product Code, this code is used to report TRACE info
    Y.TMPCREDITOCODIGOPRODUCTO = R.AA<AA.ARR.PRODUCT.GROUP>
    Y.PRODUCT.GROUP = Y.TMPCREDITOCODIGOPRODUCTO
*CALL APAP.TAM.REDO.R.COL.GET.MAPPING(C.ID.STATIC.MAPPING, R.STATIC.MAPPING, 1, R.STATIC.MAPPING, "PRODUCT.GROUP", Y.TMPCREDITOCODIGOPRODUCTO)
    CALL APAP.TAM.redoRColGetMapping(C.ID.STATIC.MAPPING, R.STATIC.MAPPING, 1, R.STATIC.MAPPING, "PRODUCT.GROUP", Y.TMPCREDITOCODIGOPRODUCTO);* R22 Manual conversion - CALL method format changed
    IF E NE "" THEN
        RETURN
    END

    Y.TMPCREDITOCODIGOTIPOINVERSION=R.AA<AA.ARR.PRODUCT.GROUP>
*CALL APAP.TAM.REDO.R.COL.GET.MAPPING(C.ID.STATIC.MAPPING, R.STATIC.MAPPING, 1, R.STATIC.MAPPING, "TIPO.INV", Y.TMPCREDITOCODIGOTIPOINVERSION)
    CALL APAP.TAM.redoRColGetMapping(C.ID.STATIC.MAPPING, R.STATIC.MAPPING, 1, R.STATIC.MAPPING, "TIPO.INV", Y.TMPCREDITOCODIGOTIPOINVERSION);* R22 Manual conversion
    
    IF E NE "" THEN
        RETURN
    END
    Y.START.DATE = R.AA<AA.ARR.START.DATE>

RETURN
*-----------------------------------------------------------------------------
* Process each AA contract
PROCESS.AA:
*-----------------------------------------------------------------------------
* Start Mapping
* Agency Code
    Y.CO.CODE = R.AA<AA.ARR.CO.CODE>
    R.COMP = ''
    CALL CACHE.READ('F.COMPANY',Y.CO.CODE, R.COMP ,YERR)
    IF YERR NE '' THEN
        E = yRecordNotFound : @FM : Y.CO.CODE : @VM : 'F.COMPANY'
        RETURN
    END
    Y.TMPCREDITOCODIGOAGENCIA = R.COMP<EB.COM.SUB.DIVISION.CODE> + 0  ;* Change from format 000X to Number
    Y.AGENCY.CODE = Y.TMPCREDITOCODIGOAGENCIA
    IF Y.TMPCREDITOCODIGOAGENCIA EQ '' THEN
        E = yValueMantatory : @FM : "COMPANY>SUB.DIVISION.CODE" : @VM : Y.CO.CODE
        RETURN
    END
* Contract Number
    Y.ACCOUNT.ID = ""
    LOCATE "ACCOUNT" IN R.AA<AA.ARR.LINKED.APPL,1> SETTING Y.POS THEN
        Y.ACCOUNT.ID =  R.AA<AA.ARR.LINKED.APPL.ID,Y.POS>
        Y.TMPCREDITONUMEROCONTRATO = Y.ACCOUNT.ID
    END ELSE
        E = "ST-REDO.COL.NON.ACCOUNT.REF" : @VM : "VALUE ACCOUNT NOT FOUND LINKED.APPL FIELD ON AA.ARRANGEMENT & ID" : @FM : AA.ID
        RETURN
    END
*
    CALL F.READ(FN.ACCOUNT, Y.ACCOUNT.ID ,R.ACCT, F.ACCOUNT, YERR)
    IF YERR NE '' THEN
        E = yRecordNotFound : @FM : Y.ACCOUNT.ID : @VM : 'F.ACCOUNT'
        RETURN
    END
    CALL F.READ(FN.EB.CONTRACT.BALANCES, Y.ACCOUNT.ID ,R.EB.CONTRACT.BALANCES, F.EB.CONTRACT.BALANCES, YERR)
    IF YERR NE '' THEN
        E = yRecordNotFound : @FM : Y.ACCOUNT.ID : @VM : 'F.EB.CONTRACT.BALANCES'
        RETURN
    END
* Customer Code
    Y.TMPCREDITOCODIGOCLIENTE   = Y.CUSTOMER.ID
* Ccy Code
    Y.TMPCREDITOCODIGOMONEDA = R.AA<AA.ARR.CURRENCY>
*CALL APAP.TAM.REDO.R.COL.GET.MAPPING(C.ID.STATIC.MAPPING, R.STATIC.MAPPING, 1, R.STATIC.MAPPING, "CURRENCY", Y.TMPCREDITOCODIGOMONEDA)
    CALL APAP.TAM.redoRColGetMapping(C.ID.STATIC.MAPPING, R.STATIC.MAPPING, 1, R.STATIC.MAPPING, "CURRENCY", Y.TMPCREDITOCODIGOMONEDA);* R22 Manual conversion - CALL method format changed
    IF E NE "" THEN
        RETURN
    END

    Y.TMPCREDITONUMEROCONTRATOORIGEN = R.ACCT<AC.ALT.ACCT.ID,1>       ;* Original Contract Number
    Y.TMPCREDITOFECHAINICIO = TRIM(R.AA<AA.ARR.START.DATE>,"","B")
* Due Days & Number of due payments

*CALL APAP.TAM.REDO.S.COL.GET.BILL.DETAILS(AA.ID,R.AA.ACCOUNT.DETAILS,  Y.PROCESS.DATE,  Y.MORA.CTA.VEN, Y.TMPCREDITODIASATRASO, Y.TMPCREDITOCUOTASVENCIDAS, Y.TMPCREDITOCUOTASPAGADAS, Y.TMPCREDITOMONTOMOROSO )
    CALL APAP.TAM.redoSColGetBillDetails(AA.ID,R.AA.ACCOUNT.DETAILS,  Y.PROCESS.DATE,  Y.MORA.CTA.VEN, Y.TMPCREDITODIASATRASO, Y.TMPCREDITOCUOTASVENCIDAS, Y.TMPCREDITOCUOTASPAGADAS, Y.TMPCREDITOMONTOMOROSO );* R22 Manual conversion - CALL method format changed
* Total
*    GOSUB GET.TOTAL.AMOUNT
* Number of Payments (call projector routine)
*    GOSUB PAYMENT.SCHEDULE.DETAILS
    Y.TMPCREDITOFECHAPROXIMOPAGO = TRIM(Y.TMPCREDITOFECHAPROXIMOPAGO,"","B")
* Payment Frequency (M,W,etc)
*CALL APAP.TAM.REDO.S.COL.GET.PAY.FREQ(AA.ID,  R.STATIC.MAPPING, Y.PROCESS.DATE, Y.TMPCREDITOTIPOCUOTA)
    CALL APAP.TAM.redoSColGetPayFreq(AA.ID,  R.STATIC.MAPPING, Y.PROCESS.DATE, Y.TMPCREDITOTIPOCUOTA);* R22 Manual conversion - CALL method format changed
    IF E THEN
        RETURN
    END
* Contract Status
*CALL APAP.AA.REDO.S.COL.GET.AA.STATUS(AA.ID, Y.PROCESS.DATE, Y.TMPCREDITOESTADOCONTRATO, Y.TMPCREDITOCOBROJUDICIAL)
    CALL APAP.AA.redoSColGetAaStatus(AA.ID, Y.PROCESS.DATE, Y.TMPCREDITOESTADOCONTRATO, Y.TMPCREDITOCOBROJUDICIAL);* R22 Manual conversion - CALL method format changed
    IF E THEN
        RETURN
    END
    Y.TMPCREDITOFECHAVCTO = TRIM(R.AA.ACCOUNT.DETAILS<AA.AD.MATURITY.DATE>,"","B")
    Y.TMPCREDITOCODIGOEJECUTIVO = R.ACCT<AC.ACCOUNT.OFFICER>[1,10]    ;* Account Officer

    GOSUB GET.PERIOD.BALANCES

    Y.TMPCREDITOMONTOINTMORADIARIO = 0 ;    Y.TYPE.SYSDATE = "ACCPENALTYINT"
    GOSUB GET.PENALTIES
    Y.TMPCREDITOMONTOINTMORADIARIO = ABS(Y.PENALTY.AMOUNT)

    Y.TMPCREDITOMONTOINTCORRDIARIO = 0 ;    Y.TYPE.SYSDATE = "ACCPRINCIPALINT"
    GOSUB GET.PENALTIES
    Y.TMPCREDITOMONTOINTCORRDIARIO = ABS(Y.PENALTY.AMOUNT)
* Fix 20170819
    YACT.RATE = R.PRINCIPALINT<AA.INT.ACC.RATE,1,1>
    IF YACT.RATE EQ '' THEN
        LAST.WORK.DAY = ''; LAST.WORK.DAY = R.DATES(EB.DAT.LAST.WORKING.DAY)
        R.AA.INTEREST  = ''; PROP.CLASS = ''; RET.ERR = '';returnConditions = ''
        PROP.NAME      = 'PRINCIPALINT'; effectiveDate = LAST.WORK.DAY
        CALL AA.GET.ARRANGEMENT.CONDITIONS(AA.ID,PROP.CLASS,PROP.NAME,effectiveDate,'',returnConditions,RET.ERR)
        R.AA.INTEREST = RAISE(returnConditions)
        YACT.RATE = R.AA.INTEREST<AA.INT.EFFECTIVE.RATE,1>
    END
* Fix 20170819
    Y.TMPCREDITOTASAINTERES = TRIM(YACT.RATE,"","B")        ;* Principal Interest
*    Y.TMPCREDITOTASAMORAINTERES = TRIM(R.PENALTYINT<AA.INT.ACC.RATE,1,1>,"","B")       ;* Penalty Interest Rate

    Y.TMPCREDITOTASAMORAINTERES = TRIM(R.MORA.CHARGE<1,AA.CHG.CHARGE.RATE>,"","B")

*CALL APAP.TAM.REDO.S.COL.GET.PAY.TYPE(AA.ID, R.STATIC.MAPPING, Y.PROCESS.DATE, Y.TMPCREDITOFORMAPAGO)
    CALL APAP.TAM.redoSColGetPayType(AA.ID, R.STATIC.MAPPING, Y.PROCESS.DATE, Y.TMPCREDITOFORMAPAGO);* R22 Manual conversion - CALL method format changed
    IF E THEN
        RETURN
    END
    GOSUB GET.HISTORY.PAYMENT
    IF E THEN
        RETURN
    END

    Y.TMPCREDITOFECHAULTIMOPAGO = TRIM(Y.TMPCREDITOFECHAULTIMOPAGO,"","B")

    Y.TMPCREDITOMORAMES01 = Y.TMPCREDITOCUOTASVENCIDAS
    Y.TMPCREDITOMORAMES02 = Y.MORA.CTA.VEN<1>
    Y.TMPCREDITOMORAMES03 = Y.MORA.CTA.VEN<2>
    Y.TMPCREDITOMORAMES04 = Y.MORA.CTA.VEN<3>
    Y.TMPCREDITOMORAMES05 = Y.MORA.CTA.VEN<4>
    Y.TMPCREDITOMORAMES06 = Y.MORA.CTA.VEN<5>
    Y.TMPCREDITOMORAMES07 = Y.MORA.CTA.VEN<6>
    Y.TMPCREDITOMORAMES08 = Y.MORA.CTA.VEN<7>
    Y.TMPCREDITOMORAMES09 = Y.MORA.CTA.VEN<8>
    Y.TMPCREDITOMORAMES10 = Y.MORA.CTA.VEN<9>
    Y.TMPCREDITOMORAMES11 = Y.MORA.CTA.VEN<10>
    Y.TMPCREDITOMORAMES12 = Y.MORA.CTA.VEN<11>

    Y.TMPCREDITOFACTORMONEDACONVERSI = ""
*CALL APAP.TAM.REDO.R.GET.MID.RATE.CM(R.AA<AA.ARR.CURRENCY>, 1, Y.TMPCREDITOFACTORMONEDACONVERSI)
    CALL APAP.TAM.redoRGetMidRateCm(R.AA<AA.ARR.CURRENCY>, 1, Y.TMPCREDITOFACTORMONEDACONVERSI);* R22 Manual conversion - CALL method format changed
*
    CALL F.READ(FN.CUSTOMER, Y.CUSTOMER.ID, R.CUS.TEMP, F.CUSTOMER, YERR)
    Y.TMPCREDITOCAMPO50 = R.CUS.TEMP<EB.CUS.LOCAL.REF><1,Y.L.CU.SCO.COB.POS>
*
*****PACS00523653****
*    CALL F.READ(FN.REDO.H.PROVISION.PARAMETER,'SYSTEM', R.RHPP,F.REDO.H.PROVISION.PARAMETER, YERR)
    CALL CACHE.READ(FN.REDO.H.PROVISION.PARAMETER,'SYSTEM', R.RHPP, YERR)
*****PACS00523653****
    Y.PROV.GROUP.LIST     =R.RHPP<PROV.PRODUCT.GROUP>
    Y.PROV.LOAN.TYPE.LIST =R.RHPP<PROV.LOAN.TYPE>
    Y.AA.PRD.GRP          =R.AA<AA.ARR.PRODUCT.GROUP>
    LOCATE Y.AA.PRD.GRP IN Y.PROV.GROUP.LIST<1,1> SETTING Y.PROV.LN.POS THEN
        Y.PROV.LOAN.TYPE=Y.PROV.LOAN.TYPE.LIST<1,Y.PROV.LN.POS>
    END

    CALL F.READ(FN.REDO.H.CUSTOMER.PROVISIONING, Y.CUSTOMER.ID, R.RHCP,F.REDO.H.CUSTOMER.PROVISIONING, YERR)
    Y.LOAN.TYPE.LIST             = R.RHCP<REDO.CUS.PROV.LOAN.TYPE>
    LOCATE Y.PROV.LOAN.TYPE IN Y.LOAN.TYPE.LIST<1,1> SETTING Y.CUR.CLASS.POS THEN
        Y.TMPCREDITOTIPOCALIFICACION = R.RHCP<REDO.CUS.PROV.CURRENT.CLASS,Y.CUR.CLASS.POS>
    END

    Y.TMPCREDITOMONTOACTUAL = Y.TMPCREDITOMONTOCAPITALVIGENTE
*
*CALL APAP.REDORETAIL.REDO.COL.FF.EXTRACT.CREDIT.2("GET.AA.CUSTOMER", Y.ACCOUNT.ID, AA.ID, "", P.GET.AA.CUSTOMER, "")
    CALL APAP.REDORETAIL.redoColFfExtractCredit2("GET.AA.CUSTOMER", Y.ACCOUNT.ID, AA.ID, "", P.GET.AA.CUSTOMER, "");* MANUAL R22 CODE CONVERSION
    IF E THEN
        RETURN
    END
    Y.TMPCREDITOCAMPO59 = P.GET.AA.CUSTOMER<1>
    Y.TMPCREDITOCAMPO60 = P.GET.AA.CUSTOMER<2>
*
*CALL APAP.REDORETAIL.REDO.COL.FF.EXTRACT.CREDIT.2("GET.CREDIT.GRANTED", Y.ACCOUNT.ID, AA.ID, P.GET.CREDIT.GRANTED, "", "")
    CALL APAP.REDORETAIL.redoColFfExtractCredit2("GET.CREDIT.GRANTED", Y.ACCOUNT.ID, AA.ID, P.GET.CREDIT.GRANTED, "", "");* MANUAL R22 CODE CONVERSION
    IF E THEN
        RETURN
    END
    Y.TMPCREDITOMONTOAPERTURA = P.GET.CREDIT.GRANTED<1>
    Y.TMPCREDITOGARANTIA           = P.GET.CREDIT.GRANTED<2>
*    Y.TMPCREDITOGARANTIA           = P.GET.CREDIT.GRANTED<3>
    Y.TMPCREDITOUBICACIONGARANTIA  = P.GET.CREDIT.GRANTED<4>
    Y.TMPCREDITOVALORGARANTIA      = P.GET.CREDIT.GRANTED<5>
*    Y.TMPCREDITOMONTODEUDATOTAL=Y.TMPCREDITOMONTOCAPITALVIGENTE+Y.TMPCREDITOMONTOINTERESVIGENTE+Y.ACCPENAL.INT+Y.TMPCREDITOMONTOMOROSO
    Y.PROP.AMT=''
    Y.TMPCREDITOMONTODEUDATOTAL=''
*CALL APAP.REDORETAIL.REDO.COL.GET.TOTAL.OUTSTANDING(AA.ID,Y.PROP.AMT,Y.TMPCREDITOMONTODEUDATOTAL)
    CALL APAP.REDORETAIL.redoColGetTotalOutstanding(AA.ID,Y.PROP.AMT,Y.TMPCREDITOMONTODEUDATOTAL);* MANUAL R22 CODE CONVERSION

    GOSUB PROCESS.ITEMS.DETAIL
    GOSUB INSERT.STMT

    Y.LWD=R.DATES(EB.DAT.LAST.WORKING.DAY)
*CALL APAP.REDORETAIL.REDO.COL.FF.EXTRACT.TXN(Y.LWD, AA.ID, R.STATIC.MAPPING, Y.ACCOUNT.ID, Y.PRODUCT.GROUP, Y.AGENCY.CODE, Y.CREDIT.TXN)
    CALL APAP.REDORETAIL.redoColFfExtractTxn(Y.LWD, AA.ID, R.STATIC.MAPPING, Y.ACCOUNT.ID, Y.PRODUCT.GROUP, Y.AGENCY.CODE, Y.CREDIT.TXN);* MANUAL R22 CODE CONVERSION

RETURN

*-----------------------------------------------------------------------------
* Initialise Process variables
INITIALISE:
*-----------------------------------------------------------------------------
    P.TABLE = "TMPCREDITO"
    PROCESS.GOAHEAD = 1
    Y.PROCESS.DATE = C.REPORT.PROCESS.DATE        ;* R.DATES(EB.DAT.LAST.WORKING.DAY) ;* TODAY

    R.AA   = ""     ;* AA.ARRANGEMENT
    R.ACCOUNT.DETAILS = ""    ;* AA.ACCOUNT.DETAILS
    R.ACCT = ""     ;* ACCOUNT

    R.STATIC.MAPPING    =  C.STATIC.MAPPING       ;* Static Mapping
*CALL APAP.TAM.REDO.R.COL.GET.MAPPING(C.ID.STATIC.MAPPING, R.STATIC.MAPPING, 1, R.STATIC.MAPPING.OUT, "", "")
    CALL APAP.TAM.redoRColGetMapping(C.ID.STATIC.MAPPING, R.STATIC.MAPPING, 1, R.STATIC.MAPPING.OUT, "", "");* MANUAL R22 CODE CONVERSION
    IF E THEN
        RETURN
    END
    R.STATIC.MAPPING = R.STATIC.MAPPING.OUT
*    Y.INS.STMT = C.INS.CREDIT.STMT

RETURN

* ---------------------------------------------------------------------------------------------
* Initialise variables for INSERT statement
INITIALISE.VARS:
* ---------------------------------------------------------------------------------------------

* Variables to use building the INSERT statement
    Y.TMPCREDITOCODIGOTIPOINVERSION=""
    Y.TMPCREDITOCODIGOAGENCIA = ""           ;     Y.TMPCREDITOCODIGOPRODUCTO = ""
    Y.TMPCREDITONUMEROCONTRATO = ""          ;     Y.TMPCREDITOCODIGOCLIENTE = ""
    Y.TMPCREDITOCODIGOMONEDA = ""            ;     Y.TMPCREDITONUMEROCONTRATOORIGEN = ""
    Y.TMPCREDITODIASATRASO = ""              ;     Y.TMPCREDITOCANTIDADCUOTASTOTAL = ""
    Y.TMPCREDITOCUOTASCREADAS = ""           ;     Y.TMPCREDITOCUOTASVENCIDAS = ""
    Y.TMPCREDITOCUOTASPAGADAS = ""           ;     Y.TMPCREDITOTIPOCUOTA = ""
    Y.TMPCREDITOESTADOCONTRATO = ""          ;     Y.TMPCREDITOFECHAINICIO = ""
    Y.TMPCREDITOFECHAVCTO = ""               ;     Y.TMPCREDITOMONTOMENSUALIDAD = ""
    Y.TMPCREDITOMONTOAPERTURA = ""           ;     Y.TMPCREDITOMONTOCAPITALVIGENTE = ""
    Y.TMPCREDITOMONTOCAPITALMOROSO = ""      ;     Y.TMPCREDITOMONTOCAPITALVENCIDO = ""
    Y.TMPCREDITOMONTOINTERESMOROSO = ""      ;     Y.TMPCREDITOMONTOINTERESVIGENTE = ""
    Y.TMPCREDITOMONTOINTERESVENCIDO = ""     ;     Y.TMPCREDITOMONTOSEGUROVIDAVENCI = 0
    Y.TMPCREDITOMONTOSEGUROFISICOVEN = 0     ;     Y.TMPCREDITOMONTOSEGUROVIDAVIGEN = 0
    Y.TMPCREDITOMONTOSEGUROFISICOVIG = 0     ;     Y.TMPCREDITOMONTOVIGENTEOTROS = 0
    Y.TMPCREDITOMONTOMOROSOOTROS = 0         ;     Y.TMPCREDITOMONTODEUDATOTAL = ""
    Y.TMPCREDITOMONTOMOROSO = ""             ;     Y.TMPCREDITOMONTOINTMORADIARIO = ""
    Y.TMPCREDITOMONTOINTCORRDIARIO = ""      ;     Y.TMPCREDITOTASAINTERES = ""
    Y.TMPCREDITOTASAMORAINTERES = ""         ;     Y.TMPCREDITOCODIGOEJECUTIVO = ""
    Y.TMPCREDITOFORMAPAGO = ""               ;     Y.TMPCREDITOFECHAULTIMOPAGO = ""
    Y.TMPCREDITOMONTOULTIMOPAGO = ""         ;     Y.TMPCREDITOFECHAPROXIMOPAGO = ""
    Y.TMPCREDITOTIPOCALIFICACION = ""        ;     Y.TMPCREDITOFECHAENTRADACOBRO = ""
    Y.TMPCREDITOCOBROJUDICIAL = ""           ;     Y.TMPCREDITOCODIGOVISTACREDITO = ""
    Y.TMPCREDITOMORAMES01 = ""               ;     Y.TMPCREDITOMORAMES02 = ""
    Y.TMPCREDITOMORAMES03 = ""               ;     Y.TMPCREDITOMORAMES04 = ""
    Y.TMPCREDITOMORAMES05 = ""               ;     Y.TMPCREDITOMORAMES06 = ""
    Y.TMPCREDITOMORAMES07 = ""               ;     Y.TMPCREDITOMORAMES08 = ""
    Y.TMPCREDITOMORAMES09 = ""               ;     Y.TMPCREDITOMORAMES10 = ""
    Y.TMPCREDITOMORAMES11 = ""               ;     Y.TMPCREDITOMORAMES12 = ""
    Y.TMPCREDITOFACTORMONEDACONVERSI = ""    ;     Y.TMPCREDITOCAMPO59 = ""
    Y.TMPCREDITOCAMPO60 = ""                 ;     Y.TMPCREDITOGARANTIA = ""
    Y.TMPCREDITOUBICACIONGARANTIA = ""       ;     Y.TMPCREDITOVALORGARANTIA = ""
    Y.TMPCREDITOMONTOACTUAL = ""

RETURN

*-----------------------------------------------------------------------------
* Create INSERT statement
INSERT.STMT:
*-----------------------------------------------------------------------------
    Y.TMPCREDITOCODIGOTIPOINVERSION= TRIM(Y.TMPCREDITOCODIGOTIPOINVERSION,"","B")
    Y.TMPCREDITOCODIGOAGENCIA      = TRIM(Y.TMPCREDITOCODIGOAGENCIA,"","B")
    Y.TMPCREDITOCODIGOPRODUCTO = TRIM(Y.TMPCREDITOCODIGOPRODUCTO,"","B")
    Y.TMPCREDITONUMEROCONTRATO = TRIM(Y.TMPCREDITONUMEROCONTRATO,"","B")
    Y.TMPCREDITOCODIGOCLIENTE = TRIM(Y.TMPCREDITOCODIGOCLIENTE,"","B")
    Y.TMPCREDITOCODIGOMONEDA = TRIM(Y.TMPCREDITOCODIGOMONEDA,"","B")
    Y.TMPCREDITONUMEROCONTRATOORIGEN = TRIM(Y.TMPCREDITONUMEROCONTRATOORIGEN,"","B")
    Y.TMPCREDITOCUOTASCREADAS=Y.AL.PAID.BILLS.CNT+Y.TMPCREDITOCUOTASVENCIDAS+Y.TMPCREDITOCUOTASPAGADAS
    Y.TMPCREDITOCUOTASCREADAS = TRIM(Y.TMPCREDITOCUOTASCREADAS,"","B")
    Y.TMPCREDITOCUOTASVENCIDAS = TRIM(Y.TMPCREDITOCUOTASVENCIDAS,"","B")
    Y.TMPCREDITOTIPOCUOTA = TRIM(Y.TMPCREDITOTIPOCUOTA,"","B")
    Y.TMPCREDITOESTADOCONTRATO = TRIM(Y.TMPCREDITOESTADOCONTRATO,"","B")
    Y.TMPCREDITOMONTOMENSUALIDAD = TRIM(Y.TMPCREDITOMONTOMENSUALIDAD,"","B")
    Y.TMPCREDITOMONTOAPERTURA = TRIM(Y.TMPCREDITOMONTOAPERTURA,"","B")
    Y.TMPCREDITOCODIGOEJECUTIVO = TRIM(Y.TMPCREDITOCODIGOEJECUTIVO,"","B")
    Y.TMPCREDITOFORMAPAGO = TRIM(Y.TMPCREDITOFORMAPAGO,"","B")
    Y.TMPCREDITOMONTOULTIMOPAGO = TRIM(Y.TMPCREDITOMONTOULTIMOPAGO,"","B")
    Y.TMPCREDITOTIPOCALIFICACION = TRIM(Y.TMPCREDITOTIPOCALIFICACION,"","B")
    Y.TMPCREDITOFECHAENTRADACOBRO = TRIM(Y.TMPCREDITOFECHAENTRADACOBRO,"","B")
    Y.TMPCREDITOCOBROJUDICIAL = TRIM(Y.TMPCREDITOCOBROJUDICIAL,"","B")
    Y.TMPCREDITOCODIGOVISTACREDITO = TRIM(Y.TMPCREDITOCODIGOVISTACREDITO,"","B")
    Y.TMPCREDITOMORAMES01 = TRIM(Y.TMPCREDITOMORAMES01,"","B")
    Y.TMPCREDITOMORAMES02 = TRIM(Y.TMPCREDITOMORAMES02,"","B")
    Y.TMPCREDITOMORAMES03 = TRIM(Y.TMPCREDITOMORAMES03,"","B")
    Y.TMPCREDITOMORAMES04 = TRIM(Y.TMPCREDITOMORAMES04,"","B")
    Y.TMPCREDITOMORAMES05 = TRIM(Y.TMPCREDITOMORAMES05,"","B")
    Y.TMPCREDITOMORAMES06 = TRIM(Y.TMPCREDITOMORAMES06,"","B")
    Y.TMPCREDITOMORAMES07 = TRIM(Y.TMPCREDITOMORAMES07,"","B")
    Y.TMPCREDITOMORAMES08 = TRIM(Y.TMPCREDITOMORAMES08,"","B")
    Y.TMPCREDITOMORAMES09 = TRIM(Y.TMPCREDITOMORAMES09,"","B")
    Y.TMPCREDITOMORAMES10 = TRIM(Y.TMPCREDITOMORAMES10,"","B")
    Y.TMPCREDITOMORAMES11 = TRIM(Y.TMPCREDITOMORAMES11,"","B")
    Y.TMPCREDITOMORAMES12 = TRIM(Y.TMPCREDITOMORAMES12,"","B")
    Y.TMPCREDITOFACTORMONEDACONVERSI = TRIM(Y.TMPCREDITOFACTORMONEDACONVERSI,"","B")
    Y.TMPCREDITOCAMPO59 = TRIM(Y.TMPCREDITOCAMPO59,"","B")
    Y.TMPCREDITOCAMPO60 = TRIM(Y.TMPCREDITOCAMPO60,"","B")
    Y.TMPCREDITOGARANTIA = TRIM(Y.TMPCREDITOGARANTIA,"","B")
    Y.TMPCREDITOUBICACIONGARANTIA = TRIM(Y.TMPCREDITOUBICACIONGARANTIA,"","B")
    Y.TMPCREDITOVALORGARANTIA = TRIM(Y.TMPCREDITOVALORGARANTIA,"","B")
    Y.TMPCREDITOMONTOACTUAL = TRIM(Y.TMPCREDITOMONTOACTUAL,"","B")

    Y.DELIM='~'
    Y.INS.VALUES = ""
    Y.INS.VALUES := "1":Y.DELIM:"BPR":Y.DELIM:'1':Y.DELIM:Y.TMPCREDITOCODIGOTIPOINVERSION:Y.DELIM
    Y.INS.VALUES := Y.TMPCREDITOCODIGOAGENCIA : Y.DELIM: Y.TMPCREDITOCODIGOPRODUCTO : Y.DELIM
    Y.INS.VALUES := Y.TMPCREDITONUMEROCONTRATO : Y.DELIM : Y.TMPCREDITOCODIGOCLIENTE : Y.DELIM : Y.TMPCREDITOCODIGOMONEDA : Y.DELIM
    Y.INS.VALUES := Y.TMPCREDITONUMEROCONTRATOORIGEN : Y.DELIM : "" : Y.DELIM: "1" : Y.DELIM
    Y.INS.VALUES := TRIM(Y.TMPCREDITODIASATRASO,"","B") : Y.DELIM : Y.TMPCREDITOCANTIDADCUOTASTOTAL : Y.DELIM: Y.TMPCREDITOCUOTASCREADAS : Y.DELIM
    Y.INS.VALUES := Y.TMPCREDITOCUOTASVENCIDAS : Y.DELIM : TRIM(Y.TMPCREDITOCUOTASPAGADAS,"","B") : Y.DELIM : Y.TMPCREDITOTIPOCUOTA : Y.DELIM
    Y.INS.VALUES := Y.TMPCREDITOESTADOCONTRATO : Y.DELIM : Y.TMPCREDITOFECHAINICIO : Y.DELIM : Y.TMPCREDITOFECHAVCTO : Y.DELIM
    Y.INS.VALUES := Y.TMPCREDITOMONTOMENSUALIDAD : Y.DELIM: Y.TMPCREDITOMONTOAPERTURA : Y.DELIM : Y.TMPCREDITOMONTOCAPITALVIGENTE : Y.DELIM
    Y.INS.VALUES := Y.TMPCREDITOMONTOCAPITALMOROSO : Y.DELIM: Y.TMPCREDITOMONTOCAPITALVENCIDO : Y.DELIM : Y.TMPCREDITOMONTOINTERESMOROSO : Y.DELIM
    Y.INS.VALUES := Y.TMPCREDITOMONTOINTERESVIGENTE : Y.DELIM: Y.TMPCREDITOMONTOINTERESVENCIDO : Y.DELIM : Y.TMPCREDITOMONTOSEGUROVIDAVENCI : Y.DELIM
    Y.INS.VALUES := Y.TMPCREDITOMONTOSEGUROFISICOVEN : Y.DELIM : Y.TMPCREDITOMONTOSEGUROVIDAVIGEN : Y.DELIM : Y.TMPCREDITOMONTOSEGUROFISICOVIG : Y.DELIM
    Y.INS.VALUES := Y.TMPCREDITOMONTOVIGENTEOTROS : Y.DELIM : Y.TMPCREDITOMONTOMOROSOOTROS :Y.DELIM
    Y.INS.VALUES := Y.TMPCREDITOMONTODEUDATOTAL : Y.DELIM: TRIM(Y.TMPCREDITOMONTOMOROSO,"","B") : Y.DELIM: Y.TMPCREDITOMONTOINTMORADIARIO : Y.DELIM
    Y.INS.VALUES := Y.TMPCREDITOMONTOINTCORRDIARIO : Y.DELIM: Y.TMPCREDITOTASAINTERES : Y.DELIM : Y.TMPCREDITOTASAMORAINTERES : Y.DELIM
    Y.INS.VALUES := Y.TMPCREDITOCODIGOEJECUTIVO : Y.DELIM: Y.TMPCREDITOFORMAPAGO : Y.DELIM : Y.TMPCREDITOFECHAULTIMOPAGO : Y.DELIM
    Y.INS.VALUES := Y.TMPCREDITOMONTOULTIMOPAGO : Y.DELIM : Y.TMPCREDITOFECHAPROXIMOPAGO : Y.DELIM : Y.TMPCREDITOTIPOCALIFICACION : Y.DELIM
    Y.INS.VALUES := Y.TMPCREDITOFECHAENTRADACOBRO : Y.DELIM: Y.TMPCREDITOCOBROJUDICIAL : Y.DELIM : Y.TMPCREDITOCODIGOVISTACREDITO : Y.DELIM
    Y.INS.VALUES := Y.TMPCREDITOMORAMES01 : Y.DELIM : Y.TMPCREDITOMORAMES02 : Y.DELIM : Y.TMPCREDITOMORAMES03 : Y.DELIM : Y.TMPCREDITOMORAMES04 : Y.DELIM
    Y.INS.VALUES := Y.TMPCREDITOMORAMES05 : Y.DELIM : Y.TMPCREDITOMORAMES06 : Y.DELIM: Y.TMPCREDITOMORAMES07 : Y.DELIM: Y.TMPCREDITOMORAMES08 : Y.DELIM
    Y.INS.VALUES := Y.TMPCREDITOMORAMES09 : Y.DELIM : Y.TMPCREDITOMORAMES10 : Y.DELIM : Y.TMPCREDITOMORAMES11 : Y.DELIM : Y.TMPCREDITOMORAMES12 : Y.DELIM
    Y.INS.VALUES := Y.TMPCREDITOFACTORMONEDACONVERSI : Y.DELIM : Y.TMPCREDITOCAMPO59 : Y.DELIM : Y.TMPCREDITOCAMPO60 : Y.DELIM
    Y.INS.VALUES := Y.TMPCREDITOGARANTIA : Y.DELIM: Y.TMPCREDITOUBICACIONGARANTIA : Y.DELIM: Y.TMPCREDITOVALORGARANTIA : Y.DELIM
    Y.INS.VALUES := Y.TMPCREDITOMONTOACTUAL : Y.DELIM : "1" : Y.DELIM : TRIM(Y.TMPCREDITOCAMPO50,"","B")
*    Y.INS.VALUES := ")"

*    R.REDO.COL.QUEUE = Y.INS.STMT  : " " : Y.INS.VALUES
    R.REDO.COL.QUEUE = Y.INS.VALUES
    Y.CREDIT<-1> = R.REDO.COL.QUEUE : @VM : Y.PRODUCT.GROUP

RETURN

*-----------------------------------------------------------------------------
PAYMENT.SCHEDULE.DETAILS:
*-----------------------------------------------------------------------------
    DUE.DATES = ''  ;* Holds the list of Schedule due dates
    DUE.TYPES = ''  ;* Holds the list of Payment Types for the above dates
    DUE.TYPE.AMTS = ''        ;* Holds the Payment Type amounts
    DUE.PROPS = ''  ;* Holds the Properties due for the above type
    DUE.PROP.AMTS = ''        ;* Holds the Property Amounts for the Properties above
    DUE.OUTS = ''   ;* Oustanding Bal for the date
    DUE.METHODS = ""
    TOT.PAYMENT = ''
    DATE.REQD = ''
    CYCLE.DATE = ''
    SIM.REF = ''

    Y.TMPCREDITOCANTIDADCUOTASTOTAL = 0
    Y.TMPCREDITOCUOTASCREADAS = 0
    Y.TMPCREDITOMONTOMENSUALIDAD = ""
    Y.MEN.FLAG = @FALSE       ;*PACS00169639

    CALL AA.SCHEDULE.PROJECTOR(AA.ID, SIM.REF, "",CYCLE.DATE, TOT.PAYMENT, DUE.DATES, DUE.DEFER.DATES, DUE.TYPES, DUE.METHODS, DUE.TYPE.AMTS, DUE.PROPS, DUE.PROP.AMTS, DUE.OUTS)   ;* Routine to Project complete schedules
    Y.TMPCREDITOCANTIDADCUOTASTOTAL = DCOUNT(DUE.DATES,@FM)

    IF Y.TMPCREDITOCANTIDADCUOTASTOTAL GT 0 THEN
        I.VAR = 1
        LOOP WHILE I.VAR LE Y.TMPCREDITOCANTIDADCUOTASTOTAL
            IF DUE.DATES<I.VAR> LE Y.PROCESS.DATE THEN
                Y.TMPCREDITOCUOTASCREADAS += 1
            END
            IF Y.MEN.FLAG EQ @FALSE AND DUE.DATES<I.VAR> GT Y.PROCESS.DATE THEN
                Y.TMPCREDITOMONTOMENSUALIDAD  = 0 ;* Could be we are running the enquiry after its maturity date, then there is no next payment
                DCNT = I.VAR
                TOT.PAY.TYPE = DCOUNT(DUE.TYPES<DCNT>,@VM)
                GOSUB GET.ALL.PAYMENTS.DETAIL
                Y.TMPCREDITOMONTOMENSUALIDAD =  TOT.DUE.PAYM
                Y.TMPCREDITOFECHAPROXIMOPAGO =  DUE.DATES<DCNT>
                Y.MEN.FLAG = @TRUE      ;*PACS00169639
            END
            I.VAR += 1
        REPEAT
    END

RETURN
*------------------------------------------------------
GET.ALL.PAYMENTS.DETAIL:
*------------------------------------------------------

    PAY.CNT = 1
    LOOP WHILE PAY.CNT LE TOT.PAY.TYPE
        GOSUB GET.PAYMENT.DETAIL
        PAY.CNT += 1
    REPEAT
RETURN
*------------------------------------------------------
GET.PAYMENT.DETAIL:
*------------------------------------------------------
    PROP.LIST = DUE.PROPS<DCNT,PAY.CNT>
    PROP.LIST = RAISE(PROP.LIST)
    CALL AA.GET.PROPERTY.CLASS(PROP.LIST,PROP.CLS.LIST)
    TOT.PROP = DCOUNT(PROP.LIST,@VM)
    PROP.CNT = 1
    LOOP WHILE PROP.CNT LE TOT.PROP
        PROP.AMT = DUE.PROP.AMTS<DCNT,PAY.CNT,PROP.CNT>
        IF DUE.METHODS<DCNT,PAY.CNT,PROP.CNT> EQ 'DUE' THEN
            TOT.DUE.PAYM += PROP.AMT
        END
        PROP.CNT += 1
    REPEAT
RETURN
*** </region>
*------------------------------------------------------
GET.PERIOD.BALANCES:
*------------------------------------------------------
    Y.TMPCREDITOMONTOCAPITALVIGENTE = 0
    Y.AA.BALANCE.LIST = C.AA.PERIOD.BALANCES.TYPE
    Y.OUT.AA.AMOUNT.LIST = ""
*CALL APAP.TAM.REDO.S.GET.PERIOD.AMTS(Y.ACCOUNT.ID,Y.PROCESS.DATE, Y.PROCESS.DATE, Y.AA.BALANCE.LIST, Y.OUT.AA.AMOUNT.LIST)
    CALL APAP.TAM.redoSGetPeriodAmts(Y.ACCOUNT.ID,Y.PROCESS.DATE, Y.PROCESS.DATE, Y.AA.BALANCE.LIST, Y.OUT.AA.AMOUNT.LIST);* R22 Manual conversion - CALL method format changed
    Y.TMPCREDITOMONTOCAPITALVIGENTE = Y.OUT.AA.AMOUNT.LIST<1,1>       ;* Current Outstanding - VIGENTE
    Y.TMPCREDITOMONTOCAPITALMOROSO = Y.OUT.AA.AMOUNT.LIST<2,1>        ;* Age Outstanding - MOROSO
    Y.TMPCREDITOMONTOCAPITALVENCIDO = Y.OUT.AA.AMOUNT.LIST<3,1>       ;* Due Outstanding - VENCIDO
    Y.TMPCREDITOMONTOINTERESMOROSO = Y.OUT.AA.AMOUNT.LIST<4,1>        ;* Age Interest    - MOROSO
    Y.TMPCREDITOMONTOINTERESVIGENTE = Y.OUT.AA.AMOUNT.LIST<5,1>       ;* Acc Interest    - VIGENTE
    Y.TMPCREDITOMONTOINTERESVENCIDO = Y.OUT.AA.AMOUNT.LIST<6,1>       ;* Due Interest    - VENCIDO
    Y.ACCPENAL.INT                  = Y.OUT.AA.AMOUNT.LIST<7,1>
RETURN

*-----------------------------------------------------------------------------
* In AA.ARRANGEMENT, search by arrangement ID:
* 1. For every arrangement, fetch value from LINKED.APPL.ID field, 2. Go to EB.CONTRACT.BALANCES, and search by ID, using the value fetched in step 1,  3. Fetch absolute value from OPEN.BALANCE field from the multivalue set that starts in TYPE.SYSDATE field and ends in CURR.ASSET.TYPE field, when TYPE.SYSDATE field is equal to CURACCOUNT,* 4. Fetch absolute value from CREDIT.MVMT field and from the multivalue set that starts in TYPE.SYSDATE field and ends in CURR.ASSET.TYPE field, when TYPE.SYSDATE field contains CURACCOUNT,* 5. Fetch value from DEBIT.MVMT field from the multivalue set that starts in TYPE.SYSDATE field and ends in CURR.ASSET.TYPE field, when TYPE.SYSDATE field contains CURACCOUNT, which es closest to TODAY's date,* 6. Add values from step 3 and 4, substract the value from step 5, and add the value from TMPCREDITOMONTOMOROSO field for that arrangement, and display the total amount here.
GET.TOTAL.AMOUNT:
*-----------------------------------------------------------------------------
    Y.TYPE.SYSDATE = "CURACCOUNT"
    Y.TMPCREDITOMONTODEUDATOTAL = 0
    LOCATE Y.TYPE.SYSDATE IN R.EB.CONTRACT.BALANCES<ECB.TYPE.SYSDATE,1> SETTING Y.POS.VM THEN
        Y.TMPCREDITOMONTODEUDATOTAL = ABS(SUM(R.EB.CONTRACT.BALANCES<ECB.OPEN.BALANCE,Y.POS.VM>))
    END
    GOSUB GET.PENALTIES
    Y.TMPCREDITOMONTODEUDATOTAL =  Y.TMPCREDITOMONTODEUDATOTAL + ABS(Y.ECB.CREDIT.MVT) - Y.ECB.DEBIT.MVT + Y.TMPCREDITOMONTOMOROSO
RETURN

**-----------------------------------------------------------------------------
GET.PENALTIES:
*-----------------------------------------------------------------------------
    Y.PENALTY.AMOUNT = 0
    Y.TYPE.SYSDATE.LIST = R.EB.CONTRACT.BALANCES<ECB.TYPE.SYSDATE>
    Y.TOTAL = DCOUNT(Y.TYPE.SYSDATE.LIST, @VM)
    Y.ECB.CREDIT.MVT = 0
    Y.ECB.DEBIT.MVT = 0
    I.VAR = 1
    LOOP WHILE I.VAR LE Y.TOTAL
        Y.TYPE.SYSDATE.CURR = Y.TYPE.SYSDATE.LIST<1,I.VAR>
        IF Y.TYPE.SYSDATE.CURR["-",1,1] EQ Y.TYPE.SYSDATE THEN
            Y.DATE = Y.TYPE.SYSDATE.CURR["-",2,1]
            IF Y.DATE NE "" AND Y.DATE LE Y.PROCESS.DATE THEN
                Y.ECB.CREDIT.MVT = R.EB.CONTRACT.BALANCES<ECB.CREDIT.MVMT,I.VAR,1>
                Y.ECB.DEBIT.MVT = R.EB.CONTRACT.BALANCES<ECB.DEBIT.MVMT,I.VAR,1>
                Y.PENALTY.AMOUNT = R.EB.CONTRACT.BALANCES<ECB.CREDIT.MVMT,I.VAR,1> + R.EB.CONTRACT.BALANCES<ECB.DEBIT.MVMT,I.VAR,1>
            END
        END
        I.VAR += 1
    REPEAT
RETURN

*-----------------------------------------------------------------------------
GET.HISTORY.PAYMENT:
*-----------------------------------------------------------------------------

*CALL APAP.REDOBATCH.REDO.B.COL.FF.EXT.LAST.PAY(AA.ID,Y.TOTAL.CUOTAS,Y.LASTPAY.AMT, Y.LASTPAY.DAT,Y.NEXTPAY.AMT,Y.NEXTPAY.DATE,Y.AL.PAID.BILLS.CNT)
    CALL APAP.REDOBATCH.redoBColFfExtLastPay(AA.ID,Y.TOTAL.CUOTAS,Y.LASTPAY.AMT, Y.LASTPAY.DAT,Y.NEXTPAY.AMT,Y.NEXTPAY.DATE,Y.AL.PAID.BILLS.CNT);* R22 Manual conversion - CALL method format changed

    Y.TMPCREDITOFECHAULTIMOPAGO=Y.LASTPAY.DAT
    Y.TMPCREDITOMONTOULTIMOPAGO=Y.LASTPAY.AMT
    Y.TMPCREDITOCANTIDADCUOTASTOTAL=Y.TOTAL.CUOTAS
    Y.TMPCREDITOMONTOMENSUALIDAD =Y.NEXTPAY.AMT
    Y.TMPCREDITOFECHAPROXIMOPAGO =Y.NEXTPAY.DATE
RETURN
*-----------------------------------------------------------------------------
PROCESS.ITEMS.DETAIL:
*-----------------------------------------------------------------------------

    P.PROCESS.ITEMS.DETAILS = ''
*CALL APAP.REDORETAIL.REDO.COL.FF.EXTRACT.CREDIT.2("PROCESS.ITEMS.DETAIL", Y.ACCOUNT.ID, AA.ID, "", "", P.PROCESS.ITEMS.DETAILS)
    CALL APAP.REDORETAIL.redoColFfExtractCredit2("PROCESS.ITEMS.DETAIL", Y.ACCOUNT.ID, AA.ID, "", "", P.PROCESS.ITEMS.DETAILS) ;* MANUAL R22 CODE CONVERSION

*    Y.TMPCREDITOMONTOSEGUROVIDAVIGEN = TRIM(P.PROCESS.ITEMS.DETAILS<1>,"","B")
*    Y.TMPCREDITOMONTOSEGUROVIDAVENCI = TRIM(P.PROCESS.ITEMS.DETAILS<2>,"","B")
*    Y.TMPCREDITOMONTOSEGUROFISICOVIG = TRIM(P.PROCESS.ITEMS.DETAILS<3>,"","B")
*    Y.TMPCREDITOMONTOSEGUROFISICOVEN = TRIM(P.PROCESS.ITEMS.DETAILS<4>,"","B")
*    Y.TMPCREDITOMONTOVIGENTEOTROS    = TRIM(P.PROCESS.ITEMS.DETAILS<5>,"","B")
    Y.TMPCREDITOMONTOMOROSOOTROS      = TRIM(P.PROCESS.ITEMS.DETAILS,"","B")

RETURN
*----------------------------------------------------------------------------
CHECK.SELECTION.CRITERIA.AA:
* Check if the current AA "commits" with filter C.AA.STA... & C.AA.PRD... are initiliazed in REDO.COL.EXTRACT.LOAD
*----------------------------------------------------------------------------
    Y.CONTINUE.AA = @FALSE

    LOCATE "ACCOUNT" IN R.AA<AA.ARR.LINKED.APPL,1> SETTING Y.POS THEN
        Y.ACCOUNT.ID =  R.AA<AA.ARR.LINKED.APPL.ID,Y.POS>
    END
    CALL F.READ(FN.ACCOUNT, Y.ACCOUNT.ID ,R.ACCT, F.ACCOUNT, YERR)

    IF R.AA<AA.ARR.ARR.STATUS> EQ 'CURRENT' AND R.ACCT<AC.POSTING.RESTRICT> EQ '75' THEN
        Y.AA.ARR.ACCT.ST='CANCELLED'
    END
    ELSE
        Y.AA.ARR.ACCT.ST=R.AA<AA.ARR.ARR.STATUS>
    END
    IF Y.AA.ARR.ACCT.ST MATCHES C.AA.STA.SELECTION THEN
        Y.CONTINUE.AA = R.AA<AA.ARR.PRODUCT.GROUP> MATCHES C.AA.PRD.SELECTION
    END

RETURN
*>>PACS00169639
*-----------------------------------------------------------------------------
TRACE.ERROR:
*-----------------------------------------------------------------------------
*    CALL OCOMO(E)
*    C.DESC = E ;   CALL TXT(C.DESC)
*    C.DESC = C.DESC : "-" : AA.ID
*    CALL APAP.TAM.REDO.R.COL.PROCESS.TRACE("EXTRACT", "20", Y.CONTADOR, "TMPCREDITO", Y.PRODUCT.GROUP, C.DESC)
*    CALL APAP.TAM.REDO.R.COL.EXTRACT.ERROR(C.DESC, "REDO.COL.EXTRACT.CREDIT",P.TABLE)
*    Y.PROCESS.FLAG.TABLE<1,5> = ""
*    GOSUB STORE.MSG.ON.QUEUE
*    PROCESS.GOAHEAD = 0
RETURN
*------------------------------------------------------------------------------
STORE.MSG.ON.QUEUE:
*------------------------------------------------------------------------------
*    Y.MSG.QUEUE.ID = ''
*    CALL ALLOCATE.UNIQUE.TIME(Y.MSG.QUEUE.ID)
*    Y.MSG.QUEUE.ID = ID.COMPANY:'.':TODAY:'.':Y.MSG.QUEUE.ID
*    R.REDO.COL.MSG.QUEUE = C.DESC
*    WRITE R.REDO.COL.MSG.QUEUE TO F.REDO.COL.MSG.QUEUE, Y.MSG.QUEUE.ID ON ERROR CALL OCOMO("NO REGISTRO EL SUCESO" : C.DESC)
RETURN
*-----------------------------------------------------------------------------
END
