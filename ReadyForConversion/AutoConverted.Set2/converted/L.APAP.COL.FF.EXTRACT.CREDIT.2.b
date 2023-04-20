*-----------------------------------------------------------------------------
* <Rating>221</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.COL.FF.EXTRACT.CREDIT.2(P.PARAGRAPH.IN, P.ACCOUNT.ID, P.AA.ID, PGET.CREDIT.GRANTED, P.GET.AA.CUSTOMER, P.PROCESS.ITEMS.DETAILS)
**-----------------------------------------------------------------------------
* Name : REDO.COLLECTOR.EXTRACT.TO.CREDIT.2
*      - paragraph GET.CREDIT.GRANTED, get TERM AMOUNT data
*      - paragraph GET.AA.CUSTOMER, get AA customer details
*      - paragraph PROCESS.ITEMS.DETAIL, get insurances amounts
*------------------------------------------------------------------------------
* @author hpasquel@temenos.com
* @stereotype subroutine
* @package REDO.COL
*
* @Parameters:
* ----------------------------------------------------------------------------
*             P.PARAGRAPH.IN            (in)  Paragraph to execute
*             P.ACCOUNT.ID              (in)  Account Id
*             P.AA.ID                   (in)  Arrangement Id
*             P.GET.CREDIT.GRANTED      (out) Out variables from GET.CREDIT.GRANTED parragraph
*             P.GET.AA.CUSTOMER         (out) Out variables from GET.AA.CUSTOMER parragraph
*             P.PROCESS.ITEMS.DETAIL    (out) Out variables from PROCESS.ITEMS.DETAIL parragraph
* ----------------------------------------------------------------------------
*             E               (out) The message error
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AA.TERM.AMOUNT
    $INSERT I_F.AA.CUSTOMER
    $INSERT I_F.COLLATERAL
    $INSERT I_F.AA.ACCOUNT.DETAILS
    $INSERT I_F.AA.BILL.DETAILS
*
    $INSERT I_L.APAP.COL.CUSTOMER.COMMON
    $INSERT TAM.BP I_REDO.COL.EXTRACT.CREDIT.COMMON

    $INSERT I_F.COLLATERAL.CODE
    $INSERT I_F.COLLATERAL.TYPE
    $INSERT I_F.CURRENCY
*
*-----------------------------------------------------------------------------

    Y.PROCESS.DATE = C.REPORT.PROCESS.DATE        ;* R.DATES(EB.DAT.LAST.WORKING.DAY) ;* TODAY
    GOSUB INIT
    BEGIN CASE
    CASE P.PARAGRAPH.IN EQ "GET.CREDIT.GRANTED"
        GOSUB GET.CREDIT.GRANTED
    CASE P.PARAGRAPH.IN EQ "GET.AA.CUSTOMER"
        GOSUB GET.AA.CUSTOMER
    CASE P.PARAGRAPH.IN EQ "PROCESS.ITEMS.DETAIL"
        GOSUB PROCESS.ITEMS.DETAIL
    CASE 1
        E = "PARAGRAPH & NOT IMPLEMENTED" : FM : P.PARAGRAPH.IN
    END CASE
    RETURN

*-----------------------------------------------------------------------------
GET.CREDIT.GRANTED:
*-----------------------------------------------------------------------------

    Y.TMPCREDITOMONTOAPERTURA = ""
*<< PACS00169639
    idPropertyClass = "TERM.AMOUNT"
    GOSUB ARR.CONDITIONS
    IF returnError THEN
        E = returnError
        RETURN
    END
    R.AA.TERM = RAISE(returnConditions)
*>> PACS00169639

*    Y.TMPCREDITOMONTOAPERTURA = R.AA.TERM<AA.AMT.AMOUNT>
    P.GET.CREDIT.GRANTED = ""
    P.GET.CREDIT.GRANTED<1> = R.AA.TERM<AA.AMT.AMOUNT>

    Y.L.AA.COL = R.AA.TERM<AA.AMT.LOCAL.REF,Y.AA.TERM.L.AA.COL,1>

    IF Y.L.AA.COL NE "" THEN
        CALL F.READ(FN.COLLATERAL, Y.L.AA.COL, R.COLLATERAL, F.COLLATERAL, YERR)
        IF YERR THEN
            E = yRecordNotFound : FM : Y.L.AA.COL : VM : "F.COLLATERAL"
            RETURN
        END
        P.GET.CREDIT.GRANTED<2> = Y.L.AA.COL
        P.GET.CREDIT.GRANTED<3> = CHANGE(R.COLLATERAL<COLL.DESCRIPTION>, VM, " ")[1,500]
        P.GET.CREDIT.GRANTED<4> = R.COLLATERAL<COLL.COUNTRY>
        P.GET.CREDIT.GRANTED<5> = R.COLLATERAL<COLL.NOMINAL.VALUE>

        YCOL.TYP = ''; YCOL.TYP = R.COLLATERAL<COLL.COLLATERAL.TYPE>
        IF NOT(YCOL.TYP) THEN
            RETURN
        END
        P.GESTIPOGARANTIAS<1> = FMT(YCOL.TYP,"R%6")

        ERR.COLLATERAL.TYPE = ''; R.COLLATERAL.TYPE = ''
        CALL F.READ(FN.COLLATERAL.TYPE,YCOL.TYP,R.COLLATERAL.TYPE,F.COLLATERAL.TYPE,ERR.COLLATERAL.TYPE)
        P.GESTIPOGARANTIAS<2> = R.COLLATERAL.TYPE<COLL.CODE.DESCRIPTION,1>[1,60]
        P.GESTIPOGARANTIAS<3> = R.COLLATERAL<COLL.LOCAL.REF,L.COL.GUAR.TYPE.POSN>[1,60]

        YL.AA.COL.TP = ''; YL.AA.COL.TP = CHANGE(Y.L.AA.COL,'.','')
        P.GESGARANTIAS<1> = FMT(YL.AA.COL.TP,"R%18")
        ERR.COLLATERAL.CODE = ''; R.COLLATERAL.CODE = ''
        YCOL.CDE = R.COLLATERAL<COLL.COLLATERAL.CODE>
        CALL F.READ(FN.COLLATERAL.CODE,YCOL.CDE,R.COLLATERAL.CODE,F.COLLATERAL.CODE,ERR.COLLATERAL.CODE)
        P.GESGARANTIAS<2> = R.COLLATERAL.CODE<COLL.TYPE.DESCRIPTION,1>[1,1000]
        P.GESGARANTIAS<3> = FMT(YCOL.TYP,"R%6")
        P.GESGARANTIAS<4> = FMT(R.COLLATERAL<COLL.EXECUTION.VALUE>,"R2%18")
        P.GESGARANTIAS<5> = FMT(R.COLLATERAL<COLL.NOMINAL.VALUE>,"R2%18")
        YCURR = ''; YCURR = R.COLLATERAL<COLL.CURRENCY>
        RV.CURRENCY = ''; CURR.ERR = ''
        CALL CACHE.READ(FN.CURRENCY,YCURR,RV.CURRENCY,CURR.ERR)
        P.GESGARANTIAS<6> = FMT(RV.CURRENCY<EB.CUR.NUMERIC.CCY.CODE>,"R%6")
        P.GESGARANTIAS<7> = R.COLLATERAL<COLL.LOCAL.REF,L.COL.VAL.DATE.POSN>
        P.GESGARANTIAS<8> = FMT(R.COLLATERAL<COLL.LOCAL.REF,L.COL.TOT.VALUA.POSN>,"R2%18")

        YEXP.DATE = ''; YEXP.DATE = R.COLLATERAL<COLL.EXPIRY.DATE>
        IF (YEXP.DATE AND YEXP.DATE GT TODAY) OR YEXP.DATE EQ '' THEN
            P.GESGARANTIAS<10> = "V"
            P.GESGARANTIAS<11> = "Vigente"
        END ELSE
            P.GESGARANTIAS<10> = "C"
            P.GESGARANTIAS<11> = "Cancelado"
        END

        YPROP.DESC = ''; YPROP.DESC = R.COLLATERAL<COLL.LOCAL.REF,L.COL.PROP.DESC.POSN>
        IF YPROP.DESC THEN
            CHANGE VM TO ' ' IN YPROP.DESC
            CHANGE SM TO ' ' IN YPROP.DESC
            CHANGE '#' TO ' ' IN YPROP.DESC
            CHANGE '~' TO ' ' IN YPROP.DESC
        END

        P.GESGARANTIAS<9> = YPROP.DESC[1,500]

        P.GESCREDITOSGARANTIAS<1> = "000001"
        P.GESCREDITOSGARANTIAS<2> = "000001"
        P.GESCREDITOSGARANTIAS<3> = "BPR"
        P.GESCREDITOSGARANTIAS<7> = FMT(YL.AA.COL.TP,"R%18")

        PGET.CREDIT.GRANTED = P.GET.CREDIT.GRANTED:"#":P.GESGARANTIAS:"#":P.GESCREDITOSGARANTIAS:"#":P.GESTIPOGARANTIAS
    END

    RETURN
*-----------------------------------------------------------------------------
GET.AA.CUSTOMER:
*-----------------------------------------------------------------------------
*<< PACS00169639
    idPropertyClass = "CUSTOMER"
    GOSUB ARR.CONDITIONS
    IF returnError THEN
        E = returnError
        RETURN
    END
    R.AA.CUSTOMER = RAISE(returnConditions)
*>> PACS00169639
    P.GET.AA.CUSTOMER = R.AA.CUSTOMER<AA.CUS.LOCAL.REF,Y.AA.CUS.L.AA.AFF.COM>[1,400]
    P.GET.AA.CUSTOMER<2> = R.AA.CUSTOMER<AA.CUS.LOCAL.REF,Y.AA.CUS.L.AA.AFF.COM>[1,400]
    P.GET.AA.CUSTOMER<3> = R.AA.CUSTOMER<AA.CUS.ROLE>
    P.GET.AA.CUSTOMER<4> = R.AA.CUSTOMER<AA.CUS.OTHER.PARTY>
    P.GET.AA.CUSTOMER<5> = R.AA.CUSTOMER<AA.CUS.OWNER>
    RETURN

*-----------------------------------------------------------------------------
*    Get AA amount details by item
*             TMPCREDITOMONTOSEGUROVIDAVIGEN
*             TMPCREDITOMONTOSEGUROVIDAVENCI
*             TMPCREDITOMONTOSEGUROFISICOVIG
*             TMPCREDITOMONTOSEGUROFISICOVEN
*             TMPCREDITOMONTOVIGENTEOTROS
*             TMPCREDITOMONTOMOROSOOTROS
PROCESS.ITEMS.DETAIL:
*-----------------------------------------------------------------------------

    Y.AA.BALANCE.LIST = ""
    Y.AA.BALANCE.LIST<1> = "ACCSEGPROPIEDADPR" : VM : "ACCSEGVIDAPR"  ;*TMPCREDITOMONTOSEGUROVIDAVIGEN
    Y.AA.BALANCE.LIST<2> = "DUESEGPROPIEDADPR" : VM : "DUESEGVIDAPR"  ;*TMPCREDITOMONTOSEGUROVIDAVENCI
    Y.AA.BALANCE.LIST<3> = "ACC" : CHANGE(C.AA.SEGUROS.CHARGES.LIST, FM, VM : "ACC")      ;*TMPCREDITOMONTOSEGUROFISICOVIG
    Y.AA.BALANCE.LIST<4> = "DUE" : CHANGE(C.AA.SEGUROS.CHARGES.LIST, FM, VM : "DUE")      ;*TMPCREDITOMONTOSEGUROFISICOVEN
    Y.AA.BALANCE.LIST<5> = "ACCPRADMSEGENDOSO"    ;*TMPCREDITOMONTOVIGENTEOTROS = ACCPRADMSEGENDOSO + TMPCREDITOMONTOSEGUROVIDAVIGEN
    Y.AA.BALANCE.LIST<6> = "GRCSEGPROPIEDADPR" : VM : "DELSEGPROPIEDADPR" : VM : "NABSEGPROPIEDADPR" : VM
    Y.AA.BALANCE.LIST<6> := "GRCSEGVIDAPR" : VM : "DELSEGVIDAPR" : VM : "NABSEGVIDAPR" : VM
    Y.AA.BALANCE.LIST<6> := "GRCPRADMSEGENDOSO" : VM : "DELPRADMSEGENDOSO" : VM : "NABPRADMSEGENDOSO"

*    CALL REDO.S.GET.PERIOD.AMTS(P.ACCOUNT.ID, Y.PROCESS.DATE, Y.PROCESS.DATE, Y.AA.BALANCE.LIST, Y.OUT.AA.AMOUNT.LIST)

*    P.PROCESS.ITEMS.DETAILS    = Y.OUT.AA.AMOUNT.LIST<1,1>
*    P.PROCESS.ITEMS.DETAILS<2> = Y.OUT.AA.AMOUNT.LIST<2,1>
*    P.PROCESS.ITEMS.DETAILS<3> = Y.OUT.AA.AMOUNT.LIST<3,1>
*    P.PROCESS.ITEMS.DETAILS<4> = Y.OUT.AA.AMOUNT.LIST<4,1>
*    P.PROCESS.ITEMS.DETAILS<5> = Y.OUT.AA.AMOUNT.LIST<5,1>
*    P.PROCESS.ITEMS.DETAILS<6> = Y.OUT.AA.AMOUNT.LIST<6,1> + Y.OUT.AA.AMOUNT.LIST<1,1>

*    FOR I=1 TO 6
*        IF P.PROCESS.ITEMS.DETAILS<I> EQ "" THEN
*            P.PROCESS.ITEMS.DETAILS<I> = 0
*        END
*    NEXT I

    CALL F.READ(FN.AA.DETAILS,P.AA.ID,R.AA.DETAILS,F.AA.DETAILS,Y.AA.DET.ERR)

    Y.AA.AD.BILL.ID=R.AA.DETAILS<AA.AD.BILL.ID>
    Y.AA.BILL.TYPE =R.AA.DETAILS<AA.AD.BILL.TYPE>
    Y.AA.SET.STATUS=R.AA.DETAILS<AA.AD.SET.STATUS>

    CHANGE SM TO VM IN Y.AA.AD.BILL.ID
    CHANGE SM TO VM IN Y.AA.BILL.TYPE
    CHANGE SM TO VM IN Y.AA.SET.STATUS

    Y.LOOP.AA.AD.TOT=DCOUNT(Y.AA.AD.BILL.ID,VM)
    Y.LOOP.AA.AD.CNT=1
    LOOP
    WHILE Y.LOOP.AA.AD.CNT LE Y.LOOP.AA.AD.TOT
        IF Y.AA.SET.STATUS<1,Y.LOOP.AA.AD.CNT> EQ 'UNPAID' THEN
            CALL F.READ(FN.AA.BILL,Y.AA.AD.BILL.ID<1,Y.LOOP.AA.AD.CNT>,R.AA.BILL.DET,F.AA.BILL,Y.AA.BI.ERR)
            IF Y.AA.BILL.TYPE<1,Y.LOOP.AA.AD.CNT> EQ 'ACT.CHARGE' THEN
                Y.TOT.OT.CH+=R.AA.BILL.DET<AA.BD.OS.TOTAL.AMOUNT>
            END
            ELSE
                Y.PROP.LIST=R.AA.BILL.DET<AA.BD.PROPERTY>
                Y.PROP.OS.AMT=R.AA.BILL.DET<AA.BD.OS.PROP.AMOUNT>
                GOSUB ADD.PROP.AMT
            END
        END
        Y.LOOP.AA.AD.CNT++
    REPEAT
    P.PROCESS.ITEMS.DETAILS=Y.TOT.OT.CH
    RETURN
*-------------
ADD.PROP.AMT:
*-------------

    Y.PROP.LIST.CNT=1
    Y.PROP.LIST.TOT=DCOUNT(Y.PROP.LIST,VM)
    LOOP
    WHILE Y.PROP.LIST.CNT LE Y.PROP.LIST.TOT
        IF Y.PROP.LIST<1,Y.PROP.LIST.CNT> EQ 'PENALTINT' OR Y.PROP.LIST<1,Y.PROP.LIST.CNT> EQ 'PRMORA' OR Y.PROP.LIST<1,Y.PROP.LIST.CNT> EQ 'PRINCIPALINT' OR Y.PROP.LIST<1,Y.PROP.LIST.CNT> EQ 'ACCOUNT' ELSE
            Y.TOT.OT.CH+=Y.PROP.OS.AMT<1,Y.PROP.LIST.CNT>
        END
        Y.PROP.LIST.CNT++
    REPEAT
    RETURN
*-----------------------------------------------------------------------------
ARR.CONDITIONS:
*-----------------------------------------------------------------------------
    ArrangementID = P.AA.ID ; idProperty = ''; effectiveDate = Y.PROCESS.DATE; returnIds = ''; R.CONDITION =''; returnConditions = ''; returnError = ''
    CALL AA.GET.ARRANGEMENT.CONDITIONS(ArrangementID, idPropertyClass, idProperty, effectiveDate, returnIds, returnConditions, returnError)
    RETURN
*-----------------------------------------------------------------------------
INIT:
*****
    FN.CURRENCY = 'F.CURRENCY'; FV.CURRENCY = ''
    CALL OPF(FN.CURRENCY,FV.CURRENCY)
    RETURN

END
