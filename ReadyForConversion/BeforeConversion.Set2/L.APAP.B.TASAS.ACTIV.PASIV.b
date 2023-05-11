*-----------------------------------------------------------------------------
* <Rating>1859</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.B.TASAS.ACTIV.PASIV(REC.ID)

*
* Client Name   : APAP
* Develop By    : Ashokkumar
* Description   : The routine to generate the Activasa and Pasivas report AR010.
*

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_BATCH.FILES
    $INSERT I_TSA.COMMON
    $INSERT I_F.ACCOUNT
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.CUSTOMER
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.AZ.PRODUCT.PARAMETER
    $INSERT I_F.BASIC.INTEREST
    $INSERT I_F.GROUP.DATE
    $INSERT I_F.ACCOUNT.CREDIT.INT
    $INSERT I_F.PERIODIC.INTEREST
    $INSERT I_F.GROUP.CREDIT.INT
    $INSERT I_DAS.COMMON
    $INSERT I_DAS.BASIC.INTEREST
    $INSERT I_F.AA.INTEREST
    $INSERT I_F.AA.TERM.AMOUNT
    $INSERT I_F.COLLATERAL
    $INSERT I_F.AA.ACTIVITY.HISTORY
    $INSERT I_F.AA.ACCOUNT
    $INSERT I_F.AA.CUSTOMER
    $INSERT I_F.AA.OVERDUE
    $INSERT I_L.APAP.B.TASAS.ACTIV.PASIV.COMMON
    $INSERT I_F.REDO.APAP.INSTIT.FINANC.PARAM


    GOSUB MAIN.PROCESS
    RETURN

MAIN.PROCESS:
*************
    YGRP.ARRY = ''; YYR.MTH = 12
    Y.SKIP.RECORD.FLAG = ''
    BEGIN CASE
    CASE CONTROL.LIST<1,1> EQ "SELECT.AZ"
        CALL F.READ(FN.AZ.ACCOUNT,REC.ID,R.AZ.ACCOUNT,F.AZ.ACCOUNT,AZ.ACCOUNT.ERR)
        GOSUB READ.ACCOUNT.DET
        CUS.ID = R.ACCOUNT<AC.CUSTOMER>
        GOSUB PROCESS.AZ

    CASE CONTROL.LIST<1,1> EQ "SELECT.AC"
        GOSUB READ.ACCOUNT.DET
        CUS.ID = R.ACCOUNT<AC.CUSTOMER>
        IF NOT(CUS.ID) THEN RETURN
        CUS.OPENING.DATE = R.ACCOUNT<AC.OPENING.DATE>
        CCY.VAL = R.ACCOUNT<AC.CURRENCY>
        YCAT.ID = R.ACCOUNT<AC.CATEGORY>
        AC.OPEN.BAL = R.ACCOUNT<AC.OPEN.CLEARED.BAL>
        IF (AC.OPEN.BAL EQ 0 OR AC.OPEN.BAL EQ 0.00 OR AC.OPEN.BAL EQ '') THEN
            RETURN
        END
        LOCATE YCAT.ID IN CAT.LIST3<1> SETTING CAT.POSN ELSE
            RETURN
        END
        GOSUB PROCESS.AC

    CASE CONTROL.LIST<1,1> EQ "SELECT.AA"
        YTP.RECID = ''; R.AA.ARRANGEMENT = ''; AA.ARRANGEMENT.ERR = ''; STRT.DATE = ''; AMT.VAL = 0; YACCT.ID = ''
        CALL F.READ(FN.AA.ARRANGEMENT,REC.ID,R.AA.ARRANGEMENT,F.AA.ARRANGEMENT,AA.ARRANGEMENT.ERR)
        GOSUB PROCESS.AA

    END CASE
    IF Y.SKIP.RECORD.FLAG NE 'Y' THEN
        IF YGRP.ARRY THEN
            CALL F.WRITE(FN.DR.REG.PASIVAS.ACTIV, YTP.REC, YGRP.ARRY)
        END
    END
    RETURN

PROCESS.AC:
***********
    GOSUB GET.CUSTOMER.DET
    INT.RATE = ''; COMI = ''
    CALL DR.REP.AC.INT.CALC(REC.ID,R.ACCOUNT,INT.RATE)
    CUS.OPENING.DATE1 = CUS.OPENING.DATE[7,2]:'/':CUS.OPENING.DATE[5,2]:'/':CUS.OPENING.DATE[1,4]

    YVAL1 = Y.CONDAC.DIS.ARR<1,1>
    YVAL2 = Y.TIPOAC.DIS.ARR<1,1>
    YVAL25 = Y.GRPSECTAC.DIS.ARR<1,1>
    YVAL3 = Y.SUBSAC.DIS.ARR<1,1>
    YVAL4 = Y.GRPCONTAC.DIS.ARR<1,1>
    YVAL5 = Y.CONTRAAC.DIS.ARR<1,1>
    YVAL6 = Y.IDENCONTAC.DIS.ARR<1,1>
    YVAL7 = Y.TIPOIDENAC.DIS.ARR<1,1>
    YVAL8 = Y.PLAZAEXTAC.DIS.ARR<1,1>
    YVAL9 = Y.CONDIPLAZAC.DIS.ARR<1,1>
    YVAL10 = Y.TASAINT.PREFAC.DIS.ARR<1,1>
    YVAL11 = Y.TASAFIJAAC.DIS.ARR<1,1>
    YVAL12 = Y.TASAREFAC.DIS.ARR<1,1>
    YVAL13 = Y.PERIODTASAAC.DIS.ARR<1,1>
    YVAL14 = Y.COSTOAC.DIS.ARR<1,1>
    YVAL15 = Y.CODIGOPROCAC.DIS.ARR<1,1>
    YVAL16 = Y.CODIGOMUNAC.DIS.ARR<1,1>
    YVAL17 = Y.UBICACIONAC.DIS.ARR<1,1>
    YVAL18 = Y.METROVIVIENAC.DIS.ARR<1,1>
    YVAL19 = Y.VALORVIVIENAC.DIS.ARR<1,1>
    YVAL20 = Y.ESTADOVIVIENAC.DIS.ARR<1,1>
    YVAL21 = Y.EDIFICACIONAC.DIS.ARR<1,1>
    YVAL22 = Y.CANTIDAOPERACIONESAC.DIS.ARR<1,1>
    YVAL23 = Y.CONDIGINSTIAC.DIS.ARR<1,1>
    YVAL24 = Y.CODNUMINSTAC.DIS.ARR<1,1>

*CAMPO #20: PERIODICIDAD
    Y.AC.PERIODICIDAD = 'NA'

    IF CUS.OPENING.DATE EQ LAST.WORK.DAY THEN
        YTP.REC = '225':CCY.VAL:INT.RATE:'-':REC.ID
        YGRP.ARRY = FMT(REC.ID,'L#25'):"|":FMT(CUS.OPENING.DATE1,'L#10'):"|":FMT(YVAL1,'L#2'):"|":FMT(YVAL2,'R%1'):"|":FMT(YVAL25,'R%2'):"|":FMT(YVAL3,'R%2'):"|"
        YGRP.ARRY:= FMT(YVAL4,'L#3'):"|":FMT(YVAL5,'L#100'):"|":FMT(YVAL6,'L#15'):"|":FMT(YVAL7,'R%1'):"|":FMT(CUS.OPENING.DATE1,'L#10'):"|":FMT(CUS.OPENING.DATE1,'L#10'):"|":FMT(CCY.VAL,'L#3'):"|"
        YGRP.ARRY:= FMT(AC.OPEN.BAL,'R2%17'):"|":FMT(YVAL8,'R%6'):"|":FMT(YVAL9,'R%2'):"|":FMT(INT.RATE,'R2%5'):"|":FMT(YVAL10,'L#2'):"|":FMT(YVAL11,'L#2'):"|":FMT(Y.AC.PERIODICIDAD,'L#2'):"|"
        YGRP.ARRY:= FMT(YVAL12,'R%1'):"|":FMT(YVAL13,'R%1'):"|":FMT(YVAL14,'R2%5'):"|":FMT(YVAL15,'R%2'):"|":FMT(YVAL16,'R%2'):"|":FMT(YVAL17,'L#100'):"|"
        YGRP.ARRY:= FMT(YVAL18,'R2%10'):"|":FMT(YVAL19,'R2%17'):"|":FMT(YVAL20,'R%1'):"|":FMT(YVAL21,'R%1'):"|":FMT(YVAL22,'R%8'):"|":FMT(YVAL23,'L#2'):"|":FMT(YVAL24,'R%3'):"|"
    END ELSE
        YTP.REC = '225ACDUMMY':CCY.VAL:INT.RATE:'-':REC.ID
        YGRP.ARRY = FMT(REC.ID,'L#25'):"|":FMT(CUS.OPENING.DATE1,'L#10'):"|":FMT(YVAL1,'L#2'):"|":FMT(YVAL2,'R%1'):"|":FMT(YVAL25,'R%2'):"|":FMT(YVAL3,'R%2'):"|"
        YGRP.ARRY:= FMT(YVAL4,'L#3'):"|":FMT(YVAL5,'L#100'):"|":FMT(YVAL6,'L#15'):"|":FMT(YVAL7,'R%1'):"|":FMT(CUS.OPENING.DATE1,'L#10'):"|":FMT(CUS.OPENING.DATE1,'L#10'):"|":FMT(CCY.VAL,'L#3'):"|"
        YGRP.ARRY:= FMT(AC.OPEN.BAL,'R2%17'):"|":FMT(YVAL8,'R%6'):"|":FMT(YVAL9,'R%2'):"|":FMT(INT.RATE,'R2%5'):"|":FMT(YVAL10,'L#2'):"|":FMT(YVAL11,'L#2'):"|":FMT(Y.AC.PERIODICIDAD,'L#2'):"|"
        YGRP.ARRY:= FMT(YVAL12,'R%1'):"|":FMT(YVAL13,'R%1'):"|":FMT(YVAL14,'R2%5'):"|":FMT(YVAL15,'R%2'):"|":FMT(YVAL16,'R%2'):"|":FMT(YVAL17,'L#100'):"|"
        YGRP.ARRY:= FMT(YVAL18,'R2%10'):"|":FMT(YVAL19,'R2%17'):"|":FMT(YVAL20,'R%1'):"|":FMT(YVAL21,'R%1'):"|":FMT(YVAL22,'R%8'):"|":FMT(YVAL23,'L#2'):"|":FMT(YVAL24,'R%3'):"|"
    END
    RETURN

PROCESS.AZ:
***********
    AZ.OPEN.BAL = 0
    GOSUB GET.CUSTOMER.DET
    CCY.VAL = R.AZ.ACCOUNT<AZ.CURRENCY>
    VAL.DATE = R.AZ.ACCOUNT<AZ.VALUE.DATE>
    CUS.ID = R.AZ.ACCOUNT<AZ.CUSTOMER>
    YCAT.ID = R.AZ.ACCOUNT<AZ.CATEGORY>
    YCREATE.DATE = R.AZ.ACCOUNT<AZ.CREATE.DATE>
    YROLL.DTE = R.AZ.ACCOUNT<AZ.ROLLOVER.DATE>
    YMATURITY.DATE = R.AZ.ACCOUNT<AZ.MATURITY.DATE>
    IF YROLL.DTE AND YROLL.DTE GT Y.TODAY THEN
        YMATURITY.DATE = YROLL.DTE
    END
    YINTEREST.RATE = R.AZ.ACCOUNT<AZ.INTEREST.RATE>
    IF YROLL.DTE AND YROLL.DTE GT Y.TODAY THEN
        YINTEREST.RATE = R.AZ.ACCOUNT<AZ.ORIG.INTEREST.RATE>
    END

    AMT.VAL = R.AZ.ACCOUNT<AZ.PRINCIPAL>
    YREINVST = R.AZ.ACCOUNT<AZ.LOCAL.REF,L.AZ.REIVSD.INT.POSN>
    YCO.CODE = R.AZ.ACCOUNT<AZ.CO.CODE>
*locked amount
    Y.LOCK.AMT = 0
    IF NOT(R.ACCOUNT<AC.FROM.DATE>) THEN
        Y.LOCK.AMT = 0

    END ELSE

        Y.DATE.COUNT = DCOUNT(R.ACCOUNT<AC.FROM.DATE>,VM)
        Y.DATE.START = 1
        LOOP
        WHILE Y.DATE.START LE Y.DATE.COUNT
            IF R.ACCOUNT<AC.FROM.DATE,Y.DATE.START> LE TODAY THEN
                Y.LOCK.AMT += R.ACCOUNT<AC.LOCKED.AMOUNT,Y.DATE.START>
            END
            Y.DATE.START += 1
        REPEAT
    END

*AZ.OPEN.BAL = AMT.VAL + YREINVST + Y.LOCK.AMT
    AZ.OPEN.BAL = AMT.VAL + YREINVST
    YDATE = VAL.DATE
    YDATE1 = YMATURITY.DATE
    GOSUB DATE.30.CHK

** NPV calcaulation*****
    YMTH.INT = ''; YINT.NPV = ''
    Y.INTEREST.RATE = YINTEREST.RATE / 100
    YMTH.INT = Y.INTEREST.RATE / YYR.MTH
    YINT.NPV = ((1+YMTH.INT)^YYR.MTH) - 1
    YINT.NPV = ABS(YINT.NPV)
    YINT.NPV = YINT.NPV * 100
*****
    GOSUB EVAL.PROD.INT.RATE

    YVALAZ1 = Y.TIPOAZ.DIS.ARR<1,1>
    YVALAZ2 = Y.SUBSAZ.DIS.ARR<1,1>
    YVALAZ4 = Y.TASAFIJAAZ.DIS.ARR<1,1>
    YVALAZ5 = Y.TASAREFAZ.DIS.ARR<1,1>
    YVALAZ6 = Y.PERIODTASAAZ.DIS.ARR<1,1>
    YVALAZ7 = Y.UBICACIONAZ.DIS.ARR<1,1>
    YVALAZ8 = Y.METROVIVIENAZ.DIS.ARR<1,1>
    YVALAZ9 = Y.VALORVIVIENAZ.DIS.ARR<1,1>
    YVALAZ10 = Y.ESTADOVIVIENAZ.DIS.ARR<1,1>
    YVALAZ11 = Y.EDIFICACIONAZ.DIS.ARR<1,1>
    YVALAZ12 = Y.CANTIDAOPERACIONESAZ.DIS.ARR<1,1>
    YVALAZ13 = Y.CONDIGINSTIAZ.DIS.ARR<1,1>
    YVALAZ14 = Y.CODNUMINSTAZ.DIS.ARR<1,1>

    GOSUB GET.AZ.PREFRATE
    IF CUS.FAX.1 NE '' THEN
        YTAS.PREF = 'EM'
    END

    GOSUB GET.MUNIC.CORP
    BEGIN CASE
    CASE TERM.IN.DAYS GT '0' AND TERM.IN.DAYS LE '30'
        YTERM.D = 7
    CASE TERM.IN.DAYS GT '30' AND TERM.IN.DAYS LE '60'
        YTERM.D = 8
    CASE TERM.IN.DAYS GT '60' AND TERM.IN.DAYS LE '90'
*CNCN008978
        YTERM.D = 9
    CASE TERM.IN.DAYS GE '91' AND TERM.IN.DAYS LE '180'
*CNCN008978
        YTERM.D = 2
    CASE TERM.IN.DAYS GE '181' AND TERM.IN.DAYS LE '360'
*CNCN008978
        YTERM.D = 3
    CASE TERM.IN.DAYS GE '361' AND Y.YEAR LE 2
        YTERM.D = 4
    CASE Y.YEAR GT '2' AND Y.YEAR LE 5
        YTERM.D = 5
    CASE Y.YEAR GT '5'
        YTERM.D = 6
    END CASE
    yperiod.D = 'NA'
*CN008325
    BEGIN CASE
    CASE TERM.IN.DAYS LE '31'
        yperiod.D = 'M'
    CASE TERM.IN.DAYS GE '32' AND TERM.IN.DAYS LE '60'
        yperiod.D = 'B'
    CASE TERM.IN.DAYS GE '61' AND TERM.IN.DAYS LE '90'
        yperiod.D = 'T'
    CASE TERM.IN.DAYS GE '91' AND TERM.IN.DAYS LE '120'
        yperiod.D = 'C'
    CASE TERM.IN.DAYS GE '121' AND TERM.IN.DAYS LE '180'
        yperiod.D = 'S'
    CASE TERM.IN.DAYS GE '181' AND TERM.IN.DAYS LE '360'
        yperiod.D = 'A'
    CASE TERM.IN.DAYS GE '361' AND Y.YEAR LE 2
        yperiod.D = 'A2'
    CASE Y.YEAR EQ '3'
        yperiod.D = 'A3'
    CASE Y.YEAR EQ '4'
        yperiod.D = 'A4'
    CASE Y.YEAR EQ '5'
        yperiod.D = 'A5'
    CASE Y.YEAR GT '5' AND Y.YEAR LE 8
        yperiod.D = 'A6'
	CASE Y.YEAR GE 9
		yperiod.D = 'A9'
    END CASE


    IF YCREATE.DATE GE LAST.WORK.DAY AND YCREATE.DATE LE Y.TODAY THEN
        COND.OPEER = 'N'
    END

    IF YROLL.DTE GE LAST.WORK.DAY AND YROLL.DTE LE Y.TODAY THEN
        COND.OPEER = 'R'
    END

    LOCATE YL.AP.ABB.DEPO IN Y.GRPSECTAZ.VAL.ARR<1,1> SETTING TIPOY.POS THEN
        YSECTOR = Y.GRPSECTAZ.DIS.ARR<1,TIPOY.POS>
    END
    IF YCAT.ID EQ '6604' THEN
        YSECTOR = '33'
    END

    VAL.DATE = LAST.WORK.DAY
    YCREATE.DATE = VAL.DATE[7,2]:'/':VAL.DATE[5,2]:'/':VAL.DATE[1,4]
    YMATURITY.DATE = YMATURITY.DATE[7,2]:'/':YMATURITY.DATE[5,2]:'/':YMATURITY.DATE[1,4]
    YTP.REC = '2':YSECTOR:CCY.VAL:'-':REC.ID
    YGRP.ARRY = FMT(REC.ID,'L#25'):"|":FMT(YCREATE.DATE,'L#10'):"|":FMT(COND.OPEER,'L#2'):"|":FMT(YVALAZ1,'R%1'):"|":FMT(YSECTOR,'R%2'):"|":FMT(YVALAZ2,'R%2'):"|":FMT(YGRP.CONT,'L#3'):"|":FMT(YCONTRA.PARTE,'L#100'):"|"
    YGRP.ARRY := FMT(YIDENT,'L#15'):"|":FMT(YTIPO,'L#1'):"|":FMT(CUS.DATE.OF.BIRTH,'L#10'):"|":FMT(YMATURITY.DATE,'L#10'):"|":FMT(CCY.VAL,'L#3'):"|":FMT(AZ.OPEN.BAL,'R2%17'):"|"
    YGRP.ARRY := FMT(TERM.IN.DAYS,'R%6'):"|":FMT(YTERM.D,'R%2'):"|":FMT(YINTEREST.RATE,'R2%5'):"|":FMT(YTAS.PREF,'L#2'):"|":FMT(YVALAZ4,'L#2'):"|":FMT(yperiod.D,'L#2'):"|":FMT(YVALAZ5,'R%1'):"|"
    YGRP.ARRY := FMT(YVALAZ6,'R%1'):"|":FMT(YINT.NPV,'R2%5'):"|":FMT(YCOD.PROVIN,'R%2'):"|":FMT(YCOD.MUNIC,'R%2'):"|":FMT(YVALAZ7,'L#100'):"|":FMT(YVALAZ8,'R2%10'):"|"
    YGRP.ARRY := FMT(YVALAZ9,'R2%17'):"|":FMT(YVALAZ10,'R%1'):"|":FMT(YVALAZ11,'R%1'):"|":FMT(YVALAZ12,'R%8'):"|":FMT(YVALAZ13,'L#2'):"|":FMT(YVALAZ14,'R%3'):"|"
    RETURN

GET.AZ.PREFRATE:
****************
    YVM.CNT = 0 ; YTAS.PREF = ''; I = 0
    YVM.CNT = DCOUNT(Y.TASAINT.PREFAZ.VAL.ARR,VM)
    Y.VALOR.TAZA = ''; Y.VALOR.TAZA.PARAM = ''; Y.TERMINO.DIAS.PARAM = ''
    FOR I = 1 TO YVM.CNT
        YVALAZ3 = ''; YVALAZ3.DIS = ''
        YVALAZ3 = Y.TASAINT.PREFAZ.VAL.ARR<1,I>
        YVALAZ3.DIS = Y.TASAINT.PREFAZ.DIS.ARR<1,I>
        IF RIGHT(YVALAZ3,2) EQ 'GT' THEN
            YVALAZ3.LEN = LEN(YVALAZ3)
            YVALAZ3 = YVALAZ3[1,YVALAZ3.LEN-2]
            IF ((YVALAZ3 NE '' AND YVALAZ3.DIS NE '') AND TERM.IN.DAYS GT YVALAZ3) THEN
                IF YINTEREST.RATE LE YVALAZ3.DIS THEN
                    YTAS.PREF = 'N'
                END ELSE
                    YTAS.PREF = 'S'
                END
                RETURN
            END
        END

        IF ((YVALAZ3 NE '' AND YVALAZ3.DIS NE '') AND TERM.IN.DAYS LE YVALAZ3) THEN
            IF YINTEREST.RATE LE YVALAZ3.DIS THEN
                YTAS.PREF = 'N'
            END ELSE
                YTAS.PREF = 'S'
            END
            RETURN
        END

        Y.VALOR.TAZA.PARAM = YVALAZ3.DIS
        Y.VALOR.TAZA = YINTEREST.RATE
        Y.TERMINO.DIAS.PARAM = YVALAZ3
    NEXT I
*CN008976
    IF TERM.IN.DAYS GT Y.TERMINO.DIAS.PARAM THEN
        IF Y.VALOR.TAZA LE Y.VALOR.TAZA.PARAM THEN
            YTAS.PREF = 'N'
        END ELSE
            YTAS.PREF = 'S'
        END
    END
    RETURN

PROCESS.AA:
***********
    YAAGRP.CONT = ''
    PROD.GRP = R.AA.ARRANGEMENT<AA.ARR.PRODUCT.GROUP>
    CCY.VAL = R.AA.ARRANGEMENT<AA.ARR.CURRENCY>
    CUS.ID = R.AA.ARRANGEMENT<AA.ARR.CUSTOMER>
    STRT.DATE = R.AA.ARRANGEMENT<AA.ARR.START.DATE>
    YACCT.ID = R.AA.ARRANGEMENT<AA.ARR.LINKED.APPL.ID>
    Y.PRODUCT = R.AA.ARRANGEMENT<AA.ARR.PRODUCT>
    YCO.CODE = R.AA.ARRANGEMENT<AA.ARR.CO.CODE>

    GOSUB GET.MUNIC.CORP
    GOSUB GET.CUSTOMER.DET

    idPropertyClass = "TERM.AMOUNT"
    idProperty = ''
    GOSUB ARR.CONDITIONS
    IF returnConditions THEN
        R.AA.TERM = RAISE(returnConditions)
        Y.MONTO = R.AA.TERM<AA.AMT.AMOUNT>
        Y.MAT.DATE = R.AA.TERM<AA.AMT.MATURITY.DATE>
        YTERM.LT = R.AA.TERM<AA.AMT.TERM>
        YTERM.LEN = LEN(YTERM.LT) - 1
        YTERM = YTERM.LT[1,YTERM.LEN]
        COLL.ID = R.AA.TERM<AA.AMT.LOCAL.REF,L.AA.COL.POSN,1>
    END

    YVALAA1 = Y.TIPOAA.DIS.ARR<1,1>
    YVALAA2 = Y.TASAFIJAAA.DIS.ARR<1,1>
    YVALAA3 = Y.TASAREFAA.DIS.ARR<1,1>
    YVALAA4 = Y.PERIODTASAAA.DIS.ARR<1,1>
    YVALAA5 = Y.CANTIDAOPERACIONESAA.DIS.ARR<1,1>
    YVALAA6 = Y.CONDIGINSTIAA.DIS.ARR<1,1>
    YVALAA7 = Y.CODNUMINSTAA.DIS.ARR<1,1>

    Y.REGION = ''; Y.DAYS = ''
    IF Y.MAT.DATE AND STRT.DATE THEN
        YDATE = STRT.DATE
        YDATE1 = Y.MAT.DATE
        GOSUB DATE.30.CHK
    END

    BEGIN CASE
    CASE TERM.IN.DAYS LE '90'
        YTERM.D = 1
    CASE TERM.IN.DAYS GE '91' AND TERM.IN.DAYS LE '180'
        YTERM.D = 2
    CASE TERM.IN.DAYS GE '181' AND TERM.IN.DAYS LE '360'
        YTERM.D = 3
    CASE TERM.IN.DAYS GE '361' AND Y.YEAR LE 2
        YTERM.D = 4
    CASE Y.YEAR GT '2' AND Y.YEAR LE 5
        YTERM.D = 5
    CASE Y.YEAR GT '5'
        YTERM.D = 6
    END CASE

    idPropertyClass = "OVERDUE"
    idProperty = ''; YACT.OD.STATUS1 = ''
    Y.L.LOAN.COND = ''
    GOSUB ARR.CONDITIONS
    IF returnConditions THEN
        R.AA.OVERDUE = RAISE(returnConditions)
        YACT.OD.STATUS1 = R.AA.OVERDUE<AA.OD.LOCAL.REF,OD.L.LOAN.STATUS1.POS>
        Y.L.LOAN.COND = R.AA.OVERDUE<AA.OD.LOCAL.REF,Y.L.LOAN.COND.POS>
    END

    IF YACT.OD.STATUS1 AND (YACT.OD.STATUS1 EQ 'Restructured') THEN
        YCOND.OPER = 'RE'
    END ELSE
        YCOND.OPER = 'N'
    END

    idPropertyClass = "CUSTOMER"
    idProperty = ''
    GOSUB ARR.CONDITIONS
    IF returnConditions THEN
        R.AA.CUSTOMER = RAISE(returnConditions)
        YL.AA.CAMP.TY = R.AA.CUSTOMER<AA.CUS.LOCAL.REF,L.AA.CAMP.TY.POS>
    END

    TAS.INTER.PREF = ''
    IF YL.AA.CAMP.TY THEN
        IF YL.AA.CAMP.TY GE '600' AND YL.AA.CAMP.TY LE '610' THEN
            TAS.INTER.PREF = 'EM'
        END
        IF YL.AA.CAMP.TY EQ '620' THEN
            TAS.INTER.PREF = 'VI'
        END
    END

    idPropertyClass = "ACCOUNT"
    idProperty = ''; Y.AA.LOAN = ''
    Y.SKIP.RECORD.FLAG = ''
    GOSUB ARR.CONDITIONS
    IF returnConditions THEN
        R.AA.ACCOUNT = RAISE(returnConditions)
        Y.AA.LOAN = R.AA.ACCOUNT<AA.AC.LOCAL.REF,L.AA.LOAN.DSN.POS>
        Y.AA.AC.CATEGORY = R.AA.ACCOUNT<AA.AC.CATEGORY>
        IF Y.AA.AC.CATEGORY EQ "3173" OR Y.AA.AC.CATEGORY EQ "3174" THEN
            Y.SKIP.RECORD.FLAG = 'Y'
        END
    END

    idPropertyClass = 'INTEREST'
    idProperty = 'PRINCIPALINT'
    GOSUB ARR.CONDITIONS
    IF returnConditions THEN
        R.AA.INTEREST = RAISE(returnConditions)
        YRV.FREQ = R.AA.INTEREST<AA.INT.LOCAL.REF,L.AA.RT.RV.FREQ.POSN>
        Y.EFC.RATE = R.AA.INTEREST<AA.INT.EFFECTIVE.RATE>
    END

    IF Y.EFC.RATE EQ '0' THEN
        TAS.INTER.PREF = 'ZE'
    END
    IF NOT(TAS.INTER.PREF) THEN
        TAS.INTER.PREF = 'N'
    END
*CN009132
    IF Y.L.LOAN.COND EQ "DTP" THEN
        TAS.INTER.PREF = 'S'
    END

    PERIOD.REV = "NA"
*CN008325
    BEGIN CASE
    CASE TERM.IN.DAYS LE '31'
        PERIOD.REV = 'M'
    CASE TERM.IN.DAYS GE '32' AND TERM.IN.DAYS LE '60'
        PERIOD.REV = 'B'
    CASE TERM.IN.DAYS GE '61' AND TERM.IN.DAYS LE '90'
        PERIOD.REV = 'T'
    CASE TERM.IN.DAYS GE '91' AND TERM.IN.DAYS LE '120'
        PERIOD.REV = 'C'
    CASE TERM.IN.DAYS GE '121' AND TERM.IN.DAYS LE '180'
        PERIOD.REV = 'S'
    CASE TERM.IN.DAYS GE '181' AND TERM.IN.DAYS LE '360'
        PERIOD.REV = 'A'
    CASE TERM.IN.DAYS GE '361' AND Y.YEAR LE 2
        PERIOD.REV = 'A2'
    CASE Y.YEAR EQ '3'
        PERIOD.REV = 'A3'
    CASE Y.YEAR EQ '4'
        PERIOD.REV = 'A4'
    CASE Y.YEAR EQ '5'
        PERIOD.REV = 'A5'
    CASE Y.YEAR GT '5' AND Y.YEAR LE 8
        PERIOD.REV = 'A6'
	CASE Y.YEAR GE 9 
		PERIOD.REV = 'A9'
    END CASE

    GOSUB GET.NPV.CALC

    BEGIN CASE
    CASE PROD.GRP EQ 'COMERCIAL'
        SUBSECTOR = Y.AA.LOAN
    CASE PROD.GRP EQ 'CONSUMO'
        SUBSECTOR = '16'
    CASE PROD.GRP EQ 'HIPOTECARIO'
        SUBSECTOR = '16'
    CASE PROD.GRP EQ 'LINEAS.DE.CREDITO'
        FINDSTR "COM" IN Y.PRODUCT SETTING YFM,YSM,YVM THEN
            PROD.GRP = 'COMERCIAL'
            SUBSECTOR = Y.AA.LOAN
        END
        FINDSTR "CONS" IN Y.PRODUCT SETTING YFM,YSM,YVM THEN
            PROD.GRP = 'CONSUMO'
            SUBSECTOR = '16'
        END
    END CASE
    LOCATE PROD.GRP IN Y.GRPSECTAA.VAL.ARR<1,1> SETTING TIPOY.POS THEN
        YSECTOR = Y.GRPSECTAA.DIS.ARR<1,TIPOY.POS>
    END

    IF CUS.FAX.1 NE '' THEN
        YTAS.PREF = 'EM'
        TAS.INTER.PREF = 'EM'
        YAAGRP.CONT = 'E'
    END

    IF COLL.ID EQ '' THEN
        GOSUB COLL.GET.DETAILS
    END

    COLLATERAL.ERR = ''; R.COLLATERAL = ''; YTPO.EFIC = ''; YTIPO.STAT = ''
    YL.COL.BLD.AREA = ''; YL.COL.TOT.VALUA = ''; YL.COL.DEP.VALUE = ''; YCOL.ADDR = ''
    CALL F.READ(FN.COLLATERAL,COLL.ID,R.COLLATERAL,F.COLLATERAL,COLLATERAL.ERR)
    YCOLT.TYPE = R.COLLATERAL<COLL.COLLATERAL.TYPE>
    IF PROD.GRP NE 'CONSUMO' THEN
        YCOL.ADDR = R.COLLATERAL<COLL.ADDRESS>
        IF NOT(YCOL.ADDR) THEN
            YCOL.ADDR = 'NA'
        END
        IF YCOL.ADDR THEN
            CHANGE SM TO ' ' IN YCOL.ADDR
            CHANGE VM TO ' ' IN YCOL.ADDR
        END
        YL.COL.BLD.AREA = R.COLLATERAL<COLL.LOCAL.REF,L.COL.BLD.AREA.POSN>
        YL.COL.TOT.VALUA = R.COLLATERAL<COLL.LOCAL.REF,L.COL.TOT.VALUA.POSN>
        YL.COL.DEP.VALUE = R.COLLATERAL<COLL.LOCAL.REF,L.COL.DEP.VALUE.POSN>
        CHANGE SM TO ' ' IN YL.COL.BLD.AREA
        CHANGE VM TO ' ' IN YL.COL.BLD.AREA

        LOCATE YCOLT.TYPE IN Y.TIPO.VAL.ARR<1,1> SETTING TIPO.POS THEN
            YTPO.EFIC = Y.TIPO.DIS.ARR<1,TIPO.POS>
        END
    END ELSE
        YCOL.ADDR = "NA"
        YL.COL.BLD.AREA = "0"
        YL.COL.TOT.VALUA = "0"
        YTPO.EFIC = "0"
    END

    IF PROD.GRP EQ 'HIPOTECARIO' THEN
        BEGIN CASE
        CASE Y.PRODUCT EQ 'HIP.CONSTRUCCION' OR Y.PRODUCT EQ 'HIP.CONSTRUCCION2'
            YTIPO.STAT = "1"
        CASE Y.PRODUCT EQ 'COM.CG.CONSTRUCCION' OR Y.PRODUCT EQ 'COM.CG.CONSTRUCCION2'
            YTIPO.STAT = "1"
        CASE (Y.PRODUCT EQ 'HIP.ADQUIS.VIVIENDA' OR Y.PRODUCT EQ 'HIP.ADQUIS.VIVIENDA2')
            IF YL.COL.DEP.VALUE EQ 0 THEN
                YTIPO.STAT = "3"
            END ELSE
                YTIPO.STAT = "4"
            END
        END CASE
    END ELSE
*CN008211
        YCOL.ADDR  = 'NA'
        YL.COL.BLD.AREA=0
        YL.COL.TOT.VALUA=0
        YTIPO.STAT=0
        YTPO.EFIC=0
    END

    IF NOT(YTIPO.STAT) THEN
        YTIPO.STAT = "0"
    END

    STRT.DATE = STRT.DATE[7,2]:'/':STRT.DATE[5,2]:'/':STRT.DATE[1,4]
    Y.MAT.DATE = Y.MAT.DATE[7,2]:'/':Y.MAT.DATE[5,2]:'/':Y.MAT.DATE[1,4]
    YTP.REC = '1':YSECTOR:CCY.VAL:'-':REC.ID
    YGRP.ARRY = FMT(YACCT.ID,'L#25'):"|":FMT(STRT.DATE,'L#10'):"|":FMT(YCOND.OPER,'L#2'):"|":FMT(YVALAA1,'R%1'):"|":FMT(YSECTOR,'R%2'):"|":FMT(SUBSECTOR,'R%2'):"|":FMT(YAAGRP.CONT,'L#3'):"|"
    YGRP.ARRY := FMT(YCONTRA.PARTE,'L#100'):"|":FMT(YIDENT,'L#15'):"|":FMT(YTIPO,'R%1'):"|":FMT(CUS.DATE.OF.BIRTH,'L#10'):"|":FMT(Y.MAT.DATE,'L#10'):"|":FMT(CCY.VAL,'L#3'):"|"
    YGRP.ARRY := FMT(Y.MONTO,'R2%17'):"|":FMT(TERM.IN.DAYS,'R%6'):"|":FMT(YTERM.D,'R%2'):"|":FMT(Y.EFC.RATE,'R2%5'):"|":FMT(TAS.INTER.PREF,'L#2'):"|":FMT(YVALAA2,'L#2'):"|"
    YGRP.ARRY := FMT(PERIOD.REV,'L#2'):"|":FMT(YVALAA3,'R%1'):"|":FMT(YVALAA4,'R%1'):"|":FMT(YINT.NPV,'R2%5'):"|":FMT(YCOD.PROVIN,'R%2'):"|":FMT(YCOD.MUNIC,'R%2'):"|"
    YGRP.ARRY := FMT(YCOL.ADDR,'L#100'):"|":FMT(YL.COL.BLD.AREA,'R2%10'):"|":FMT(YL.COL.TOT.VALUA,'R2%17'):"|":FMT(YTIPO.STAT,'R%1'):"|":FMT(YTPO.EFIC,'R%1'):"|"
    YGRP.ARRY := FMT(YVALAA5,'R%8'):"|":FMT(YVALAA6,'L#2'):"|":FMT(YVALAA7,'R%3'):"|"
    RETURN

GET.NPV.CALC:
**************
    SIMULATION.REF = ''; NO.RESET = ''; TOT.PAYMENT = ''; DUE.DATES = ''; DUE.TYPES = ''; DUE.DEFER.DATES = '' ; DATE.RANGE = ''  ; yTOT.PAYMENT = 0
    DUE.METHODS = ''; DUE.TYPE.AMTS = ''; DUE.PROPS = ''; DUE.PROP.AMTS = ''; DUE.OUTS = ''; BALANCES.TO.CHECK = ''; TOT.BALANCES = '' ; TOTAL.REVENU  = 0
    CALL AA.SCHEDULE.PROJECTOR(REC.ID, SIMULATION.REF, NO.RESET, DATE.RANGE, TOT.PAYMENT, DUE.DATES, DUE.DEFER.DATES, DUE.TYPES, DUE.METHODS,DUE.TYPE.AMTS, DUE.PROPS, DUE.PROP.AMTS, DUE.OUTS)
    yTOT.PAYMENT = SUM(TOT.PAYMENT)

    Y.IDS.DETAILS<1> = REC.ID
    Y.IDS.DETAILS<2> = "YES"
    CALL REDO.GET.DISBURSEMENT.DETAILS(Y.IDS.DETAILS,R.DISB.DETAILS,Y.COMMITED.AMT,Y.PEND.DISB)
    Y.TOT.DIS.AMT = ABS(R.DISB.DETAILS<3>)
    TOTAL.REVENU =  Y.TOT.DIS.AMT - yTOT.PAYMENT

    TOT.IRR = ''; YINT.NPV = ''
    YEFC.RATE = ''; YEFC.RATE = Y.EFC.RATE / YYR.MTH
*    TOT.IRR = ((ABS(TOTAL.REVENU) *Y.EFC.RATE)/ (Y.NO.OF.MONTHS*Y.EFC.RATE)) / 100
*    YINT.NPV = ((1+TOT.IRR)^YYR.MTH) - 1
    YINT.NPV = (ABS(TOTAL.REVENU) / ABS(Y.TOT.DIS.AMT) * 10)
    YINT.NPV = YINT.NPV + Y.EFC.RATE
    RETURN

GET.MUNIC.CORP:
***************
    YOPER.TYPE = ''; YCOD.MUNIC  = ''; YCOD.PROVIN = ''
    LOCATE YCO.CODE IN Y.COMP.VAL.ARR<1,1> SETTING COMP.POS THEN
        YOPER.TYPE = Y.COMP.DIS.ARR<1,COMP.POS>
        YCOD.PROVIN = FIELD(YOPER.TYPE,'.',1)
        YCOD.MUNIC = FIELD(YOPER.TYPE,'.',2)
    END
    RETURN

ARR.CONDITIONS:
***************
    ArrangementID = REC.ID ; effectiveDate = LAST.WORK.DAY; returnIds = ''; R.CONDITION =''; returnConditions = ''; returnError = ''
    CALL AA.GET.ARRANGEMENT.CONDITIONS(ArrangementID, idPropertyClass, idProperty, effectiveDate, returnIds, returnConditions, returnError)
    RETURN

COLL.GET.DETAILS:
*****************
    REQD.MODE = ''; EFF.DATE = STRT.DATE; R.AA.ACTIVITY.HISTORY = ''; YACT.DTE.ID = ''; ID.COM3 = ''
    CALL AA.READ.ACTIVITY.HISTORY(REC.ID, REQD.MODE, EFF.DATE, R.AA.ACTIVITY.HISTORY)
    IF R.AA.ACTIVITY.HISTORY THEN
        YACT.ID.ARR = R.AA.ACTIVITY.HISTORY<AA.AH.ACTIVITY.ID>
    END
    YACT.DTE.ID = ''
    LOCATE "LENDING-TAKEOVER-ARRANGEMENT" IN YACT.ID.ARR<1,1> SETTING CHG.POSN.1 THEN
        YACT.DTE.ID = R.AA.ACTIVITY.HISTORY<AA.AH.ACT.DATE,CHG.POSN.1,1>
        TERM.AMT.ID = REC.ID:'-TERM.AMOUNT-':YACT.DTE.ID:'.1'
        GOSUB READ.TERM.AMT
    END ELSE

        YACT.DTE.ID = ''
        LOCATE "LENDING-NEW-ARRANGEMENT" IN YACT.ID.ARR<1,1> SETTING CHG.POSN.2 THEN
            YACT.DTE.ID = R.AA.ACTIVITY.HISTORY<AA.AH.ACT.DATE,CHG.POSN.2,1>
            TERM.AMT.ID = REC.ID:'-TERM.AMOUNT-':YACT.DTE.ID:'.1'
            GOSUB READ.TERM.AMT
        END
    END
    COLL.ID = R.AA.TERM.AMOUNT<AA.AMT.LOCAL.REF,L.AA.COL.POSN,1>
    RETURN

READ.TERM.AMT:
**************
    AA.ARR.TERM.AMOUNT.ERR = ''; R.AA.TERM.AMOUNT = ''
    CALL F.READ(FN.AA.ARR.TERM.AMOUNT,TERM.AMT.ID,R.AA.TERM.AMOUNT,F.AA.ARR.TERM.AMOUNT,AA.ARR.TERM.AMOUNT.ERR)
    RETURN

GET.GRUPO.CONTRA:
*****************
    YGRP.CONT = ''
    LOCATE YCUS.SECTOR IN Y.SECT.VAL.ARR<1,1> SETTING SEC.POS THEN
        YGRP.CONT = Y.SECT.DIS.ARR<1,SEC.POS>
    END
    IF YCUS.INDUT EQ '659934' THEN
        YGRP.CONT = 'FD'
    END
    RETURN

GET.CUSTOMER.DET:
*****************
    R.CUSTOMER = ''; ERR.CUSTOMER = ''; YLEGAL.ISS.DATE = ''; YCUS.INDUT = ''; YCUS.SECTOR = ''
    YFLD32 = ''; YFLD33 = ''; Y.CUS.LEGAL.ID = ''
    CALL F.READ(FN.CUSTOMER,CUS.ID,R.CUSTOMER,F.CUSTOMER,ERR.CUSTOMER)
    CU.TIPO.CL = R.CUSTOMER<EB.CUS.LOCAL.REF,L.CU.TIPO.CL.POSN>
    YCUS.CIDENT = R.CUSTOMER<EB.CUS.LOCAL.REF,L.CU.CIDENT.POSN>
    YCUS.RNC = R.CUSTOMER<EB.CUS.LOCAL.REF,L.CU.RNC.POSN>
    YCUS.FOREIGN = R.CUSTOMER<EB.CUS.LOCAL.REF,L.CU.PASS.NAT.POSN>
    Y.CUS.LEGAL.ID = R.CUSTOMER<EB.CUS.LEGAL.ID,1>
    Y.L.CU.ACTANAC = R.CUSTOMER<EB.CUS.LOCAL.REF,L.CU.ACTANAC.POSN>
    Y.L.CU.NOUNICO = R.CUSTOMER<EB.CUS.LOCAL.REF,L.CU.NOUNICO.POSN>
    CUS.GIVEN.NAMES = R.CUSTOMER<EB.CUS.GIVEN.NAMES>
    CUS.FAMILY.NAME = R.CUSTOMER<EB.CUS.FAMILY.NAME>
    CUS.TEXT = R.CUSTOMER<EB.CUS.TEXT>
    CUS.NAME.1 = R.CUSTOMER<EB.CUS.NAME.1>
    CUS.NAME.2 = R.CUSTOMER<EB.CUS.NAME.2>
    CUS.FAX.1 = R.CUSTOMER<EB.CUS.FAX.1>
    YL.LOCALIDAD = R.CUSTOMER<EB.CUS.LOCAL.REF,L.LOCALIDAD.POSN>
    YCUS.INDUT = R.CUSTOMER<EB.CUS.LOCAL.REF,L.APAP.INDUSTRY.POSN>
    YCO.CODE = R.CUSTOMER<EB.CUS.COMPANY.BOOK>
    YCUS.SECTOR = R.CUSTOMER<EB.CUS.SECTOR>
    YNATIONALITY = R.CUSTOMER<EB.CUS.NATIONALITY>

*CN008565
    IF YCUS.SECTOR GE 3002 AND YCUS.SECTOR LE 3006 THEN
        Y.SKIP.RECORD.FLAG = 'Y'
    END

    IF CU.TIPO.CL EQ 'PERSONA JURIDICA' THEN
        YCONTRA.PARTE = CUS.NAME.1:' ':CUS.NAME.2
*        YCONTRA.PARTE = CUS.TEXT
        CUS.DATE.OF.BIRTH = R.CUSTOMER<EB.CUS.LEGAL.ISS.DATE>
        CUS.DATE.OF.BIRTH = CUS.DATE.OF.BIRTH[7,2]:'/':CUS.DATE.OF.BIRTH[5,2]:'/':CUS.DATE.OF.BIRTH[1,4]
        YAAGRP.CONT = 'EP'
    END ELSE
        YCONTRA.PARTE = CUS.GIVEN.NAMES:' ':CUS.FAMILY.NAME
        CUS.DATE.OF.BIRTH = R.CUSTOMER<EB.CUS.DATE.OF.BIRTH>
        CUS.DATE.OF.BIRTH = CUS.DATE.OF.BIRTH[7,2]:'/':CUS.DATE.OF.BIRTH[5,2]:'/':CUS.DATE.OF.BIRTH[1,4]
        YAAGRP.CONT = 'PF'
    END

    BEGIN CASE
    CASE YCUS.CIDENT NE ''
        YIDENT = YCUS.CIDENT
        YTIPO = 1
    CASE YCUS.RNC NE ''
        YIDENT = YCUS.RNC
        YTIPO = 2
        GOSUB READ.INST.PARAM
    CASE Y.L.CU.ACTANAC NE ''
        YIDENT = Y.L.CU.ACTANAC
        YIDENT = RIGHT(YIDENT,15)
        YTIPO = 5
    CASE Y.L.CU.NOUNICO NE ''
        YIDENT = Y.L.CU.NOUNICO
        YIDENT = RIGHT(YIDENT,15)
        YTIPO = 5
    CASE YCUS.FOREIGN NE ''
        YIDENT = YCUS.FOREIGN
        IF LEN(Y.CUS.LEGAL.ID) GE '16' THEN
            YIDENT = RIGHT(YCUS.FOREIGN,15)
        END
        YTIPO = 4
    END CASE

    IF NOT(YFLD32) THEN
        YFLD32 = "NA"
    END
    IF NOT(YFLD33) THEN
        YFLD33 = "1"
    END

    GOSUB GET.GRUPO.CONTRA
    RETURN

READ.ACCOUNT.DET:
*****************
    R.ACCOUNT = ''; ACCOUNT.ERR = ''
    CALL F.READ(FN.ACCOUNT,REC.ID,R.ACCOUNT,F.ACCOUNT,ACCOUNT.ERR)
    RETURN

READ.INST.PARAM:
****************
    R.REDO.APAP.INSTIT.FINANC.PARAM = ''; ERR.REDO.APAP.INSTIT.FINANC.PARAM = ''
    CALL F.READ(FN.REDO.APAP.INSTIT.FINANC.PARAM,YCUS.RNC,R.REDO.APAP.INSTIT.FINANC.PARAM,F.REDO.APAP.INSTIT.FINANC.PARAM,ERR.REDO.APAP.INSTIT.FINANC.PARAM)
    IF R.REDO.APAP.INSTIT.FINANC.PARAM THEN
        YFLD32 = R.REDO.APAP.INSTIT.FINANC.PARAM<INST.FIN.TIPO.INSTITUTION>
        YFLD33 = R.REDO.APAP.INSTIT.FINANC.PARAM<INST.FIN.NUMERO.INSTITUTION>
    END
    RETURN

DATE.30.CHK:
*************
    YACT.MONTH = 0; Y.NO.OF.MONTHS = 0; Y.MNTH = ''; Y.DAYS = ''; Y.GDAYS = ''
    TMP.YDATE = ''; TMP.YDATE1 = ''; Y.YEAR = 0
    IF LEN(YDATE) <> 8 OR LEN(YDATE1) <> 8 THEN
        Y.DAYS = 0
        RETURN
    END
    IF YDATE[1,4] > YDATE1[1,4] THEN
        TMP.YDATE1 = YDATE1
        TMP.YDATE = YDATE
        YDATE1 = TMP.YDATE
        YDATE = TMP.YDATE1
    END
    Y.GDAYS = 'C'
    CALL EB.NO.OF.MONTHS(YDATE,YDATE1,Y.NO.OF.MONTHS)
    Y.MNTH = Y.NO.OF.MONTHS:'M'
    CALL CALENDAR.DAY(YDATE,'+',Y.MNTH)
    CALL CDD('',Y.MNTH,YDATE1,Y.GDAYS)
    IF Y.NO.OF.MONTHS THEN
        YACT.MONTH = Y.NO.OF.MONTHS * 30
    END
    Y.DAYS =  YACT.MONTH + Y.GDAYS
    Y.YEAR = Y.NO.OF.MONTHS/12
    TERM.IN.DAYS = ABS(Y.DAYS)
    RETURN

EVAL.PROD.INT.RATE:
*******************
    CAT.VAL = R.AZ.ACCOUNT<AZ.ALL.IN.ONE.PRODUCT>
    R.AZ.PRODUCT.PARAMETER = ''; AZ.PRODUCT.PARAMETER.ERR = ''; YL.AP.ABB.DEPO = ''
    CALL F.READ(FN.AZ.PRODUCT.PARAMETER,CAT.VAL,R.AZ.PRODUCT.PARAMETER,F.AZ.PRODUCT.PARAMETER,AZ.PRODUCT.PARAMETER.ERR)
    YL.AP.ABB.DEPO = R.AZ.PRODUCT.PARAMETER<AZ.APP.LOCAL.REF,L.AP.ABB.DEPO.POSN>
    RETURN

END
