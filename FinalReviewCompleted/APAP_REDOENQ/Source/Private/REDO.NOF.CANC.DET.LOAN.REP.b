* @ValidationCode : MjoxNzkyNDgzODI6Q3AxMjUyOjE2ODI1NzkxNDc2NjA6dmlnbmVzaHdhcmk6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 27 Apr 2023 12:35:47
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : vigneshwari
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.NOF.CANC.DET.LOAN.REP(Y.FIN.ARR)
*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
*
* DATE              WHO                REFERENCE                 DESCRIPTION
* 18-APR-2023     Conversion tool    R22 Auto conversion       FM TO @FM, VM to @VM, SM to @SM
* 18-APR-2023      Harishvikram C   Manual R22 conversion      CALL method format changed
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.AA.ACTIVITY.HISTORY
    $INSERT I_F.REDO.TRANSACTION.CHAIN
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.AA.CUSTOMER
    $INSERT I_F.AA.INTEREST
    $INSERT I_F.COLLATERAL
    $INSERT I_F.CUSTOMER
    $INSERT I_F.ACCOUNT
    $INSERT I_F.AA.ACCOUNT
    $INSERT I_F.AA.TERM.AMOUNT
    $INSERT I_F.AA.INTEREST.ACCRUALS
    $INSERT I_F.AA.PROPERTY
    $INSERT I_F.APAP.H.INSURANCE.DETAILS
    $INSERT I_F.AA.REFERENCE.DETAILS
    $INSERT I_F.AA.ACTIVITY.BALANCES
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_F.AA.ACCOUNT.DETAILS
    $USING APAP.TAM


    FN.AC = 'F.ACCOUNT'
    F.AC = ''
    CALL OPF(FN.AC,F.AC)

    FN.AC.HIS = 'F.ACCOUNT$HIS'
    F.AC.HIS = ''
    CALL OPF(FN.AC.HIS,F.AC.HIS)

    FN.AA = 'F.AA.ARRANGEMENT'
    F.AA = ''
    CALL OPF(FN.AA,F.AA)

    FN.AAA = 'F.AA.ARRANGEMENT.ACTIVITY'
    F.AAA = ''
    CALL OPF(FN.AAA,F.AAA)

    FN.AAA.HIS = 'F.AA.ARRANGEMENT.ACTIVITY$HIS'
    F.AAA.HIS = ''
    CALL OPF(FN.AAA.HIS,F.AAA.HIS)

    FN.AA.PR = 'F.AA.PROPERTY'
    F.AA.PR = ''
    CALL  OPF(FN.AA.PR,F.AA.PR)

    FN.CUS = 'F.CUSTOMER'
    F.CUS = ''
    CALL OPF(FN.CUS,F.CUS)

    FN.COL = 'F.COLLATERAL'
    F.COL = ''
    CALL OPF(FN.COL,F.COL)

    FN.AID = 'F.APAP.H.INSURANCE.DETAILS'
    F.AID = ''
    CALL OPF(FN.AID,F.AID)

    FN.RTC = 'F.REDO.TRANSACTION.CHAIN'
    F.RTC = ''
    CALL OPF(FN.RTC,F.RTC)

    FN.AA.HIS = 'F.AA.ACTIVITY.HISTORY'
    F.AA.HIS = ''
    CALL OPF(FN.AA.HIS,F.AA.HIS)

    FN.FT = 'F.FUNDS.TRANSFER'
    F.FT = ''
    CALL OPF(FN.FT,F.FT)

    FN.FT.HIS = 'F.FUNDS.TRANSFER$HIS'
    F.FT.HIS = ''
    CALL OPF(FN.FT.HIS,F.FT.HIS)

    FN.AA.INT = 'F.AA.INTEREST.ACCRUALS'
    F.AA.INT = ''
    CALL OPF(FN.AA.INT,F.AA.INT)

    FN.AA.REF = 'F.AA.REFERENCE.DETAILS'
    F.AA.REF = ''
    CALL OPF(FN.AA.REF,F.AA.REF)

    FN.AA.ACT.BAL = 'F.AA.ACTIVITY.BALANCES'
    F.AA.ACT.BAL = ''
    CALL OPF(FN.AA.ACT.BAL,F.AA.ACT.BAL)
* PACS00321229 - Cristina's email - 20150429 - S
    FN.AA.AD = 'F.AA.ACCOUNT.DETAILS'
    F.AA.AD  = ''
    CALL OPF(FN.AA.AD,F.AA.AD)

    FN.APAP.H.INSURANCE.ID.CONCAT = 'F.APAP.H.INSURANCE.ID.CONCAT'
    F.APAP.H.INSURANCE.ID.CONCAT = ''
    CALL OPF(FN.APAP.H.INSURANCE.ID.CONCAT,F.APAP.H.INSURANCE.ID.CONCAT)
* PACS00321229 - Cristina's email - 20150429 - E
    Y.APL = 'AA.PRD.DES.TERM.AMOUNT':@FM:'COLLATERAL'
    Y.FLDS = 'L.AA.COL':@FM:'L.CO.LOC.STATUS':@VM:'L.COL.SEC.STA'
    CALL MULTI.GET.LOC.REF(Y.APL,Y.FLDS,POS.LL)
    Y.CO.POS = POS.LL<1,1>
    Y.LOC.POS = POS.LL<2,1>
    Y.SEC.POS = POS.LL<2,2>

    GOSUB SEL.SECTION

RETURN

SEL.SECTION:

    SEL.CMD = 'SELECT ':FN.RTC:' WITH (TRANS.VERS LIKE "FUNDS.TRANSFER...REDO.MULTI.AA.ACPOAP..." OR TRANS.VERS LIKE "FUNDS.TRANSFER...REDO.AA.PAY.OFF...") AND (TRANS.AUTH EQ "A")'
    GOSUB SEL.FIELDS

    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,REC.ERR)

    FLG = ''
    LOOP
    WHILE NO.OF.REC GT 0 DO
        FLG += 1
        Y.FT = SEL.LIST<FLG>
        GOSUB GET.DETAILS
        NO.OF.REC -= 1
    REPEAT

RETURN

SEL.FIELDS:

    LOCATE "LOAN.ORIGIN.AGENCY" IN D.FIELDS<1> SETTING Y.AGENCY.POS  THEN
        Y.AGENCY.VAL= D.RANGE.AND.VALUE<Y.AGENCY.POS>
    END

    LOCATE "GUARANTEE.TYPE" IN D.FIELDS<1> SETTING Y.GUARANTEE.TYPE.POS  THEN
        Y.GUARANTEE.TYPE.VAL= D.RANGE.AND.VALUE<Y.GUARANTEE.TYPE.POS>
    END
    LOCATE "GUARANTEE.STATUS" IN D.FIELDS<1> SETTING Y.GUARANTEE.STATUS.POS  THEN
        Y.GUARANTEE.STATUS.VAL= D.RANGE.AND.VALUE<Y.GUARANTEE.STATUS.POS>
    END
    LOCATE "PRODUCT" IN D.FIELDS<1> SETTING Y.PRODUCT.TYPE.POS THEN
        Y.PRODUCT.TYPE.VAL= D.RANGE.AND.VALUE<Y.PRODUCT.TYPE.POS>
    END
    LOCATE "LOAN.PORTFOLIO.TYP" IN D.FIELDS<1> SETTING Y.LOAN.PORTFOLIO.TYPE.POS THEN
        Y.LOAN.PORTFOLIO.TYPE.VAL= D.RANGE.AND.VALUE<Y.LOAN.PORTFOLIO.TYPE.POS>
    END
    LOCATE "CANCEL.DATE" IN D.FIELDS<1> SETTING Y.CANC.DATE.FROM.POS THEN
        Y.CANC.DATE.VAL= D.RANGE.AND.VALUE<Y.CANC.DATE.FROM.POS>
        Y.CD.FR = FIELD(Y.CANC.DATE.VAL,@SM,1)
        Y.CD.TO = FIELD(Y.CANC.DATE.VAL,@SM,2)

        SEL.CMD := ' AND TRANS.DATE GE ':Y.CD.FR:' AND TRANS.DATE LE ':Y.CD.TO
    END
    LOCATE "GUARANTEE.LOC" IN D.FIELDS<1> SETTING GL THEN
        Y.VAL.GL = D.RANGE.AND.VALUE<GL>
    END

    IF Y.CD.FR NE '' AND Y.CD.TO EQ '' THEN
        ENQ.ERROR = 'BOTH DATES SHOULD BE ENTERED'
    END

RETURN

GET.DETAILS:

    CALL F.READ(FN.FT,Y.FT,R.FT,F.FT,FT.ERR)
    IF NOT(R.FT) THEN
        CALL EB.READ.HISTORY.REC(F.FT.HIS,Y.FT,R.FT,FT.ERR)
        IF R.FT THEN
            IF R.FT<FT.RECORD.STATUS> NE 'REVE' THEN
                Y.AC.ID = R.FT<FT.CREDIT.ACCT.NO>
                GOSUB GET.AA.DET
            END
        END ELSE
            RETURN
        END
    END ELSE
        Y.AC.ID = R.FT<FT.CREDIT.ACCT.NO>
        GOSUB GET.AA.DET
    END

RETURN

GET.AA.DET:

    Y.SET = ''
    CALL F.READ(FN.AC,Y.AC.ID,R.AC,F.AC,AC.ER)
    Y.AA.ID = R.AC<AC.ARRANGEMENT.ID>
    IF NOT(R.AC) THEN
        Y.ACC.ID = Y.AC.ID:';1'
        CALL F.READ(FN.AC.HIS,Y.ACC.ID,R.AC,F.AC.HIS,ER.HIS)

        Y.AA.ID = R.AC<AC.ARRANGEMENT.ID>
    END

    CALL F.READ(FN.AA.REF,Y.AA.ID,R.AA.REF,F.AA.REF,REF.ERR)
    Y.FT.ID = FIELD(Y.FT,';',1) ; Y.AAA.ID = '' ; Y.SET = ''
    LOCATE Y.FT.ID IN R.AA.REF<AA.REF.TRANS.REF,1> SETTING POS.FT THEN
        Y.AAA.ID = R.AA.REF<AA.REF.AAA.ID,POS.FT>
        Y.SET = 'Y'
    END ELSE
        Y.SET = 'N'
    END

    IF Y.SET EQ 'N' THEN
        RETURN
    END
* PACS00321229 - Cristina's email - 20150429 - S
    GOSUB GET.AA.AD
    IF Y.SET EQ 'N' THEN
        RETURN
    END
* PACS00321229 - Cristina's email - 20150429 - E
    Y.REGION = R.AC<AC.ACCOUNT.OFFICER>

    Y.ALT.ID = R.AC<AC.ALT.ACCT.ID>

    CALL F.READ(FN.AA,Y.AA.ID,R.AA,F.AA,AA.ER)
    Y.CO.CODE = R.AA<AA.ARR.CO.CODE>
    Y.PRD = R.AA<AA.ARR.PRODUCT>
    Y.PRD.GRP = R.AA<AA.ARR.PRODUCT.GROUP>
    Y.CUS.ID = R.AA<AA.ARR.CUSTOMER>
    Y.CUR = R.AA<AA.ARR.CURRENCY>
    Y.OPEN.DATE = R.AA<AA.ARR.PROD.EFF.DATE>

    IF Y.AGENCY.VAL THEN
        IF Y.CO.CODE NE Y.AGENCY.VAL THEN
            RETURN
        END
    END

    IF Y.PRODUCT.TYPE.VAL THEN
        IF Y.PRD NE Y.PRODUCT.TYPE.VAL THEN
            RETURN
        END
    END

    IF Y.LOAN.PORTFOLIO.TYPE.VAL THEN
        IF Y.PRD.GRP NE Y.LOAN.PORTFOLIO.TYPE.VAL THEN
            RETURN
        END
    END

    CALL F.READ(FN.CUS,Y.CUS.ID,R.CUS,F.CUS,CUS.ER)
    Y.NAME.1 = R.CUS<EB.CUS.NAME.1,1>
    Y.NAME.2 = R.CUS<EB.CUS.NAME.2,1>

    IF Y.NAME.1 EQ '' THEN
        Y.NAME.1 = R.CUS<EB.CUS.NAME.1,2>
    END
    IF Y.NAME.2 EQ '' THEN
        Y.NAME.2 = R.CUS<EB.CUS.NAME.2,2>
    END

    Y.FIN.NAME = Y.NAME.1:' ':Y.NAME.2

    CALL AA.GET.ARRANGEMENT.CONDITIONS(Y.AA.ID,'TERM.AMOUNT','','','',RET.COND,RET.ERR)
    RET.COND = RAISE(RET.COND)
    Y.AMT = RET.COND<AA.AMT.AMOUNT>
    Y.COL.ID = RET.COND<AA.AMT.LOCAL.REF,Y.CO.POS>

    Y.IN.ID = Y.AA.ID:'-PRINCIPALINT'
    CALL F.READ(FN.AA.INT,Y.IN.ID,R.AA.INT,F.AA.INT,INT.ER)
    Y.RATE = R.AA.INT<AA.INT.ACC.RATE,1,1>

    IF Y.RATE EQ '' THEN
        CALL AA.GET.ARRANGEMENT.CONDITIONS(Y.AA.ID,'INTEREST','PRINCIPALINT','','',RET.COND,RET.ERR)
        RET.COND = RAISE(RET.COND)
        Y.RATE = RET.COND<AA.INT.EFFECTIVE.RATE>
    END

    GOSUB GET.DISB    ;* PACS00321229 - Cristina's email - 20150429 - S/E
    GOSUB GET.INS.CONCAT        ;* PACS00321229 - Cristina's email - 20150429 - S/E
    GOSUB GET.COL.DET
    GOSUB GET.RTC
    GOSUB GET.SEP.BAL
    GOSUB FIN.ARR

RETURN


GET.AA.AD:

    Y.AA.AD.PM = '' ; Y.AA.AD.AS = ''
    Y.SET      = '' ; Y.ARR.BILL.TYPE = ''
    R.AA.AD    = '' ; Y.AA.AD.ERR = '' ; POS.Y = ''
    CALL F.READ(FN.AA.AD,Y.AA.ID,R.AA.AD,F.AA.AD,Y.AA.AD.ERR)
    IF R.AA.AD NE "" THEN
        Y.ARR.BILL.TYPE = R.AA.AD<AA.AD.BILL.TYPE>
        CHANGE @SM TO @FM IN Y.ARR.BILL.TYPE
        CHANGE @VM TO @FM IN Y.ARR.BILL.TYPE

        LOCATE 'PAYOFF' IN Y.ARR.BILL.TYPE SETTING POS.Y THEN

            Y.ARR.PAYM = R.AA.AD<AA.AD.PAY.METHOD>
            CHANGE @SM TO @VM IN Y.ARR.PAYM
            Y.ARR.AGIS = R.AA.AD<AA.AD.AGING.STATUS>
            CHANGE @SM TO @VM IN Y.ARR.AGIS
            Y.AA.AD.PM = R.AA.AD<AA.AD.PAY.METHOD, POS.Y>

            GOSUB EVAL.AGING.ST

        END ELSE
            Y.SET = 'N'
        END
    END

RETURN

EVAL.AGING.ST:

    CHANGE @VM TO @FM IN Y.ARR.AGIS
    POS.W = ''
    LOCATE 'SETTLED' IN R.AA.AD<AA.AD.AGING.STATUS> SETTING POS.W THEN
        Y.AA.AD.AS = Y.ARR.AGIS<1,POS.W>
        IF Y.AA.AD.PM EQ "INFO" AND Y.AA.AD.AS EQ "SETTLED" THEN
            Y.SET = 'Y'
        END ELSE
            Y.SET = 'N'
        END
    END

RETURN

FIN.ARR:

    Y.FIN.ARR<-1> = Y.CO.CODE:'*':Y.REGION:'*':Y.PRD:'*':Y.AC.ID:'*':Y.ALT.ID:'*':Y.FIN.NAME:'*':Y.CUR:'*':Y.OPEN.DATE:'*':Y.AMT:'*':Y.RATE:'*':Y.DISP.AMT
    Y.FIN.ARR := '*':Y.LIF.IN.NO:'*':Y.FHA.INS:'*':Y.FHA.NO:'*':Y.GR.LOC.ST:'*':Y.COL.CODE:'*':Y.HL.ID:'*':Y.GR.ST:'*':Y.EFF.DATE:'*':Y.PAY.TYPE:'*':Y.CAL.AGENT
    Y.FIN.ARR := '*':Y.USER:'*':Y.TRANS.AMT:'*':Y.PRIN.BL:'*':Y.INT.BL:'*':Y.OT.BL:'*':Y.INS.BL

RETURN

GET.DISB:

    Y.DISP.AMT = ''
    CALL APAP.TAM.redoGetDisbursementDetails(Y.AA.ID,R.DISB.DETAILS,Y.COMMITED.AMT,Y.PEND.DISB);* R22 Manual conversion
    Y.DISP.AMT = R.DISB.DETAILS<3>        ;* Total Disb amount.

RETURN


GET.INS.CONCAT:

    Y.ERR.AHIIC = ''; R.APAP.H.INSURANCE.ID.CONCAT = ''
    CALL F.READ(FN.APAP.H.INSURANCE.ID.CONCAT,Y.AA.ID,R.APAP.H.INSURANCE.ID.CONCAT,F.APAP.H.INSURANCE.ID.CONCAT,Y.ERR.AHIIC)
    Y.NO.FF.RE = DCOUNT(R.APAP.H.INSURANCE.ID.CONCAT<1>,@VM)
    FLG.L = '' ; Y.INS.ID = '' ; Y.LIF.IN.NO = '' ; Y.FHA.INS = '' ; Y.FHA.NO = ''
    LOOP
    WHILE Y.NO.FF.RE GT 0 DO
        FLG.L += 1
        Y.INS.ID = R.APAP.H.INSURANCE.ID.CONCAT<1,FLG.L>
        GOSUB GET.AHID
        Y.NO.FF.RE -= 1
    REPEAT

RETURN

GET.AHID:

    Y.P.TYPE = ''
    R.AID = '' ; Y.AID.RR = ''
    CALL F.READ(FN.AID,Y.INS.ID,R.AID,F.AID,Y.AID.RR)
    Y.P.TYPE = R.APAP.H.INSURANCE.ID.CONCAT<2,FLG.L>

    IF Y.P.TYPE  EQ 'VI' OR Y.P.TYPE EQ 'VU' OR Y.P.TYPE  EQ 'PVP' OR Y.P.TYPE EQ 'PVC' THEN
        Y.LIF.IN.NO<1,-1> = Y.P.TYPE:"-":R.AID<INS.DET.POLICY.NUMBER>
    END

    IF Y.P.TYPE EQ 'FHA' THEN
        Y.FHA.INS<1,-1> = Y.P.TYPE:"-":R.AID<INS.DET.CHARGE>
        Y.FHA.NO<1,-1> = R.AID<INS.DET.POLICY.NUMBER>
    END

RETURN

GET.COL.DET:

    Y.COL.ID = CHANGE(Y.COL.ID,@SM,@VM) ; Y.CNT = DCOUNT(Y.COL.ID,@VM) ; FLG.H = ''
    Y.GR.LOC.ST = '' ; Y.COL.CODE = '' ; Y.GR.ST = '' ; Y.HL.ID = ''
    LOOP
    WHILE Y.CNT GT 0 DO
        FLG.H += 1
        Y.CL.ID = Y.COL.ID<1,FLG.H>
        CALL F.READ(FN.COL,Y.CL.ID,R.COL,F.COL,COL.ERR)
* Y.GR.LOC.ST<1,-1> = R.COL<COLL.LOCAL.REF,Y.LOC.POS>
        IF Y.GUARANTEE.TYPE.VAL THEN
            IF Y.GUARANTEE.TYPE.VAL EQ R.COL<COLL.COLLATERAL.CODE> THEN
                Y.COL.CODE<1,-1> = R.COL<COLL.COLLATERAL.CODE>
            END
        END ELSE
            Y.COL.CODE<1,-1> = R.COL<COLL.COLLATERAL.CODE>
        END

        IF Y.GUARANTEE.STATUS.VAL THEN
            IF Y.GUARANTEE.STATUS.VAL EQ R.COL<COLL.LOCAL.REF,Y.SEC.POS> THEN
                GOSUB CK.VAL.SEC
            END
        END ELSE
            GOSUB CK.VAL.SEC
        END

        IF Y.VAL.GL THEN
            IF Y.VAL.GL EQ R.COL<COLL.LOCAL.REF,Y.LOC.POS> THEN
                Y.GR.LOC.ST<1,-1> = R.COL<COLL.LOCAL.REF,Y.LOC.POS>
            END
        END ELSE
            Y.GR.LOC.ST<1,-1> = R.COL<COLL.LOCAL.REF,Y.LOC.POS>
        END
        Y.HL.ID<1,-1> = Y.CL.ID
        Y.CNT -= 1
    REPEAT

RETURN

CK.VAL.SEC:

    BEGIN CASE

        CASE R.COL<COLL.LOCAL.REF,Y.SEC.POS> EQ 'CANCELLED'
            Y.GR.ST<1,-1> = 'CANCELADO'

        CASE R.COL<COLL.LOCAL.REF,Y.SEC.POS> EQ 'IN-FORCE'
            Y.GR.ST<1,-1> = 'VIGENTE'

        CASE R.COL<COLL.LOCAL.REF,Y.SEC.POS> EQ 'IN-USE'
            Y.GR.ST<1,-1> = 'EN USO'

        CASE R.COL<COLL.LOCAL.REF,Y.SEC.POS> EQ 'OVERRIDE'
            Y.GR.ST<1,-1> = 'INVALIDO'

    END CASE

RETURN

GET.RTC:

    Y.PAY.TYPE = '' ; Y.TRANS.AMT = ''; Y.PAY.T = ''
    CALL F.READ(FN.RTC,Y.FT.ID,R.RTC,F.RTC,RTC.ERR)
    Y.EFF.DATE = R.RTC<RTC.TRANS.DATE>

    LOCATE Y.FT.ID IN R.RTC<RTC.TRANS.ID,1> SETTING POS.D THEN
        Y.PAY.TYPE = R.RTC<RTC.TRANS.TYPE,POS.D>
        Y.TRANS.AMT = R.RTC<RTC.TRANS.AMOUNT,POS.D>
        Y.TRANS.AMT = ABS(Y.TRANS.AMT)
        Y.PAY.T = Y.PAY.TYPE
    END

    BEGIN CASE
        CASE Y.PAY.T EQ 'CASH'
            Y.PAY.TYPE = 'EFECTIVO'

        CASE Y.PAY.T EQ 'CHECK'
            Y.PAY.TYPE = 'CHEQUES'

    END CASE

    Y.CAL.AGENT = R.RTC<RTC.BRANCH.CODE>
    Y.USER = R.FT<FT.INPUTTER>
    Y.USER = FIELD(Y.USER,'_',2)

RETURN


GET.SEP.BAL:

    CALL F.READ(FN.AA.REF,Y.AA.ID,R.AA.REF,F.AA.REF,REF.ERR)
    Y.FT.ID = FIELD(Y.FT,';',1) ; Y.AAA.ID = '' ; Y.SET = ''
    LOCATE Y.FT.ID IN R.AA.REF<AA.REF.TRANS.REF,1> SETTING POS.FT THEN
        Y.AAA.ID = R.AA.REF<AA.REF.AAA.ID,POS.FT>
        Y.SET = 'Y'
    END ELSE
        Y.SET = 'N'
    END

    IF Y.AAA.ID THEN
        Y.PRIN.BL = '' ; Y.INT.BL = '' ; Y.OT.BL = '' ; Y.INS.BL = ''
        CALL F.READ(FN.AA.ACT.BAL,Y.AA.ID,R.AA.ACT.BAL,F.AA.ACT.BAL,BAL.ERR)
        GOSUB CAL.AMT
        CALL F.READ(FN.AAA,Y.AAA.ID,R.AAA,F.AAA,AAA.ERR)
        IF NOT(R.AAA) THEN
            CALL EB.READ.HISTORY.REC(F.AAA.HIS,Y.AAA.ID,R.AAA,AAA.ERRR)
        END

        Y.CHLD.AAA = R.AAA<AA.ARR.ACT.CHILD.ACTIVITY> ; Y.CLD.CNT = DCOUNT(Y.CHLD.AAA,@VM) ; FLG.CHL = ''; Y.CHLD.CHIN = ''

        LOOP
        WHILE Y.CLD.CNT GT 0 DO
            FLG.CHL += 1
            Y.AAA.ID = Y.CHLD.AAA<1,FLG.CHL>
            GOSUB CAL.AMT
            IF Y.CHLD.Y EQ 'Y' THEN
                CALL F.READ(FN.AAA,Y.AAA.ID,R.AAA,F.AAA,AAA.ERR)
                IF NOT(R.AAA) THEN
                    CALL EB.READ.HISTORY.REC(F.AAA.HIS,Y.AAA.ID,R.AAA,AAA.ERRR)
                END
                IF R.AAA THEN
                    Y.CHLD.AAA<1,-1> = R.AAA<AA.ARR.ACT.CHILD.ACTIVITY>
                    Y.CLD.CNT = DCOUNT(Y.CHLD.AAA,@VM)
                END
            END
            Y.CLD.CNT -= 1
        REPEAT
        Y.INS.BL += Y.TRANS.AMT - (Y.PRIN.BL+Y.INT.BL+Y.OT.BL+Y.INS.BL)
    END

RETURN

CAL.AMT:

    LOCATE Y.AAA.ID IN R.AA.ACT.BAL<AA.ACT.BAL.ACTIVITY.REF,1> SETTING POS.AAA THEN
        Y.PROPS = R.AA.ACT.BAL<AA.ACT.BAL.PROPERTY,POS.AAA>
        Y.PROPS.AMTS = R.AA.ACT.BAL<AA.ACT.BAL.PROPERTY.AMT,POS.AAA>
        Y.PR.CNT = DCOUNT(Y.PROPS,@SM) ; FLP = ''
        LOOP
        WHILE Y.PR.CNT GT 0 DO
            FLP += 1
            Y.PR.CL = Y.PROPS<1,1,FLP>
            Y.PR.CL = FIELD(Y.PR.CL,'.',1)

            CALL CACHE.READ(FN.AA.PR, Y.PR.CL, R.AA.PR, AA.PR.ERR) ;*R22 Auto conversion
            Y.PRP.CLASS = R.AA.PR<AA.PROP.PROPERTY.CLASS>
            GOSUB CASE.SS
            Y.PR.CNT -= 1
        REPEAT
        Y.CHLD.Y = 'Y'
    END ELSE
        Y.CHLD.Y = 'N'
    END

RETURN

CASE.SS:

    BEGIN CASE
        CASE Y.PR.CL EQ 'ACCOUNT'
            Y.PRIN.BL += Y.PROPS.AMTS<1,1,FLP>
        CASE Y.PR.CL EQ 'PRINCIPALINT' OR Y.PR.CL EQ 'PENALTINT'
            Y.INT.BL += Y.PROPS.AMTS<1,1,FLP>
        CASE Y.PR.CL EQ 'PRMORA'
            Y.OT.BL += Y.PROPS.AMTS<1,1,FLP>
        CASE Y.PR.CL NE 'PRMORA' AND Y.PRP.CLASS EQ 'CHARGE'
            Y.INS.BL += Y.PROPS.AMTS<1,1,FLP>
    END CASE

RETURN

END
