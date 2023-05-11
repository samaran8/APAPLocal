* @ValidationCode : MjotNzI0MjI1NDU0OkNwMTI1MjoxNjgyMDcwMTUzNDQ0OjMzM3N1Oi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 21 Apr 2023 15:12:33
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : 333su
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.LAPAP
*-----------------------------------------------------------------------------------
*Modification History:
*DATE                 WHO                  REFERENCE                     DESCRIPTION
*21/04/2023      CONVERSION TOOL     AUTO R22 CODE CONVERSION      FM TO @FM, VM TO @VM, SM TO @SM,++ TO +=, INCLUDE TO INSERT
*21/04/2023         SURESH           MANUAL R22 CODE CONVERSION           NOCHANGE
*-----------------------------------------------------------------------------------
SUBROUTINE REDO.OVR.LOAN.REPORT(Y.FIN.ARR)

    $INSERT I_COMMON ;*AUTO R22 CODE CONVERSION - START
    $INSERT I_EQUATE
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.ACCOUNT
    $INSERT I_F.AA.INTEREST.ACCRUALS
    $INSERT I_F.AA.INTEREST
    $INSERT I_F.AA.CUSTOMER
    $INSERT I_F.AA.OVERDUE
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_F.CUSTOMER
    $INSERT I_F.AA.ACCOUNT.DETAILS
    $INSERT I_F.AA.BILL.DETAILS
    $INSERT I_F.AA.ACTIVITY.HISTORY
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.EB.LOOKUP
    $INSERT I_F.REDO.H.CUSTOMER.PROVISIONING ;*AUTO R22 CODE CONVERSION - END

    GOSUB OPEN.FILES
    SEL.CMD = 'SELECT ':FN.AA:' WITH (ARR.STATUS EQ CURRENT OR ARR.STATUS EQ EXPIRED)'
    GOSUB SELECTION.FLDS

    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,REC.ERR)
    YNO.OF.REC = NO.OF.REC
    FLG = '';Y.FIN.ARR = ''
    LOOP
    WHILE NO.OF.REC GT 0 DO
        Y.STT1 = ''; Y.STT = ''; Y.SF.AGE = ''
        FLG += 1
        CRT "REDO.OVERDUE.LOAN.REPORT.BATCH - ":FLG:"/":YNO.OF.REC
        Y.AA.ID = SEL.LIST<FLG>
        GOSUB RETREIVE.VALUES
        IF Y.STT1 EQ '' AND Y.STT EQ '' AND Y.SF.AGE EQ '' THEN
            GOSUB GET.HIS
            GOSUB GET.PROVIN.BALANCE
            GOSUB GET.DISB.AMT
            GOSUB FINAL.ARR
        END
        NO.OF.REC -= 1
    REPEAT
RETURN
*------------------------------------------------------------------
OPEN.FILES:
*------------------------------------------------------------------

    FN.AA = 'F.AA.ARRANGEMENT'
    F.AA = ''
    CALL OPF(FN.AA,F.AA)

    FN.AC = 'F.ACCOUNT'
    F.AC = ''
    CALL OPF(FN.AC,F.AC)

    FN.AAA = 'F.AA.ARRANGEMENT.ACTIVITY'
    F.AAA = ''
    CALL OPF(FN.AAA,F.AAA)

    FN.CUS = 'F.CUSTOMER'
    F.CUS = ''
    CALL OPF(FN.CUS,F.CUS)

    FN.AA.AC = 'F.AA.ACCOUNT.DETAILS'
    F.AA.AC = ''
    CALL OPF(FN.AA.AC,F.AA.AC)

    FN.AA.INT = 'F.AA.INTEREST.ACCRUALS'
    F.AA.INT = ''
    CALL OPF(FN.AA.INT,F.AA.INT)

    FN.AA.BILL = 'F.AA.BILL.DETAILS'
    F.AA.BILL = ''
    CALL OPF(FN.AA.BILL,F.AA.BILL)

    FN.AA.HIS = 'F.AA.ACTIVITY.HISTORY'
    F.AA.HIS = ''
    CALL OPF(FN.AA.HIS,F.AA.HIS)

    FN.EB.LOOKUP = 'F.EB.LOOKUP'
    F.EB.LOOKUP = ''
    CALL OPF(FN.EB.LOOKUP,F.EB.LOOKUP)

    FN.REDO.H.CUSTOMER.PROVISIONING = "F.REDO.H.CUSTOMER.PROVISIONING"
    F.REDO.H.CUSTOMER.PROVISIONING  = ""
    CALL OPF(FN.REDO.H.CUSTOMER.PROVISIONING,F.REDO.H.CUSTOMER.PROVISIONING)

    FN.AA.BILL.HST = 'F.AA.BILL.DETAILS.HIST'
    F.AA.BILL.HST = ''
    CALL OPF(FN.AA.BILL.HST,F.AA.BILL.HST)

    Y.APPL = 'AA.PRD.DES.CUSTOMER':@FM:'CUSTOMER':@FM:'AA.PRD.DES.OVERDUE'
    Y.FLDS = 'L.AA.CAMP.TY':@FM:'L.CU.CIDENT':@VM:'L.CU.RNC':@VM:'L.CU.NOUNICO':@VM:'L.CU.ACTANAC':@VM:'L.CU.TEL.NO':@VM:'L.CU.TEL.AREA':@FM:'L.LOAN.STATUS.1':@VM:'L.LOAN.COND'
    CALL MULTI.GET.LOC.REF(Y.APPL,Y.FLDS,Y.POS)
    Y.CA.POS = Y.POS<1,1> ; Y.CID.POS = Y.POS<2,1> ; Y.RNC.POS = Y.POS<2,2> ; Y.NOUN.POS = Y.POS<2,3> ; Y.ACT.POS = Y.POS<2,4>; Y.TEL.NO.PS = Y.POS<2,5> ;POS.L.CU.TEL.AREA = Y.POS<2,6>
    Y.LN.ST.POS = Y.POS<3,1> ; Y.LN.COND.POS = Y.POS<3,2>
RETURN

*------------------------------------------------------------------
SELECTION.FLDS:
*------------------------------------------------------------------

    LOCATE 'LOAN.PORTFOLIO' IN D.FIELDS SETTING POS.LOP THEN
        Y.PD.DFG = D.RANGE.AND.VALUE<POS.LOP>
        SEL.CMD := ' AND (PRODUCT.GROUP EQ ':Y.PD.DFG:')'
    END
    LOCATE 'PRODUCT.TYPE' IN D.FIELDS SETTING POS.PRD THEN
        Y.PRD.TR = D.RANGE.AND.VALUE<POS.PRD>
        SEL.CMD := ' AND (PRODUCT EQ ':Y.PRD.TR:')'
    END
    LOCATE 'LOAN.ORG.AGENCY' IN D.FIELDS SETTING POS.AG THEN
        Y.COM.DF = D.RANGE.AND.VALUE<POS.AG>
        SEL.CMD := ' AND (CO.CODE EQ ':Y.COM.DF:')'
    END
    LOCATE 'LOAN.STATUS' IN D.FIELDS SETTING POS.LS THEN
        Y.LON.STATJS = D.RANGE.AND.VALUE<POS.LS>
        GOSUB CHECK.NAM
    END
    LOCATE 'CAMPAIGN.TYPE' IN D.FIELDS SETTING POS.CPA THEN
        Y.CAP.GFT = D.RANGE.AND.VALUE<POS.CPA>
    END
    LOCATE 'COURT.DATE' IN D.FIELDS SETTING POS.CD THEN
        Y.COURT.DATE = D.RANGE.AND.VALUE<POS.CD>
    END
RETURN

CHECK.NAM:
    Y.TRANS.ST = ''

    BEGIN CASE
        CASE Y.LON.STATJS EQ 'COBRANZA JUDICIAL'
            Y.TRANS.ST = Y.LON.STATJS
            Y.LON.STATJS = 'JudicialCollection'

        CASE Y.LON.STATJS EQ 'NORMAL'
            Y.TRANS.ST = Y.LON.STATJS
            Y.LON.STATJS = 'Normal'

        CASE Y.LON.STATJS EQ 'REESTRUCTURADO'
            Y.TRANS.ST = Y.LON.STATJS
            Y.LON.STATJS = 'Restructured'

        CASE Y.LON.STATJS EQ 'CASTIGADO'
            Y.TRANS.ST = Y.LON.STATJS
            Y.LON.STATJS = 'Write-off'
    END CASE
RETURN

FINAL.ARR:

    Y.TOT.DEF.BAL = Y.INT.PN + Y.INS.AMT + Y.MORA.BAL + Y.PRIN.BAL + Y.INT.BAL + Y.CARG.AMT
    Y.FIN.ARR<-1> = Y.PRD.GRP:'*':Y.PRD:'*':Y.CAMP.TYPE:'*':Y.COMP:'*':Y.AC.ID:'*':Y.ALT.ID:'*':Y.NAME:'*':Y.IDS.CUS:'*':Y.PHO:'*':Y.ST.DESC:'*':Y.ST.C.DESC
*                     1               2        3              4          5            6          7           8              9          10               11
    Y.FIN.ARR := '*':Y.DELAY:'*':Y.CURR:'*':Y.RATE:'*':Y.OVR.AMT.CNT:'*':Y.LAST.REP.DATE:'*':Y.LAST.REP.AMT:'*':Y.PROV.BAL:'*':Y.TOT.DIS.AMT:'*':Y.PRIN.BAL:'*':Y.INT.BAL
*                     12          13          14          15            16                   17               18              19                20            21
    Y.FIN.ARR := '*':Y.MORA.BAL:'*':Y.INS.AMT:'*':Y.CARG.AMT:'*':Y.INT.PN:'*':Y.TOT.DEF.BAL
*                       22          23              24                 25            26


RETURN

RETREIVE.VALUES:
    AA.AC.ER = ''; R.AA.AC = ''
    CALL F.READ(FN.AA.AC,Y.AA.ID,R.AA.AC,F.AA.AC,AA.AC.ER)
    Y.SF.AGE = ''

    Y.SETTLE.STATUS = R.AA.AC<AA.AD.SET.STATUS>
    CHANGE @SM TO @FM IN Y.SETTLE.STATUS
    CHANGE @VM TO @FM IN Y.SETTLE.STATUS

    LOCATE "UNPAID" IN Y.SETTLE.STATUS SETTING SET.POS ELSE ;* Atleast one bill has to be unpaid status, then only that loan has to be displayed on report.
        Y.SF.AGE = 1
        RETURN
    END

    CALL F.READ(FN.AA,Y.AA.ID,R.AA,F.AA,AA.ERR)
    Y.PRD.GRP = R.AA<AA.ARR.PRODUCT.GROUP>
    Y.PRD = R.AA<AA.ARR.PRODUCT>
    Y.CURR = R.AA<AA.ARR.CURRENCY>
    Y.COMP = R.AA<AA.ARR.CO.CODE>
    Y.AC.ID = R.AA<AA.ARR.LINKED.APPL.ID>
    Y.CUS = R.AA<AA.ARR.CUSTOMER>

    IF Y.CAP.GFT THEN
        CALL AA.GET.ARRANGEMENT.CONDITIONS(Y.AA.ID,'CUSTOMER','','','',RET.COND,RET.ERR)
        RET.COND = RAISE(RET.COND)
        Y.CAMP.TYPE = RET.COND<AA.CUS.LOCAL.REF,Y.CA.POS>
        Y.STT1 = ''
        IF Y.CAP.GFT NE Y.CAMP.TYPE THEN
            Y.STT1 = 1
            RETURN
        END
    END

    CALL F.READ(FN.AC,Y.AC.ID,R.AC,F.AC,AC.ERR)
    Y.ALT.ID = R.AC<AC.ALT.ACCT.ID>

    CALL F.READ(FN.CUS,Y.CUS,R.CUS,F.CUS,CUS.ERR)
    Y.NAME.1 = R.CUS<EB.CUS.NAME.1,1> ; Y.NAME.2 = R.CUS<EB.CUS.NAME.2,LNGG,1>

    Y.NAME = Y.NAME.1:' ':Y.NAME.2
    IF Y.NAME.1 EQ '' AND Y.NAME.2 EQ '' THEN
        Y.NAME = R.CUS<EB.CUS.NAME.1,2>:' ':R.CUS<EB.CUS.NAME.2,LNGG,2>
    END

    Y.IDS.CUS = ''
    Y.CIDENT = R.CUS<EB.CUS.LOCAL.REF,Y.CID.POS>
    IF Y.CIDENT THEN
        Y.IDS.CUS<1,-1> := Y.CIDENT
    END
    Y.RNC = R.CUS<EB.CUS.LOCAL.REF,Y.RNC.POS>
    IF Y.RNC THEN
        Y.IDS.CUS<1,-1> := Y.RNC
    END
    Y.LEGAL.ID = R.CUS<EB.CUS.LEGAL.ID>
    IF Y.LEGAL.ID THEN
        Y.IDS.CUS<1,-1> := Y.LEGAL.ID
    END
    Y.NOUNICA = R.CUS<EB.CUS.LOCAL.REF,Y.NOUN.POS>
    IF Y.NOUNICA THEN
        Y.IDS.CUS<1,-1> := Y.NOUNICA
    END
    Y.ACTI = R.CUS<EB.CUS.LOCAL.REF,Y.ACT.POS>
    IF Y.ACTI THEN
        Y.IDS.CUS<1,-1> := Y.ACTI
    END

*Y.PHO = R.CUS<EB.CUS.PHONE.1> ;* Commented since local fields holds the phone no.

    Y.PHO = ""
    Y.PHO = CATS(R.CUS<EB.CUS.LOCAL.REF,POS.L.CU.TEL.AREA>,R.CUS<EB.CUS.LOCAL.REF,Y.TEL.NO.PS>)
    CHANGE @SM TO @VM IN Y.PHO
    GOSUB GET.OVRDUE
RETURN

GET.OVRDUE:

    CALL AA.GET.ARRANGEMENT.CONDITIONS(Y.AA.ID,'OVERDUE','','','',RET.PR.OV,OV.ERR)
    RET.PR.OV = RAISE(RET.PR.OV)
    Y.LOAN.STATUS = RET.PR.OV<AA.OD.LOCAL.REF,Y.LN.ST.POS>; Y.LOAN.STATUS = CHANGE(Y.LOAN.STATUS,@SM,@VM)
    Y.LOAN.COND = RET.PR.OV<AA.OD.LOCAL.REF,Y.LN.COND.POS> ; Y.LOAN.COND = CHANGE(Y.LOAN.COND,@SM,@VM)

    GOSUB CK.DESC.LM

    Y.OVR.STATUS = RET.PR.OV<AA.OD.OVERDUE.STATUS>
    CHANGE @SM TO @VM IN Y.OVR.STATUS
    Y.STT = ''
    IF Y.LON.STATJS THEN
        LOCATE Y.LON.STATJS IN Y.LOAN.STATUS<1,1> SETTING POS.LLU THEN
            Y.STT = ''
        END ELSE
            Y.STT = 1
            RETURN
        END
    END

* CALL F.READ(FN.AA.AC,Y.AA.ID,R.AA.AC,F.AA.AC,AA.AC.ER)
    Y.BIL.IDS = R.AA.AC<AA.AD.BILL.ID> ; Y.BIL.IDS = CHANGE(Y.BIL.IDS,@SM,@FM) ; Y.BIL.IDS = CHANGE(Y.BIL.IDS,@VM,@FM)
    Y.BIL.TYPE = R.AA.AC<AA.AD.BILL.TYPE> ; Y.BIL.TYPE =  CHANGE(Y.BIL.TYPE,@SM,@FM) ; Y.BIL.TYPE =  CHANGE(Y.BIL.TYPE,@VM,@FM)
    Y.BILL.DATE = R.AA.AC<AA.AD.BILL.DATE> ; Y.BILL.DATE = CHANGE(Y.BILL.DATE,@SM,@FM) ; Y.BILL.DATE = CHANGE(Y.BILL.DATE,@VM,@FM)
    Y.BILL.STATUS = R.AA.AC<AA.AD.BILL.STATUS> ; Y.BILL.STATUS = CHANGE(Y.BILL.STATUS,@SM,@FM) ; Y.BILL.STATUS = CHANGE(Y.BILL.STATUS,@VM,@FM)
    Y.BILL.PAY.DATE = R.AA.AC<AA.AD.BILL.PAY.DATE>; Y.BILL.PAY.DATE = CHANGE(Y.BILL.PAY.DATE,@SM,@FM); Y.BILL.PAY.DATE = CHANGE(Y.BILL.PAY.DATE,@VM,@FM)
    YBILL.CNT = DCOUNT(Y.BIL.IDS,@FM) ; Y.BILL.SETCNT = R.AA.AC<AA.AD.BILLS.SETTLED.CNT>; YBILL.DTECNT = DCOUNT(Y.BILL.PAY.DATE,@FM)
    Y.DELAY = 0; Y.CNT.V = DCOUNT(Y.BIL.IDS,@FM)
    FLG.PO = '' ; Y.HJ = '' ; Y.DFDD = ''
    IF Y.COURT.DATE THEN
        Y.DFDD = Y.COURT.DATE
        YCOURT.TEMP = Y.COURT.DATE
    END ELSE
        Y.DFDD = TODAY
        YCOURT.TEMP = Y.BILL.PAY.DATE<YBILL.DTECNT>
    END
    IF YBILL.CNT GT Y.BILL.SETCNT THEN
        GOSUB GET.BILL.SETT.DETAILS
    END

    Y.INT.AC = Y.AA.ID:'-PRINCIPALINT'
    CALL F.READ(FN.AA.INT,Y.INT.AC,R.AA.INT,F.AA.INT,INT.ERR)
    Y.RATE = ''
    Y.RATE = R.AA.INT<AA.INT.ACC.RATE,1,1>

    IF NOT(Y.RATE) THEN
        CALL AA.GET.ARRANGEMENT.CONDITIONS(Y.AA.ID,'INTEREST','PRINCIPALINT','','',RET.PR.OV,OV.ERR)
        RET.PR.OV = RAISE(RET.PR.OV)
        Y.RATE = RET.PR.OV<AA.INT.EFFECTIVE.RATE>
    END

    FLG.R = '' ; Y.OVR.AMT = '' ; Y.INS.AMT = ''; Y.CARG.AMT = '' ; Y.OVR.AMT.CNT = 0
    GOSUB CK.CRT.DATE
RETURN

GET.BILL.SETT.DETAILS:
**********************
    Y.CNT = Y.BILL.SETCNT + 1
    LOOP
    WHILE Y.CNT LE YBILL.CNT
        IF Y.BILL.STATUS<Y.CNT> NE 'SETTLED' AND Y.BIL.TYPE<Y.CNT> EQ 'PAYMENT' THEN
            Y.BL.DATE = Y.BILL.DATE<Y.CNT>
            IF NOT(Y.BL.DATE) THEN
                CONTINUE
            END ;*AUTO R22 CODE CONVERSION
            Y.CNT = YBILL.CNT
            Y.DAYS = 'C'
            CALL CDD(Y.REG,Y.BL.DATE,Y.DFDD,Y.DAYS)
            Y.DELAY = Y.DAYS
            Y.HJ = 'Y'
        END
        Y.CNT += 1 ;*AUTO R22 CODE CONVERSION
    REPEAT
RETURN

CK.CRT.DATE:
************
    LOOP
    WHILE Y.CNT.V GT 0 DO
        FLG.R += 1
        Y.BL.ID = Y.BIL.IDS<1,FLG.R>
        BL.ERR = ''; R.AA.BILL = ''
        CALL F.READ(FN.AA.BILL,Y.BL.ID,R.AA.BILL,F.AA.BILL,BL.ERR)
        IF NOT(R.AA.BILL) THEN
            BL.ERR = ''; R.AA.BILL = ''
            CALL F.READ(FN.AA.BILL.HST,Y.BL.ID,R.AA.BILL,F.AA.BILL.HST,BL.ERR)
        END
        IF R.AA.BILL<AA.BD.PAYMENT.DATE> LE YCOURT.TEMP THEN
            IF R.AA.BILL<AA.BD.SETTLE.STATUS,1> EQ 'UNPAID'  THEN
                Y.OVR.AMT += SUM(R.AA.BILL<AA.BD.OS.PROP.AMOUNT>)
                Y.OVR.AMT.CNT += 1
            END

            LOCATE 'INSURANCE' IN R.AA.BILL<AA.BD.PAYMENT.TYPE,1> SETTING POS.INS THEN
                Y.INS.AMT += SUM(R.AA.BILL<AA.BD.OS.PR.AMT,POS.INS>)
            END

            LOCATE 'CARGOS' IN R.AA.BILL<AA.BD.PAYMENT.TYPE,1> SETTING POS.CARG THEN
                Y.CARG.AMT += SUM(R.AA.BILL<AA.BD.OS.PR.AMT,POS.CARG>)
                LOCATE 'PRMORA' IN R.AA.BILL<AA.BD.PAY.PROPERTY,POS.CARG,1> SETTING POS.PRSD THEN
                    Y.CARG.AMT -= R.AA.BILL<AA.BD.OS.PR.AMT,POS.CARG,POS.PRSD>
                END
            END
        END
        Y.CNT.V -= 1
    REPEAT
RETURN

CK.DESC.LM:
***********
    Y.ST.DNT = DCOUNT(Y.LOAN.STATUS,@VM)
    Y.ST.CND = DCOUNT(Y.LOAN.COND,@VM)
    FLG.D = '' ; FLG.C = '' ; Y.ST.DESC = '' ; Y.ST.C.DESC = ''
    LOOP
    WHILE Y.ST.DNT GT 0 DO
        FLG.D += 1
        Y.ST.D = Y.LOAN.STATUS<1,FLG.D>
        Y.ST.ID = 'L.LOAN.STATUS.1*':Y.ST.D
        CALL F.READ(FN.EB.LOOKUP,Y.ST.ID,R.EB.LOOKUP,F.EB.LOOKUP,LK.ERR)
        IF R.EB.LOOKUP<EB.LU.DESCRIPTION,2> EQ '' THEN
            Y.ST.DESC<1,-1> = R.EB.LOOKUP<EB.LU.DESCRIPTION,1>
        END ELSE
            Y.ST.DESC<1,-1> = R.EB.LOOKUP<EB.LU.DESCRIPTION,2>
        END
        Y.ST.DNT -= 1
    REPEAT

    LOOP
    WHILE Y.ST.CND GT 0 DO
        FLG.C += 1
        Y.ST.C = Y.LOAN.COND<1,FLG.C>
        Y.ST.C.ID = 'L.LOAN.COND*':Y.ST.C
        CALL F.READ(FN.EB.LOOKUP,Y.ST.C.ID,R.EB.LOOKUP,F.EB.LOOKUP,LK.ERR)
        IF R.EB.LOOKUP<EB.LU.DESCRIPTION,2> EQ '' THEN
            Y.ST.C.DESC<1,-1> = R.EB.LOOKUP<EB.LU.DESCRIPTION,1>
        END ELSE
            Y.ST.C.DESC<1,-1> = R.EB.LOOKUP<EB.LU.DESCRIPTION,2>
        END
        Y.ST.CND -= 1
    REPEAT
RETURN

GET.HIS:
    HIS.ERR = ''; R.AA.HIS = ''
    CALL F.READ(FN.AA.HIS,Y.AA.ID,R.AA.HIS,F.AA.HIS,HIS.ERR)
    Y.ACTY = R.AA.HIS<AA.AH.ACTIVITY> ; Y.ACTY = CHANGE(Y.ACTY,@SM,@VM) ; Y.ACTY.DUP = Y.ACTY
    Y.ACTY.REF = R.AA.HIS<AA.AH.ACTIVITY.REF> ; Y.ACTY.REF = CHANGE(Y.ACTY.REF,@SM,@VM); Y.ACTY.REF.DUP = Y.ACTY.REF
    Y.ACT.STS = R.AA.HIS<AA.AH.ACT.STATUS> ; Y.ACT.STS = CHANGE(Y.ACT.STS,@SM,@VM) ; CK = '' ; Y.ACT.STS.DUP = Y.ACT.STS
    Y.INT.TYPES = R.AA.HIS<AA.AH.INITIATION> ; Y.INT.TYPES = CHANGE(Y.INT.TYPES,@SM,@VM) ; Y.LAST.REP.DATE = '' ; Y.LAST.REP.AMT = ''

    LOOP
    WHILE CK EQ '' DO
        LOCATE 'TRANSACTION' IN Y.INT.TYPES<1,1> SETTING POS.LP THEN
            IF Y.ACT.STS<1,POS.LP> EQ 'AUTH' AND Y.ACTY<1,POS.LP> MATCHES 'LENDING-APPLYPAYMENT...' THEN
                Y.APL.ACT = Y.ACTY.REF<1,POS.LP>
                CALL F.READ(FN.AAA,Y.APL.ACT,R.AAA,F.AAA,AAA.ERR)
                Y.LAST.REP.DATE = R.AAA<AA.ARR.ACT.EFFECTIVE.DATE>
                Y.LAST.REP.AMT = R.AAA<AA.ARR.ACT.ORIG.TXN.AMT>
                CK = 1
            END ELSE
                DEL Y.ACTY<1,POS.LP>
                DEL Y.ACTY.REF<1,POS.LP>
                DEL Y.ACT.STS<1,POS.LP>
                DEL Y.INT.TYPES<1,POS.LP>
            END
        END ELSE
            CK = 1
        END
    REPEAT
RETURN

GET.PROVIN.BALANCE:

    REQUEST.TYPE = ''; REQUEST.TYPE<2> = 'ALL'; REQUEST.TYPE<4> = 'ECB'
    Y.OV.ST.PR = 'DUE':@VM:Y.OVR.STATUS
    Y.OV.ST.IN = 'DUE':@VM:Y.OVR.STATUS
    Y.OV.MORA = 'DUE':@VM:Y.OVR.STATUS
    Y.CNT.O = DCOUNT(Y.OV.ST.PR,@VM)
    FLG.T = '' ; Y.PRIN.BAL = ''; Y.INT.BAL = '' ; Y.INT.PN = ''
    Y.MORA.BAL = '' ; SYSTEM.DATE = '' ; END.DATE = '' ; BALANCE.AMOUNT = ''

    IF Y.COURT.DATE NE TODAY AND Y.COURT.DATE NE '' THEN
        START.DATE = Y.COURT.DATE ;  END.DATE = Y.COURT.DATE ; SYSTEM.DATE = Y.COURT.DATE ; REQUEST.DATE = Y.COURT.DATE
    END ELSE
        START.DATE = TODAY ; END.DATE = TODAY ; SYSTEM.DATE = TODAY ; REQUEST.DATE = TODAY
    END

    Y.PR.PN = 'ACCPENALTINT'
    CALL AA.GET.ECB.BALANCE.AMOUNT(Y.AC.ID, Y.PR.PN, REQUEST.DATE, BALANCE.AMOUNT,RET.ERROR)
    Y.INT.PN = ABS(BALANCE.AMOUNT)

    LOOP
    WHILE Y.CNT.O GT 0 DO
        FLG.T += 1
        Y.PRO = Y.OV.ST.PR<1,FLG.T>:'ACCOUNT'
        CALL AA.GET.ECB.BALANCE.AMOUNT(Y.AC.ID, Y.PRO, REQUEST.DATE, BALANCE.AMOUNT,RET.ERROR)
        Y.PRIN.BAL += ABS(BALANCE.AMOUNT)

        Y.PR.I = Y.OV.ST.IN<1,FLG.T>:'PRINCIPALINT'
        CALL AA.GET.ECB.BALANCE.AMOUNT(Y.AC.ID, Y.PR.I, REQUEST.DATE, BALANCE.AMOUNT,RET.ERROR)
        Y.INT.BAL += ABS(BALANCE.AMOUNT)

        Y.PR.MORA = Y.OV.MORA<1,FLG.T>:'PRMORA'
        CALL AA.GET.ECB.BALANCE.AMOUNT(Y.AC.ID, Y.PR.MORA, REQUEST.DATE, BALANCE.AMOUNT,RET.ERROR)
        Y.MORA.BAL += ABS(BALANCE.AMOUNT)

        Y.CNT.O -= 1
    REPEAT

*Y.PROV.BAL = Y.PRIN.BAL + Y.INT.BAL ;* Line has been commented to get the provision balance from REDO.H.CUSTOMER.PROVISIONING.

    CALL F.READ(FN.REDO.H.CUSTOMER.PROVISIONING,Y.CUS,R.REDO.H.CUSTOMER.PROVISIONING,F.REDO.H.CUSTOMER.PROVISIONING,PROV.ERR)
    LOCATE Y.AA.ID IN R.REDO.H.CUSTOMER.PROVISIONING<REDO.CUS.PROV.ARRANGEMENT.ID,1> SETTING PROV.POS THEN
        Y.PROV.BAL = R.REDO.H.CUSTOMER.PROVISIONING<REDO.CUS.PROV.TOTAL.PROV,PROV.POS>
    END ELSE
        Y.PROV.BAL = Y.PRIN.BAL + Y.INT.BAL       ;* In case if arrangement not found in provisioning table.
    END
RETURN

GET.DISB.AMT:

    Y.IDS.DETAILS<1> = Y.AA.ID
    Y.IDS.DETAILS<2> = "YES"
    CALL REDO.GET.DISBURSEMENT.DETAILS(Y.IDS.DETAILS,R.DISB.DETAILS,Y.COMMITED.AMT,Y.PEND.DISB)
    Y.TOT.DIS.AMT = R.DISB.DETAILS<3>
RETURN

END
