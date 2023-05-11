* @ValidationCode : MjoxNzQ1ODE2ODIxOkNwMTI1MjoxNjgxMzYwNjM5MjMxOklUU1M6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 13 Apr 2023 10:07:19
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
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.SEC.ASSET(Y.INV.DET)
*-----------------------------------------------------------------------------
*
* Developed By            : Vijayarani G
*
* Developed On            : 25-NOV-2013
*
* Development Reference   : 786942(FS-219-OA01)
*
* Development Description : This report has to be generated at the end of every month to report the investments made by the bank.
*                           The output is based on the securities investments and the Money Market placements made by the Bank.
*
* Attached To             : BATCH>BNK/REDO.B.SEC.POS.ASSET
*
* Attached As             : COB Routine
*
*-----------------------------------------------------------------------------------------------------------------
*  M O D I F I C A T I O N S
*-----------------------------------------------------------------------------------------------------------------
* Defect Reference       Modified By                    Date of Change        Change Details
* NA                     Thenmalar T                     18-Feb-2013                   Updated mapping positions
* 13-04-2023         CONVERSTION TOOL     R22 AUTO CONVERSTION - FM TO @FM AND VM TO @VM AND SM TO @SM 
* 13-04-2023          ANIL KUMAR B        R22 MANUAL CONVERSTION -NO CHANGES
*-----------------------------------------------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------------------------------------------
* Include files
*-----------------------------------------------------------------------------------------------------------------
* <region name= Inserts>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.COMPANY
    $INSERT I_BATCH.FILES
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.MM.MONEY.MARKET
    $INSERT I_F.SECURITY.POSITION
    $INSERT I_F.SECURITY.MASTER
    $INSERT I_F.SEC.ACC.MASTER
    $INSERT I_F.REDO.H.REPORTS.PARAM
    $INSERT I_F.REDO.H.CUSTOMER.PROVISION
    $INSERT I_F.REDO.AZACC.DESC
    $INSERT I_F.EB.CONTRACT.BALANCES
    $INSERT I_F.INDUSTRY
    $INSERT I_F.CUSTOMER
    $INSERT I_F.CATEGORY
    $INSERT I_F.DATES
    $INSERT I_F.SUB.ASSET.TYPE
    $INSERT I_F.REDO.CATEGORY.CIUU
    $INSERT I_F.SC.POS.ASSET
    $INSERT I_F.EB.RATING
    $INSERT I_F.SEC.TRADE
    $INSERT I_SC.COMMON
    $INSERT I_F.STOCK.EXCHANGE
    $INSERT I_REDO.B.SEC.POS.ASSET.COMMON
    $INSERT I_REDO.GENERIC.FIELD.POS.COMMON
* </region>
*-----------------------------------------------------------------------------
*

    Y.LANG.CODE = R.COMPANY(EB.COM.LANGUAGE.CODE)
    Y.SEC.ACC.MAS  = FIELD(Y.INV.DET,'.',1)
    Y.SEC.MAS  = FIELD(Y.INV.DET,'.',2)

    GOSUB MAIN.PROCESS.SC

*
RETURN

MAIN.PROCESS.SC:
*---------------
    Y.FINAL.MSG = ''
    Y.TODAY = TODAY
    Y.REDO.AZ.DES = Y.SEC.ACC.MAS:".":Y.SEC.MAS
*

    GOSUB READ.SC.RECORD

RETURN
*------------------------------------------------------------------------------------------------------------
MAIN.PROCESS:
*-------------
    GOSUB READ.CUS.RECORD
    GOSUB READ.REDO.AZACC.DESC
*
    GOSUB ASSIGN.ISS.ID
    GOSUB FIND.CATGORY.VAL
    GOSUB DATE.OF.ACQUIS
    GOSUB FIND.MAT.DATE
    GOSUB CALC.MONTH.CNT
    GOSUB CALC.INT.SCH
    GOSUB FIND.MKT.VALUE
    GOSUB FIND.SEC.STATUS.INV
    GOSUB BUY.OR.SELL

    GOSUB FIND.INT.RECEIVABLE
    GOSUB CLASSIFY.COMPANY
    GOSUB FIND.ISIN.CODE
    GOSUB TYPE.OF.CUSTOMER

    Y.PENALTY.RET = "0"
    Y.EARLY.RET = "N"
    Y.PART.RATE = R.SEC.MAS<SC.SCM.LOCAL.REF,L.PARTICIP.RATE.POS>

    GOSUB READ.TRN.CON.DATE

*    GOSUB FIND.LINK.TYPE

    IF Y.RELATION.CODE EQ '' THEN
        Y.LINK.TYPE = ''
        GOSUB ASSING.FIN.VAR
        GOSUB ASSING.FIN.VAL
        GOSUB READ.REDO.REPORT.TEMP
    END
*
*
RETURN
*--------------------------------------------------------------------------------------------------
READ.SC.RECORD:
*--------------
    CALL F.READ(FN.SEC.POS,Y.INV.DET,R.SEC.POS,F.SEC.POS,ERR.SEC.POS)
    IF R.SEC.POS THEN
        Y.BOOK.VALUE = R.SEC.POS<SC.SCP.COST.INVST.SEC.CCY>
        Y.ACQU.DATE = R.SEC.POS<SC.SCP.HELD.SINCE>
    END

    CALL F.READ(FN.SEC.MAS,Y.SEC.MAS,R.SEC.MAS,F.SEC.MAS,ERR.SEC.MAS)
    IF R.SEC.MAS THEN
        Y.MAT.DATE = R.SEC.MAS<SC.SCM.MATURITY.DATE>
        Y.L.INV.FACILITY = R.SEC.MAS<SC.SCM.LOCAL.REF,L.SEC.INV.FACILITY.POS>
        Y.INT.RATE = SUM(R.SEC.MAS<SC.SCM.INTEREST.RATE>)
        Y.CURRENCY = R.SEC.MAS<SC.SCM.SECURITY.CURRENCY>
        Y.COLL.ISS  = R.SEC.MAS<SC.SCM.LOCAL.REF,L.SEC.COLL.ISSUE.POS>
        Y.RATING  = R.SEC.MAS<SC.SCM.RATING,1>
        Y.RATE.REV = R.SEC.MAS<SC.SCM.RATE.CH.DATE,1>
        Y.ISSUE.DATE = R.SEC.MAS<SC.SCM.ISSUE.DATE>

        Y.CUSTOMER.ARR = R.SEC.MAS<SC.SCM.ISSUER>
        Y.CUS.INT = 1
        Y.CUS.TOT = DCOUNT(Y.CUSTOMER.ARR,@VM)
        LOOP
        WHILE Y.CUS.INT LE Y.CUS.TOT
            Y.CUSTOMER = Y.CUSTOMER.ARR<1,Y.CUS.INT>
            GOSUB MAIN.PROCESS
            Y.CUS.INT += 1
        REPEAT
    END

*
RETURN

READ.CUS.RECORD:
*--------------
    CALL F.READ(FN.CUSTOMER,Y.CUSTOMER,R.CUSTOMER,F.CUSTOMER,ERR.CUSTOMER)
    IF R.CUSTOMER THEN
        Y.RELATION.CODE = R.CUSTOMER<EB.CUS.RELATION.CODE>
        Y.RISK.CAT.INV = R.CUSTOMER<EB.CUS.LOCAL.REF,L.RISK.CAT.INV.POS>
        Y.ISSUER.TYPE  = R.CUSTOMER<EB.CUS.LOCAL.REF,L.ISSUER.TYPE.POS>
        Y.L.CU.RNC = R.CUSTOMER<EB.CUS.LOCAL.REF,L.CU.RNC.POS>
        Y.NATIONALITY = R.CUSTOMER<EB.CUS.NATIONALITY>
        Y.MNTHLY.OUT = R.CUSTOMER<EB.CUS.NET.MONTHLY.OUT>
*20140317(S)
        NAME.1 = R.CUSTOMER<EB.CUS.NAME.1,Y.LANG.CODE>
        NAME.2 = R.CUSTOMER<EB.CUS.NAME.2,Y.LANG.CODE>

        IF NAME.1 EQ '' THEN
            NAME.1 = R.CUSTOMER<EB.CUS.NAME.1,1>
        END

        IF NAME.2 EQ '' THEN
            NAME.2 = R.CUSTOMER<EB.CUS.NAME.2,1>
        END

        Y.ISSUER.DES = NAME.1:" ":NAME.2

*20140317(E)

    END
*
RETURN

ASSING.FIN.VAR:
*--------------
    C$SPARE(451) = Y.ISSUER.ID
    C$SPARE(452) = Y.ISSUER.DES
    C$SPARE(453) = Y.ACCT.CODE.VAL
    C$SPARE(454) = Y.L.INV.FACILITY
    C$SPARE(455) = Y.SUB.DESC
    C$SPARE(456) = Y.FIN.AC.OPN.DT
    C$SPARE(457) = Y.FIN.MM.OPN.DT
    C$SPARE(458) = Y.INT.RATE
    C$SPARE(459) = NO.OF.MONTHS
    C$SPARE(460) = Y.VAL.REPORT
    C$SPARE(461) = Y.BOOK.VALUE
    C$SPARE(462) = Y.MARKET.VALUE
    C$SPARE(463) = Y.PART.RATE
    C$SPARE(464) = Y.CURRENCY
    C$SPARE(465) = Y.INVST.STATUS
    C$SPARE(466) = Y.RISK.CAT.INV
    C$SPARE(467) = Y.PROV.INV.TOT
    C$SPARE(468) = Y.MNTHLY.OUT
    C$SPARE(469) = Y.ISSUE.DATE
    C$SPARE(470) = Y.LINK.TYPE
    C$SPARE(471) = Y.EARLY.RET
    C$SPARE(472) = Y.PENALTY.RET
    C$SPARE(473) = Y.PROV.INV.TOT
    C$SPARE(474) = Y.STOCK.EXCH.DES
    C$SPARE(475) = Y.INT.ACCD
    C$SPARE(476) = Y.ACCT.INT
    C$SPARE(477) = Y.RATE.REV
    C$SPARE(478) = Y.RATING
    C$SPARE(479) = Y.DEAL.TICKET
    C$SPARE(480) = Y.ACCT.DESC
    C$SPARE(481) = Y.INT.RECEIVABLE
    C$SPARE(482) = ''
    C$SPARE(483) = ''
    C$SPARE(484) = Y.AGENCY.ID
    C$SPARE(485) = Y.CUST.TYPE
    C$SPARE(486) = Y.ISSUER.TYPE
    C$SPARE(487) = Y.NATIONALITY
    C$SPARE(488) = Y.COUPON
    C$SPARE(489) = Y.COLL.ISS
    C$SPARE(490) = Y.RATE.RETN
    C$SPARE(491) = Y.ISIN.CODE
*
RETURN

READ.REDO.AZACC.DESC:
*-------------------
*
    CALL F.READ(FN.REDO.AZA.DES,Y.REDO.AZ.DES,R.REDO.AZA.DES,F.REDO.AZA.DES,ERR.REDO.AZA.DES)
    IF R.REDO.AZA.DES THEN

        Y.ASSET.DESC = R.REDO.AZA.DES<AZACC.DESC>
        FINDSTR Y.ACCT.CODE IN R.REDO.AZA.DES<AZACC.ASSET.TYPE> SETTING AZ.DES.POS1,AZ.DES.POS2,AZ.DES.POS3 THEN
            Y.ACCT.CODE.VAL  = Y.ASSET.DESC<AZ.DES.POS1,AZ.DES.POS2>
        END

        CALL F.READ(FN.SEC.ACC.MAS,Y.SEC.ACC.MAS,R.SEC.ACC.MAS,F.SEC.ACC.MAS,ERR.SEC.ACC.MAS)
        IF R.SEC.ACC.MAS THEN

            Y.INT.RECD.CAT = R.SEC.ACC.MAS<SC.SAM.INT.RECD.CAT>
            AZ.DES.POS1 = '';AZ.DES.POS2 = '';AZ.DES.POS3 = ''
            FINDSTR Y.INT.RECD.CAT IN R.REDO.AZA.DES<AZACC.ASSET.TYPE> SETTING AZ.DES.POS1,AZ.DES.POS2,AZ.DES.POS3 THEN
                Y.ACCT.INT = R.REDO.AZA.DES<AZACC.DESC><AZ.DES.POS1,AZ.DES.POS2>
            END

            Y.DISCOUNT.CAT = R.SEC.ACC.MAS<SC.SAM.DISCOUNT.CAT>
            AZ.DES.POS1 = '';AZ.DES.POS2 = '';AZ.DES.POS3 = ''
            FINDSTR Y.DISCOUNT.CAT IN R.REDO.AZA.DES<AZACC.ASSET.TYPE> SETTING AZ.DES.POS1,AZ.DES.POS2,AZ.DES.POS3 THEN
                Y.ACCT.DESC = R.REDO.AZA.DES<AZACC.DESC><AZ.DES.POS1,AZ.DES.POS2>
            END
        END
    END
*
RETURN


ASSIGN.ISS.ID:
*-------------
*
    IF Y.L.CU.RNC EQ '' THEN
        Y.LGL.ID = R.CUSTOMER<EB.CUS.LEGAL.ID,1>
        Y.ISSUER.ID  = Y.NATIONALITY:Y.LGL.ID
    END ELSE
        Y.ISSUER.ID = Y.L.CU.RNC
    END
*
RETURN


FIND.ISIN.CODE:
*--------------
    Y.ALT.SEC.ID = R.SEC.MAS<SC.SCM.ALT.SECURITY.ID>
    FINDSTR "ALTISIN" IN Y.ALT.SEC.ID<1,1> SETTING ALTIS.POS THEN
        Y.ISIN.CODE  = R.SEC.MAS<SC.SCM.ALT.SECURITY.NO,ALTIS.POS>
    END
*
RETURN

CLASSIFY.COMPANY:
*----------------
    CALL F.READ(FN.EB.RATING,Y.RATING,R.EB.RATING,F.EB.RATING,ERR.EB.RATING)
    IF R.EB.RATING THEN
        Y.AGENCY.ID = R.EB.RATING<EB.RAT.AGENCY.ID,1>
    END
*
RETURN

READ.TRN.CON.DATE:
*-----------------
    CALL F.READ(FN.TRN.CON.DATE,Y.INV.DET,R.TRN.CON.DATE,F.TRN.CON.DATE,ERR.TRN.CON.DATE)
    IF R.TRN.CON.DATE THEN
        Y.DEAL.CNT = DCOUNT(R.TRN.CON.DATE,@FM)
        Y.DEAL.INT = 1
        LOOP
        WHILE Y.DEAL.INT LE Y.DEAL.CNT
            Y.SEC.TRADE.ID = FIELD(R.TRN.CON.DATE<Y.DEAL.INT>,'.',11)
*            Y.SEC.TRADE.ID = FIELD(Y.DEAL.TICKET,'.',11)
            Y.DEAL.TICKET = RIGHT(Y.SEC.TRADE.ID,10)
            GOSUB READ.SEC.TRADE
            GOSUB FIND.PROV.INV
            Y.DEAL.INT += 1
        REPEAT
*
    END
RETURN

READ.SEC.TRADE:
*---------------

    CALL F.READ(FN.SEC.TRADE,Y.SEC.TRADE.ID,R.SEC.TRADE,F.SEC.TRADE,ERR.SEC.TRADE)
    IF R.SEC.TRADE THEN
        Y.COUPON = R.SEC.TRADE<SC.SBS.LOCAL.REF,L.SEC.COUPON.TYPE.POS>
        Y.RATE.RETN  = R.SEC.TRADE<SC.SBS.LOCAL.REF,L.SC.TRN.YIELD.POS>
        Y.SC.PORTFOLIO.ID = R.SEC.TRADE<SC.SBS.CUST.SEC.ACC>
    END
*
RETURN

FIND.INT.RECEIVABLE:
*------------------

    R.EB.CONT.BAL = ""
    ERR.EB.CONT.BAL = ""
    CALL F.READ(FN.EB.CONT.BAL,Y.REDO.AZ.DES,R.EB.CONT.BAL,F.EB.CONT.BAL,ERR.EB.CONT.BAL)
    IF R.EB.CONT.BAL THEN
        Y.TYP.SYS = R.EB.CONT.BAL<ECB.TYPE.SYSDATE>
        Y.TYP.SYS.CNT = DCOUNT(Y.TYP.SYS,@VM)
        START.TYP.CNT = "1"
        LOOP
        WHILE START.TYP.CNT LE Y.TYP.SYS.CNT
            Y.DISCOUNT.CAT = R.SEC.ACC.MAS<SC.SAM.DISCOUNT.CAT>
            Y.ASSET.TYPE = R.EB.CONT.BAL<ECB.TYPE.SYSDATE,START.TYP.CNT>
            Y.ASSET.TYPE = FIELD(Y.ASSET.TYPE,"-",1)
            IF Y.ASSET.TYPE EQ Y.DISCOUNT.CAT THEN
                Y.OPEN.BAL.VAL = R.EB.CONT.BAL<ECB.OPEN.BALANCE,START.TYP.CNT>
                Y.OPEN.BAL = SUM(Y.OPEN.BAL.VAL)
                Y.CREDIT.MVMT.VAL = R.EB.CONT.BAL<ECB.CREDIT.MVMT,START.TYP.CNT>
                Y.CREDIT.MVMT = SUM(Y.CREDIT.MVMT.VAL)
                Y.DEBIT.MVMT.VAL = R.EB.CONT.BAL<ECB.DEBIT.MVMT,START.TYP.CNT>
                Y.DEBIT.MVMT = SUM(Y.DEBIT.MVMT.VAL)
                IF Y.OPEN.BAL OR Y.CREDIT.MVMT OR Y.DEBIT.MVMT THEN
                    Y.INT.REC = Y.OPEN.BAL + Y.CREDIT.MVMT + Y.DEBIT.MVMT
                END
                IF Y.INT.REC THEN
                    Y.INT.RECEIVABLE += Y.INT.REC
                END
            END
            START.TYP.CNT += 1
        REPEAT
    END
*
RETURN

FIND.MKT.VALUE:
*--------------

    Y.SC.POS.ASSET  = Y.SEC.ACC.MAS:".":Y.SUB.ASSET.TYP:".":Y.ASSET.TYPE.CODE
    CALL F.READ(FN.SC.POS.ASSET,Y.SC.POS.ASSET,R.SC.POS.ASSET,F.SC.POS.ASSET,ERR.SC.POS.ASSET)
    Y.SC.PAS.SEC.NO = R.SC.POS.ASSET<SC.PAS.SECURITY.NO>
    CHANGE @VM TO @FM IN Y.SC.PAS.SEC.NO
    IF R.SC.POS.ASSET THEN
        LOCATE Y.SEC.MAS IN Y.SC.PAS.SEC.NO<1> SETTING Y.SEC.MAS.POS THEN
            Y.MARKET.VALUE = R.SC.POS.ASSET<SC.PAS.ESTIMATION,Y.SEC.MAS.POS>
            Y.INT.ACCD = R.SC.POS.ASSET<SC.PAS.ACCRUED.INT,Y.SEC.MAS.POS>
        END
    END
*
RETURN

FIND.SEC.STATUS.INV:
*-------------------
    IF Y.MAT.DATE NE '' THEN
        IF Y.TODAY LT Y.MAT.DATE THEN
            Y.INVST.STATUS = "V"
        END ELSE
            Y.INVST.STATUS = "E"
        END
    END ELSE
        Y.INVST.STATUS = "V"
    END
*
RETURN

BUY.OR.SELL:
*------------
    Y.STOCK.EXCH.ID = R.SEC.MAS<SC.SCM.STOCK.EXCHANGE>
    CALL F.READ(FN.STCK.EXCH,Y.STOCK.EXCH.ID,R.STCK.EXCH,F.STCK.EXCH,ERR.STCK.EXCH)
    IF R.STCK.EXCH THEN
        Y.STOCK.EXCH.DES = R.STCK.EXCH<SC.STE.DESCRIPTION,1>
    END
*
RETURN

TYPE.OF.CUSTOMER:
*----------------
    Y.CU.TIPO.CL = R.CUSTOMER<EB.CUS.LOCAL.REF,L.CU.TIPO.CL.POS>

    Y.PROG.GRP = ''
    Y.REL.REQ = ''
    OUT.ARR = ''
    CALL REDO.S.REP.CUSTOMER.EXTRACT(Y.CUSTOMER,Y.PROG.GRP,Y.REL.REQ,OUT.ARR)
    Y.CUST.TYPE.VAL = FIELD(OUT.ARR,@FM,2)
    Y.CUST.TYPE = Y.CUST.TYPE.VAL[1,2]
*
RETURN

FIND.PROV.INV:
*------------

    CALL F.READ(FN.REDO.CUS.PROV,Y.CUSTOMER,R.REDO.CUS.PROV,F.REDO.CUS.PROV,ERR.REDO.CUS.PROV)
    IF R.REDO.CUS.PROV THEN
        Y.PROV.SC.PORTFOLIO.ID = R.REDO.CUS.PROV<CUS.PROV.SC.PORTFOLIO.ID>
        Y.CUS.PROV.SECURITY.NO = R.REDO.CUS.PROV<CUS.PROV.SECURITY.NO>

        Y.SC.INT = 1
        Y.SC.TOT = DCOUNT(Y.SC.PORTFOLIO.ID,@VM)
        Y.PROV.INV.TOT = ''
        LOOP
        WHILE Y.SC.INT LE Y.SC.TOT
            Y.SC.PORTFOLIO = Y.SC.PORTFOLIO.ID<1,Y.SC.INT>
            CHANGE @SM TO @VM IN Y.PROV.SC.PORTFOLIO.ID
            CHANGE @VM TO @FM IN Y.PROV.SC.PORTFOLIO.ID
            LOCATE Y.SC.PORTFOLIO IN Y.PROV.SC.PORTFOLIO.ID<1> SETTING PROV.POS THEN
                Y.SM.CNT = '1'
                Y.SM.TOT = DCOUNT(Y.CUS.PROV.SECURITY.NO,@SM)
                LOOP
                WHILE Y.SM.CNT LE Y.SM.TOT
                    IF Y.SEC.MAS EQ Y.CUS.PROV.SECURITY.NO<1,1,Y.SM.CNT> THEN

                        Y.PROV.INV = ''
                        Y.SC.CAP = ''
                        Y.SC.INT.AMT = ''
                        Y.SC.CAP = R.REDO.CUS.PROV<CUS.PROV.SC.CAP.PROV,PROV.POS,Y.SM.CNT>
*20140328(S)
                        Y.SC.CAP.AMT = TRIM(Y.SC.CAP,",","A")

                        Y.SC.INT.AMT = R.REDO.CUS.PROV<CUS.PROV.SC.INT.PROV,PROV.POS,Y.SM.CNT>
                        Y.SC.INT.AMOUNT = TRIM(Y.SC.INT.AMT,",","A")

                        Y.PROV.INV = Y.SC.CAP.AMT + Y.SC.INT.AMOUNT
*20140328(E)
                        Y.PROV.INV.TOT = Y.PROV.INV
                    END
                    Y.SM.CNT += 1
                REPEAT
                GOSUB FIND.LINK.TYPE
            END
            Y.SC.INT += 1
        REPEAT
    END
*
RETURN

FIND.LINK.TYPE:
*--------------
    Y.RELATION.CODE = R.CUSTOMER<EB.CUS.RELATION.CODE>
    Y.RELATION.CODE.CNT = DCOUNT(Y.RELATION.CODE,@VM)
    Y.START.REL = "1"
    LOOP
    WHILE Y.START.REL LE Y.RELATION.CODE.CNT
        Y.REL.ACT.VAL.ARR = CHANGE(Y.REL.VAL.ARR,@SM,@VM)
        Y.REL.ACT.DIS.ARR = CHANGE(Y.REL.DIS.ARR,@SM,@VM)
        LOCATE Y.RELATION.CODE<1,Y.START.REL> IN Y.REL.ACT.VAL.ARR<1,1> SETTING L.REL.POS THEN
            Y.LINK.TYPE = Y.REL.ACT.DIS.ARR<1,L.REL.POS>
        END

*
        IF Y.RELATION.CODE<1,Y.START.REL> EQ "301" OR Y.RELATION.CODE<1,Y.START.REL> EQ "302" THEN
            Y.PART.RATE = R.CUSTOMER<EB.CUS.ROLE.MORE.INFO,1>
        END ELSE
            Y.PART.RATE = R.SEC.MAS<SC.SCM.LOCAL.REF,L.PARTICIP.RATE.POS>
        END
*
        GOSUB ASSING.FIN.VAR
        GOSUB ASSING.FIN.VAL
        GOSUB READ.REDO.REPORT.TEMP

        Y.START.REL += 1
    REPEAT
*
RETURN

FIND.CATGORY.VAL:
*----------------
    Y.SUB.ASSET.TYP = R.SEC.MAS<SC.SCM.SUB.ASSET.TYPE>
    CALL F.READ(FN.SUB.ASSET.TYPE,Y.SUB.ASSET.TYP,R.SUB.ASSET.TYPE,F.SUB.ASSET.TYPE,ERR.SUB.ASSET.TYPE)
    IF R.SUB.ASSET.TYPE THEN
        Y.SUB.DESC = R.SUB.ASSET.TYPE<SC.CSG.DESCRIPTION,Y.LANG.CODE>
*20140317(S)
        IF Y.SUB.DESC EQ '' THEN
            Y.SUB.DESC = R.SUB.ASSET.TYPE<SC.CSG.DESCRIPTION>
        END
*20140317(E)

        Y.ASSET.TYPE.CODE = R.SUB.ASSET.TYPE<SC.CSG.ASSET.TYPE.CODE>
    END
*
RETURN

FIND.MAT.DATE:
*-------------
    IF Y.MAT.DATE THEN
        Y.MT.OPN.DT.YY = Y.MAT.DATE[1,4]
        Y.MT.OPN.DT.MM = Y.MAT.DATE[5,2]
        Y.MT.OPN.DT.DT = Y.MAT.DATE[7,2]
        Y.FIN.MM.OPN.DT = Y.MT.OPN.DT.DT:"/":Y.MT.OPN.DT.MM:"/":Y.MT.OPN.DT.YY
    END ELSE
        Y.FIN.MM.OPN.DT = ""
    END
*
RETURN

DATE.OF.ACQUIS:
*--------------

    Y.ACQU.DATE = R.SEC.POS<SC.SCP.HELD.SINCE>
    IF Y.ACQU.DATE THEN
        Y.AC.OPN.DT.YY = Y.ACQU.DATE[1,4]
        Y.AC.OPN.DT.MM = Y.ACQU.DATE[5,2]
        Y.AC.OPN.DT.DT = Y.ACQU.DATE[7,2]
        Y.FIN.AC.OPN.DT = Y.AC.OPN.DT.DT:"/":Y.AC.OPN.DT.MM:"/":Y.AC.OPN.DT.YY
    END ELSE
        Y.FIN.AC.OPN.DT = ""
    END
*
RETURN

CALC.INT.SCH:
*------------
    Y.BND.SHR = R.SEC.MAS<SC.SCM.BOND.OR.SHARE>
    IF Y.BND.SHR EQ "B" THEN
        Y.VAL.REPORT = ''
        Y.INT.SCH = R.SEC.MAS<SC.SCM.NO.OF.PAYMENTS>

        LOCATE Y.INT.SCH IN Y.SCH.VAL.ARR<1,1> SETTING L.SCH.POS THEN
            Y.VAL.REPORT = Y.SCH.DIS.ARR<1,L.SCH.POS>
        END ELSE
            Y.VAL.REPORT = 'V'
        END
    END
*
RETURN

CALC.MONTH.CNT:
*--------------
    Y.LAST.DAY = R.DATES(EB.DAT.LAST.WORKING.DAY)
    CALL EB.NO.OF.MONTHS(Y.LAST.DAY,Y.MAT.DATE,NO.OF.MONTHS)
*
RETURN

ASSING.FIN.VAL:
*--------------
* Pass arguments to RCL and get the return message
*-------------------------------------------------

    Y.FINAL.MSG = ''
    RCL.ID  = Y.RCL.ID
    MAP.FMT = "MAP"
    APP     = FN.SEC.POS
    R.APP   = R.SEC.POS
    CALL RAD.CONDUIT.LINEAR.TRANSLATION (MAP.FMT,RCL.ID,APP,Y.AA.ARR.ID,R.APP,R.RETURN.MSG,ERR.MSG)
    Y.FINAL.MSG = R.RETURN.MSG
*
RETURN
*-----------------------------------------------------------------------------------------------------------------
READ.REDO.REPORT.TEMP:
*-----------------------------------------------------------------------------------------------------------------
*Read REDO.REPORT.TEMP
*If Record exits then append R.RETURN.MSG  to R.REDO.REPORT.TEMP
*Else assign R.RETURN.MSG to R.REDO.REPORT.TEMP
*-----------------------------------------------------------------------------------------------------------------
    R.REDO.REPORT.TEMP = ""

    IF Y.FINAL.MSG THEN

        WRITESEQ Y.FINAL.MSG APPEND TO Y$.SEQFILE.PTR ELSE
            Y.ERR.MSG = "Unable to Write '":Y.FILE.NAME:"'"
            GOSUB RAISE.ERR.C.22
        END
    END

RETURN
*-----------------------------------------------------------------------------------------------------------------
RAISE.ERR.C.22:
*-----------------------------------------------------------------------------------------------------------------
*Handling Fatal error to halt the process
*-----------------------------------------------------------------------------------------------------------------
    MON.TP    = "01"
    Y.ERR.MSG = "Record not found"
    REC.CON   = "OA01-":Y.AA.ARR.ID:Y.ERR.MSG
    DESC      = "OA01-":Y.AA.ARR.ID:Y.ERR.MSG
    INT.CODE  = 'REP001'
    INT.TYPE  = 'ONLINE'
    BAT.NO    = ''
    BAT.TOT   = ''
    INFO.OR   = ''
    INFO.DE   = ''
    ID.PROC   = ''
    EX.USER   = ''
    EX.PC     = ''
    CALL REDO.INTERFACE.REC.ACT(INT.CODE,INT.TYPE,BAT.NO,BAT.TOT,INFO.OR,INFO.DE,ID.PROC,MON.TP,DESC,REC.CON,EX.USER,EX.PC)
*
RETURN
*------------------------------------------------------------------Final End-------------------------------------------
END
