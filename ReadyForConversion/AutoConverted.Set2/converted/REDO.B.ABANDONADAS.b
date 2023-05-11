SUBROUTINE REDO.B.ABANDONADAS(ACCOUNT.ID)
*-------------------------------------------------------------------------------
* Company Name      : PAGE SOLUTIONS, INDIA
* Developed By      : Nirmal.P
* Reference         :
*-------------------------------------------------------------------------------
* Subroutine Type   : B
* Attached to       :
* Attached as       : Multi threaded Batch Routine.
*-------------------------------------------------------------------------------
* Input / Output :
*----------------
* IN     :
* OUT    :
*-------------------------------------------------------------------------------
* Description:
*-------------------------------------------------------------------------------
* Modification History
*-----------------------------------------------------------------------------------------------------------------
* Defect Reference       Modified By                    Date of Change        Change Details
*(RTC/TUT/PACS)                                        (YYYY-MM-DD)
*-----------------------------------------------------------------------------------------------------------------
* PACS00392015          Ashokkumar.V.P                  19/11/2014           Changes based on mapping.
*-----------------------------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.EB.LOOKUP
    $INSERT I_F.CUSTOMER
    $INSERT I_F.CURRENCY
    $INSERT I_F.EB.CONTRACT.BALANCES
    $INSERT I_F.RE.STAT.REP.LINE
*   $INSERT I_F.ACCOUNT
    $INSERT I_F.REDO.AZACC.DESC
    $INSERT I_F.REDO.H.REPORTS.PARAM
    $INSERT I_REDO.B.ABANDONADAS.COMMON
    $INSERT I_REDO.GENERIC.FIELD.POS.COMMON

    GOSUB INIT
    GOSUB PROCESS.PARA
RETURN

**************
PROCESS.PARA:
**************
    CALL F.READ(FN.ACCOUNT,ACCOUNT.ID,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
    Y.LAST.CR = R.ACCOUNT<AC.DATE.LAST.CR.CUST>
    Y.LAST.DR = R.ACCOUNT<AC.DATE.LAST.DR.CUST>
    Y.CATEGORY = R.ACCOUNT<AC.CATEGORY>
    Y.INST.TYPE = R.ACCOUNT<AC.LOCAL.REF,L.INV.FACILITY.POS>
    Y.FIELD.NAME = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.FIELD.NAME>
    Y.FIELD.NAME = CHANGE(Y.FIELD.NAME,@VM,@FM)
    LOCATE 'IT.SEL.CODES' IN Y.FIELD.NAME SETTING IT.POS THEN
        IT.LIST = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.FIELD.VALUE><1,IT.POS>
        IT.LIST = CHANGE(IT.LIST,@SM,@FM)
        LOCATE Y.CATEGORY IN IT.LIST SETTING CA.POS THEN
        END ELSE
            RETURN
        END
    END
    Y.RECENT.DATE = ''
*    Y.ALT.ACCT.TYPE=R.ACCOUNT<AC.ALT.ACCT.TYPE>
*    Y.ALT.ACCT.ID=R.ACCOUNT<AC.ALT.ACCT.ID>
*    LOCATE 'ALTERNO1' IN Y.ALT.ACCT.TYPE<1,1> SETTING ALT.TYPE.POS THEN
*        Y.PREV.ACCOUNT = Y.ALT.ACCT.ID<1,ALT.TYPE.POS>
*    END
*    IF NOT(Y.PREV.ACCOUNT) THEN
*        Y.PREV.ACCOUNT = ACCOUNT.ID
*    END
    C$SPARE(478) = ACCOUNT.ID
    IF Y.LAST.CR LT Y.LAST.DR THEN
        Y.RECENT.DATE = Y.LAST.DR
    END ELSE
        Y.RECENT.DATE = Y.LAST.CR
    END
    Y.LOOKUP.ID = 'L.AC.STATUS1*ABANDONED'
    CALL F.READ(FN.EB.LOOKUP,Y.LOOKUP.ID,R.EB.LOOKUP,F.EB.LOOKUP,LOOKUP.ERR)
    Y.ARRIVED.DATE = ''
    IF R.EB.LOOKUP THEN
        Y.AC.MONTHS = R.EB.LOOKUP<EB.LU.LOCAL.REF,Y.AC.MONTHS.POS>
        SIGN = '+'
        CALL CALENDAR.DAY(Y.RECENT.DATE,SIGN,Y.AC.MONTHS)
        Y.ARRIVED.DATE =  Y.AC.MONTHS
    END
*
    IF (Y.ARRIVED.DATE GE YSTART.DATE AND Y.ARRIVED.DATE LE Y.PR.DATE) THEN
        GOSUB FETCH.DATA
    END
RETURN

FETCH.DATA:
*----------
    CUSTOMER.ID = R.ACCOUNT<AC.CUSTOMER>
    ACC.CUSTOMER = CUSTOMER.ID
    GOSUB CUSTOMER.INFO

    C$SPARE(451) = Y.CUS.TYPE
    C$SPARE(452) = Y.CUS.ID
    C$SPARE(453) = Y.CUS.NAME
    C$SPARE(454) = Y.LAST.NAME
    C$SPARE(455) = Y.ACCT.ADDR
    C$SPARE(456) = Y.ACCT.TEL
    C$SPARE(457) = Y.ACCT.OFF.PHONE
    C$SPARE(458) = Y.ACCT.MOB
    C$SPARE(475) = Y.EMAIL
    C$SPARE(459) = Y.INST.TYPE

    Y.LAST.TXN = Y.RECENT.DATE
    IF Y.LAST.TXN THEN
        C$SPARE(460) = Y.LAST.TXN[7,2]:'/':Y.LAST.TXN[5,2]:'/':Y.LAST.TXN[1,4]
    END
    Y.ORG.BAL = ''; Y.WORK.BAL = ''
    Y.ORG.BAL = R.ACCOUNT<AC.WORKING.BALANCE>
    Y.WORK.BAL = R.ACCOUNT<AC.OPEN.ACTUAL.BAL>
    IF Y.ORG.BAL EQ 0 OR Y.ORG.BAL EQ '' THEN
        YZERO.FLG = 'ZERO'
    END
    Y.CURRENCY = R.ACCOUNT<AC.CURRENCY>
    IF Y.CURRENCY NE LCCY THEN
        R.CURRENCY = ''; CURR.ERR = ''; Y.MID.RATE = ''
        CALL F.READ(FN.CURRENCY,Y.CURRENCY,R.CURRENCY,F.CURRENCY,CURR.ERR)
        LOCATE '1' IN R.CURRENCY<EB.CUR.CURRENCY.MARKET,1> SETTING CCY.MKT.POS THEN
            Y.MID.RATE = R.CURRENCY<EB.CUR.MID.REVAL.RATE,CCY.MKT.POS>
        END
        Y.WORK.BAL = Y.WORK.BAL * Y.MID.RATE
    END
    C$SPARE(461) = Y.WORK.BAL

    LOCATE 'AT.SEL.CODES' IN Y.FIELD.NAME SETTING AT.POS THEN
        AT.LIST = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.FIELD.VALUE><1,AT.POS>
        AT.INT = AT.LIST<1,1,1>
        AT.CR = AT.LIST<1,1,2>
        AT.DB = AT.LIST<1,1,3>
    END
    R.EB.CONTRACT.BALANCES = ''; EB.CONT.BAL.ERR = ''
    CALL F.READ(FN.EB.CONTRACT.BALANCES,ACCOUNT.ID,R.EB.CONTRACT.BALANCES,F.EB.CONTRACT.BALANCES,EB.CONT.BAL.ERR)
*
    GOSUB CALC.INT.AMT

    C$SPARE(462) = Y.INT.AMT.LCY
    GOSUB DEF.DESC.CRF.DC
    GOSUB DEF.DESC.CRF.5000
    C$SPARE(463) = Y.AZACC.DESC
    C$SPARE(464) = Y.AZACC.INT.DESC

    IF Y.CURRENCY EQ LCCY THEN
        C$SPARE(465) = Y.INT.AMT.LCY
    END

    CUSTOMER.ID = R.ACCOUNT<AC.JOINT.HOLDER,1>
    YRELAT.CDE = R.ACCOUNT<AC.RELATION.CODE,1>
    GOSUB CUSTOMER.INFO

    C$SPARE(466) = Y.CUS.TYPE
    C$SPARE(467) = Y.CUS.ID
    C$SPARE(468) = Y.CUS.NAME
    C$SPARE(469) = Y.LAST.NAME
    C$SPARE(470) = Y.ACCT.ADDR
    C$SPARE(471) = Y.ACCT.TEL
    C$SPARE(472) = Y.ACCT.OFF.PHONE
    C$SPARE(473) = Y.ACCT.MOB
    C$SPARE(476) = Y.EMAIL
    C$SPARE(479) = Y.CATEGORY
    LOCATE 'RC.SEL.CODES' IN Y.FIELD.NAME SETTING RC.POS THEN
        RC.LIST = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.FIELD.VALUE><1,RC.POS>
        RC.LIST = CHANGE(RC.LIST,@SM,@FM)
    END

    CU.POS = ''; Y.REL.BTW.CUS = ''; Y.OPENING.DATE = ''
    LOCATE YRELAT.CDE IN RC.LIST SETTING RC1.POS THEN
        Y.REL.BTW.CUS = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.DISPLAY.TEXT><1,RC.POS,RC1.POS>
    END
    C$SPARE(474) = Y.REL.BTW.CUS

    Y.OPENING.DATE = R.ACCOUNT<AC.OPENING.DATE>
    IF Y.OPENING.DATE THEN
        C$SPARE(477) = Y.OPENING.DATE[7,2]:'/':Y.OPENING.DATE[5,2]:'/':Y.OPENING.DATE[1,4]
    END

    GOSUB MAP.RCL.RECORD
RETURN

CUSTOMER.INFO:
*-------------
    Y.CUS.TYPE = ''; Y.CUS.ID = ''; Y.CUS.NAME = ''; Y.LAST.NAME = ''
    Y.ACCT.ADDR = ''; Y.ACCT.TEL = ''; Y.ACCT.OFF.PHONE = ''; Y.ACCT.MOB = ''
    Y.CIDENT = ''; Y.RNC = ''; Y.FOREIGN = ''; Y.TIPO.CL = ''; CUS.ERR = ''
    Y.NATIONALITY = ''; Y.LEGAL.ID = ''; Y.REL.CODE = ''; Y.ADDRESS = ''
    Y.STREET = ''; Y.TOWN.COUNTRY = ''; Y.TEL.TYPE = ''; Y.TEL.AREA = ''
    Y.TEL.NO = ''; Y.TEL.EXT = ''; R.CUSTOMER = ''; Y.EMAIL = ''
    Y.ADDR = ''; Y.L.CU.ACTANAC = ''; Y.L.CU.NOUNICO = ''

    CALL F.READ(FN.CUSTOMER,CUSTOMER.ID,R.CUSTOMER,F.CUSTOMER,CUS.ERR)
    LOCATE 'CN.SEL.CODES' IN Y.FIELD.NAME SETTING CN.POS THEN
        CN.LIST = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.FIELD.VALUE><1,CN.POS>
        CN.DIS.LIST = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.DISPLAY.TEXT><1,CN.POS>
    END
    Y.CIDENT = R.CUSTOMER<EB.CUS.LOCAL.REF,L.CU.CIDENT.POS>
    Y.RNC = R.CUSTOMER<EB.CUS.LOCAL.REF,L.CU.RNC.POS>
    Y.FOREIGN = R.CUSTOMER<EB.CUS.LOCAL.REF,L.CU.FOREIGN.POS>
    Y.TIPO.CL = R.CUSTOMER<EB.CUS.LOCAL.REF,L.CU.TIPO.CL.POS>
    Y.NATIONALITY = R.CUSTOMER<EB.CUS.NATIONALITY>
    Y.LEGAL.ID = R.CUSTOMER<EB.CUS.LEGAL.ID,1>
    Y.L.CU.ACTANAC = R.CUSTOMER<EB.CUS.LOCAL.REF,L.CU.ACTANAC.POS>
    Y.L.CU.NOUNICO = R.CUSTOMER<EB.CUS.LOCAL.REF,L.CU.NOUNICO.POS>

    BEGIN CASE
        CASE Y.CIDENT NE ''
            Y.CUS.TYPE = CN.LIST<1,1,1>
        CASE Y.RNC NE ''
            Y.CUS.TYPE = CN.LIST<1,1,2>
        CASE Y.LEGAL.ID
            Y.CUS.TYPE = CN.LIST<1,1,4>
        CASE Y.FOREIGN NE ''
            Y.CUS.TYPE = CN.LIST<1,1,3>
        CASE Y.L.CU.NOUNICO NE ''
            Y.CUS.TYPE = CN.LIST<1,1,4>
        CASE Y.L.CU.ACTANAC NE ''
            Y.CUS.TYPE = CN.LIST<1,1,4>
    END CASE

    Y.CUSTOMER.ID = CUSTOMER.ID
    Y.PRODUCT.GROUP = ''; Y.REL.CODE = ''
    IF R.CUSTOMER<EB.CUS.RELATION.CODE> NE '' THEN
        Y.REL.CODE = 'Y'
    END
    OUT.ARR = ''
    CALL REDO.S.REG.CUSTOMER.EXTRACT(Y.CUSTOMER.ID,Y.PRODUCT.GROUP,Y.REL.CODE,OUT.ARR)
    Y.CUS.ID = FIELD(OUT.ARR,@FM,1)
    IF Y.L.CU.NOUNICO NE '' THEN
        Y.CUS.ID = Y.L.CU.NOUNICO
    END
    IF Y.L.CU.ACTANAC NE '' THEN
        Y.CUS.ID = Y.L.CU.ACTANAC
    END
    IF NOT(Y.CUS.ID) THEN
        Y.CUS.ID = 'DESCONOCIDO'
    END

    Y.CUS.NAME =  FIELD(OUT.ARR,@FM,3)
    IF Y.CUS.NAME EQ '' THEN
        Y.CUS.NAME = 'DESCONOCIDO'
    END

    Y.LAST.NAME = FIELD(OUT.ARR,@FM,4)
    IF Y.LAST.NAME EQ '' THEN
        Y.LAST.NAME = 'DESCONOCIDO'
    END

    Y.ADDRESS = R.CUSTOMER<EB.CUS.ADDRESS>
    Y.STREET = R.CUSTOMER<EB.CUS.STREET,1>
    Y.ADR.CNT = DCOUNT(Y.ADDRESS,@VM)
    IF Y.ADDRESS NE '' THEN
        FOR I.VAR = 1 TO Y.ADR.CNT
            Y.ADDR<-1> = Y.ADDRESS<1,I.VAR,1>
        NEXT I.VAR
        Y.ACCT.ADDR = CHANGE(Y.ADDR,@FM,' ')
        Y.ACCT.ADDR = Y.ACCT.ADDR:' ':Y.STREET
    END ELSE
        Y.TOWN.COUNTRY = R.CUSTOMER<EB.CUS.TOWN.COUNTRY,1>
        Y.ACCT.ADDR = Y.STREET:' ':Y.TOWN.COUNTRY
    END
    IF Y.ACCT.ADDR EQ '' THEN
        Y.ACCT.ADDR = 'DESCONOCIDO'
    END

    Y.TEL.TYPE = R.CUSTOMER<EB.CUS.LOCAL.REF,Y.TEL.TYPE.POS>
    LOCATE 'TT.SEL.CODES' IN Y.FIELD.NAME SETTING TT.POS THEN
        TT.LIST = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.FIELD.VALUE><1,TT.POS>
    END
    Y.TEL.TYPE = CHANGE(Y.TEL.TYPE,@SM,@FM)
    LOCATE TT.LIST<1,1,1> IN Y.TEL.TYPE SETTING TEL.NO.POS THEN
        Y.TEL.AREA = R.CUSTOMER<EB.CUS.LOCAL.REF,Y.TEL.AREA.POS,TEL.NO.POS>
        Y.TEL.NO = R.CUSTOMER<EB.CUS.LOCAL.REF,Y.TEL.NO.POS,TEL.NO.POS>
        Y.TEL.NO = FMT(Y.TEL.NO,"R(###-####)")
        Y.ACCT.TEL = Y.TEL.AREA:'-':Y.TEL.NO
    END

    LOCATE TT.LIST<1,1,2> IN Y.TEL.TYPE SETTING OFF.NO.POS THEN
        Y.TEL.EXT = R.CUSTOMER<EB.CUS.LOCAL.REF,Y.TEL.EXT.POS,OFF.NO.POS>
        IF Y.TEL.EXT THEN
            Y.TEL.EXT = FMT(Y.TEL.EXT,'R%10')
            Y.TEL.EXT = FMT(Y.TEL.EXT,'R(###-###-####)')
            Y.ACCT.OFF.PHONE = Y.TEL.EXT
        END
    END

    LOCATE TT.LIST<1,1,3> IN Y.TEL.TYPE SETTING MOB.POS THEN
        Y.TEL.EXT = R.CUSTOMER<EB.CUS.LOCAL.REF,Y.TEL.EXT.POS,MOB.POS>
        IF Y.TEL.EXT THEN
            Y.TEL.EXT = FMT(Y.TEL.EXT,'R%10')
            Y.TEL.EXT = FMT(Y.TEL.EXT,'R(###-###-####)')
            Y.ACCT.MOB = Y.TEL.EXT
        END
    END
    Y.EMAIL = R.CUSTOMER<EB.CUS.EMAIL.1,1>
RETURN

DEF.DESC.CRF.DC:
****************
    Y.REGULATORY.AC.ACC = ''; Y.IN.CONSOL.KEY = ''; YCRF.TYPE = ''
    IF R.EB.CONTRACT.BALANCES THEN
        Y.CONSOL.KEY = R.EB.CONTRACT.BALANCES<ECB.CONSOL.KEY>
        YCRF.TYPE = R.EB.CONTRACT.BALANCES<ECB.OPEN.ASSET.TYPE>
        Y.CONSOL.PART = FIELD(Y.CONSOL.KEY,'.',1,16)
        Y.IN.CONSOL.KEY = Y.CONSOL.PART:'.':YCRF.TYPE
        Y.VARIABLE = ''; Y.RPRTS = ''; Y.LINES = ''
        CALL RE.CALCUL.REP.AL.LINE(Y.IN.CONSOL.KEY,Y.RPRTS,Y.LINES,Y.VARIABLE)
        Y.LINE = Y.RPRTS:'.':Y.LINES
        CALL F.READ(FN.RE.STAT.REP.LINE,Y.LINE,R.LINE,F.RE.STAT.REP.LINE,REP.ERR)
        Y.REGULATORY.ACC.NO = R.LINE<RE.SRL.DESC,1>
        Y.AZACC.DESC = Y.REGULATORY.ACC.NO
    END
    IF NOT(Y.AZACC.DESC) THEN
        Y.AZACC.DESC = Y.CCAP.VAL.ARR
    END
RETURN

DEF.DESC.CRF.5000:
*****************
    Y.REGULATORY.AC.ACC = ''; Y.IN.CONSOL.KEY = ''
    IF R.EB.CONTRACT.BALANCES THEN
        Y.CONSOL.KEY = R.EB.CONTRACT.BALANCES<ECB.CONSOL.KEY>
        Y.CONSOL.PART = FIELD(Y.CONSOL.KEY,'.',1,16)
        Y.IN.CONSOL.KEY = Y.CONSOL.PART:'.':AT.INT
        Y.VARIABLE = ''; Y.RPRTS = ''; Y.LINES = ''
        CALL RE.CALCUL.REP.AL.LINE(Y.IN.CONSOL.KEY,Y.RPRTS,Y.LINES,Y.VARIABLE)
        Y.LINE = Y.RPRTS:'.':Y.LINES
        CALL F.READ(FN.RE.STAT.REP.LINE,Y.LINE,R.LINE,F.RE.STAT.REP.LINE,REP.ERR)
        Y.REGULATORY.ACC.NO = R.LINE<RE.SRL.DESC,1>
        Y.AZACC.INT.DESC = Y.REGULATORY.ACC.NO
    END
    IF NOT(Y.AZACC.INT.DESC) THEN
        Y.AZACC.INT.DESC = Y.CINT.VAL.ARR
    END
RETURN

CALC.INT.AMT:
*------------
    Y.INT.AMT.LCY = ''
    IF NOT(R.EB.CONTRACT.BALANCES) THEN
        RETURN
    END
    Y.SYS.DATE = R.EB.CONTRACT.BALANCES<ECB.TYPE.SYSDATE>
    Y.SYS.CNT = DCOUNT(Y.SYS.DATE,@VM)
    Y.OPEN.BAL = '0'; Y.CR.MVMT = '0'; Y.DB.MVMT = '0'; Y.TOTAL = '0'
    Y.INT.AMT.LCY = '0'
    FOR I.VAR = 1 TO Y.SYS.CNT
        Y.SYS.DT = Y.SYS.DATE<1,I.VAR>
        Y.SYS.INT = FIELD(Y.SYS.DT,'-',1)
        YINT.AMT.LCY = 0
        IF Y.SYS.INT EQ AT.LIST<1,1,1> THEN
            Y.OPEN.BAL = R.EB.CONTRACT.BALANCES<ECB.OPEN.BALANCE><1,I.VAR>
            Y.CR.MVMT =  R.EB.CONTRACT.BALANCES<ECB.CREDIT.MVMT><1,I.VAR>
            Y.DB.MVMT = ABS(R.EB.CONTRACT.BALANCES<ECB.DEBIT.MVMT><1,I.VAR>)
            Y.INT.AMT = Y.OPEN.BAL+Y.CR.MVMT-Y.DB.MVMT
            YINT.AMT.LCY = Y.INT.AMT.LCY + Y.INT.AMT
        END
        Y.INT.AMT.LCY += YINT.AMT.LCY
    NEXT I.VAR
    IF NOT(Y.INT.AMT.LCY) THEN
        Y.INT.AMT.LCY = R.ACCOUNT<AC.ACCR.CR.AMOUNT>
    END
    IF Y.INT.AMT.LCY EQ 0 THEN
        Y.INT.AMT.LCY = R.ACCOUNT<AC.ACCR.CR.AMOUNT>
    END
    IF Y.CURRENCY EQ LCCY AND Y.INT.AMT.LCY THEN
        Y.INT.AMT.LCY = Y.INT.AMT.LCY * Y.MID.RATE
    END
RETURN

MAP.RCL.RECORD:
*--------------
    MAP.FMT = 'MAP'
    ID.RCON.L = "REDO.RCL.ABAN"
    APP = FN.ACCOUNT
    ID.APP = ACCOUNT.ID
    R.APP = R.ACCOUNT
    CALL RAD.CONDUIT.LINEAR.TRANSLATION (MAP.FMT,ID.RCON.L,APP,ID.APP,R.APP,R.RETURN.MSG,ERR.MSG)
    IF R.RETURN.MSG THEN
        YFIN.ID = ''; YFIN.ID = Y.LAST.TXN:'-':ACCOUNT.ID:'-':YZERO.FLG
        CALL F.WRITE(FN.DR.REG.ABANDON.WORKFILE,YFIN.ID,R.RETURN.MSG)
    END
RETURN

INIT:
*****
    Y.INST.TYPE = ''; Y.LAST.TXN = ''; Y.WORK.BAL = ''; Y.INT.AMT.LCY = ''; Y.INST.TYPE = ''
    Y.P.ACCT.TYPE = ''; Y.I.ACCT.TYPE = ''; Y.INT.AMT.LCY = ''; Y.REL.BTW.CUS = ''; YZERO.FLG = ''
    Y.LAST.CR = ''; Y.LAST.DR = ''; Y.CATEGORY = ''; Y.RECENT.DATE = ''; C$SPARE(478) = ''
    R.ACCOUNT = ''; ACC.ERRR = ''; Y.RECENT.DATE = ''; C$SPARE(451) = ''; C$SPARE(452) = ''; C$SPARE(478) = ''
    C$SPARE(453) = ''; C$SPARE(454) = ''; C$SPARE(455) = ''; C$SPARE(456) = ''; C$SPARE(457) = ''
    C$SPARE(458) = ''; C$SPARE(459) = ''; C$SPARE(460) = ''; C$SPARE(461) = ''; C$SPARE(462) = ''
    C$SPARE(463) = ''; C$SPARE(464) = ''; C$SPARE(465) = ''; C$SPARE(466) = ''; C$SPARE(467) = ''
    C$SPARE(468) = ''; C$SPARE(469) = ''; C$SPARE(470) = ''; C$SPARE(471) = ''; C$SPARE(472) = ''
    C$SPARE(473) = ''; C$SPARE(474) = ''; C$SPARE(475) = ''; C$SPARE(476) = ''; C$SPARE(477) = ''
RETURN

END
