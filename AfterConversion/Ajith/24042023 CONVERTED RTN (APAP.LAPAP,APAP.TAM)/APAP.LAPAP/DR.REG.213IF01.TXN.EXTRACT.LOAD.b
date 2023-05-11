* @ValidationCode : MjoxNDY0NTU2ODA1OkNwMTI1MjoxNjgyMzIxMjA2NjY4OmFqaXRoOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 24 Apr 2023 12:56:46
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ajith
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.LAPAP
* @(#) DR.REG.213IF01.TXN.EXTRACT.LOAD Ported to jBASE 16:16:51  28 NOV 2017
*-----------------------------------------------------------------------------
SUBROUTINE DR.REG.213IF01.TXN.EXTRACT.LOAD
*-----------------------------------------------------------------------------
* Company Name   : APAP
* Developed By   :
* Program Name   : DR.REG.213IF01.TXN.EXTRACT
* Date           : 2-May-2013
*-----------------------------------------------------------------------------
* Description:
*------------
* This multi-thread job is meant for to extact the tranactions made over 10000 by individual Customer daily.
*-----------------------------------------------------------------------------
*
* Modification History :
* ----------------------
*   Date          Author               Modification Description
*
* 11-Aug-2014     V.P.Ashokkumar       PACS00309079 - Updated the field mapping and format
* 14-Oct-2014     V.P.Ashokkumar       PACS00309079 - Updated to filter AML transaction
* 23-Feb-2017     Bernard Gladin S    Modified based on the RTE process change
*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*24-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION   FM to@FM, SM to@SM , VM to@VM
*24-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION NO CHANGE
*----------------------------------------------------------------------------------------





*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_BATCH.FILES
    $INSERT I_F.DATES

    $INSERT I_DR.REG.213IF01.TXN.EXTRACT.COMMON
    $INSERT I_F.DR.REG.213IF01.PARAM
    $INSERT I_F.REDO.H.REPORTS.PARAM
    $INSERT I_F.CURRENCY


    GOSUB OPEN.FILES
    GOSUB INIT.PARA
RETURN
*----------------------------------------------------------
OPEN.FILES:
***********

    FN.CUSTOMER = 'F.CUSTOMER'  ; F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER, F.CUSTOMER)

    FN.ACCOUNT = 'F.ACCOUNT'  ; F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT, F.ACCOUNT)

    FN.ACCOUNT.HST = 'F.ACCOUNT$HIS'  ; F.ACCOUNT.HST = ''
    CALL OPF(FN.ACCOUNT.HST, F.ACCOUNT.HST)

    FN.STMT.ENTRY = 'F.STMT.ENTRY'  ; F.STMT.ENTRY = ''
    CALL OPF(FN.STMT.ENTRY, F.STMT.ENTRY)

    FN.FT.HIS = 'F.FUNDS.TRANSFER$HIS'  ; F.FT.HIS = ''
    CALL OPF(FN.FT.HIS, F.FT.HIS)

    FN.TT.HIS = 'F.TELLER$HIS'  ; F.TT.HIS = ''
    CALL OPF(FN.TT.HIS, F.TT.HIS)

    FN.TELLER = 'F.TELLER'; F.TELLER = ''
    CALL OPF(FN.TELLER,F.TELLER)

    FN.T24FS.HIS = 'F.T24.FUND.SERVICES$HIS'
    F.T24FS.HIS = ''
    CALL OPF(FN.T24FS.HIS,F.T24FS.HIS)

    FN.T24FS = 'F.T24.FUND.SERVICES'
    F.T24FS = ''
    CALL OPF(FN.T24FS,F.T24FS)

    FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER';    F.FUNDS.TRANSFER = ''
    CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)

    FN.ACCT.ENT.LWORK.DAY = 'F.ACCT.ENT.LWORK.DAY'  ; F.ACCT.ENT.LWORK.DAY = ""
    CALL OPF(FN.ACCT.ENT.LWORK.DAY, F.ACCT.ENT.LWORK.DAY)

    FN.DR.REG.213IF01.PARAM = 'F.DR.REG.213IF01.PARAM'   ; F.DR.REG.213IF01.PARAM = ''
    CALL OPF(FN.DR.REG.213IF01.PARAM, F.DR.REG.213IF01.PARAM)

    FN.DR.REG.213IF01.CONCAT = 'F.DR.REG.213IF01.CONCAT'; F.DR.REG.213IF01.CONCAT = ''
    CALL OPF(FN.DR.REG.213IF01.CONCAT,F.DR.REG.213IF01.CONCAT)

    FN.DR.REG.213IF01.WORKFILE = 'F.DR.REG.213IF01.WORKFILE'; F.DR.REG.213IF01.WORKFILE = ''
    CALL OPF(FN.DR.REG.213IF01.WORKFILE,F.DR.REG.213IF01.WORKFILE)

* PACS00309079 - start
    FN.AA.ARR.TERM.AMOUNT = 'F.AA.ARR.TERM.AMOUNT'; F.AA.ARR.TERM.AMOUNT = ''
    CALL OPF(FN.AA.ARR.TERM.AMOUNT,F.AA.ARR.TERM.AMOUNT)

    FN.SEC.TRADE = 'F.SEC.TRADE'; F.SEC.TRADE = ''
    CALL OPF(FN.SEC.TRADE,F.SEC.TRADE)

    FN.AZ.ACCOUNT = 'F.AZ.ACCOUNT'; F.AZ.ACCOUNT = ''
    CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)

    FN.AZ.ACCOUNT.HST = 'F.AZ.ACCOUNT$HIS'; F.AZ.ACCOUNT.HST = ''
    CALL OPF(FN.AZ.ACCOUNT.HST,F.AZ.ACCOUNT.HST)

    FN.AA.ARRANGEMENT = 'F.AA.ARRANGEMENT'; F.AA.ARRANGEMENT = ''
    CALL OPF(FN.AA.ARRANGEMENT,F.AA.ARRANGEMENT)

    FN.AA.PRODUCT.GROUP = 'F.AA.PRODUCT.GROUP'; F.AA.PRODUCT.GROUP = ''
    CALL OPF(FN.AA.PRODUCT.GROUP,F.AA.PRODUCT.GROUP)

    FN.CURRENCY = 'F.CURRENCY'; FV.CURRENCY = ''
    CALL OPF(FN.CURRENCY,FV.CURRENCY)

    FN.REDO.RTE.CUST.CASHTXN = 'F.REDO.RTE.CUST.CASHTXN'
    F.REDO.RTE.CUST.CASHTXN = ''
    CALL OPF(FN.REDO.RTE.CUST.CASHTXN,F.REDO.RTE.CUST.CASHTXN)

    FN.ACCT.ENT.TODAY = 'F.ACCT.ENT.TODAY'  ; F.ACCT.ENT.TODAY = ""
    CALL OPF(FN.ACCT.ENT.TODAY, F.ACCT.ENT.TODAY)

    FN.COUNTRY = 'F.COUNTRY'
    F.COUNTRY = ''
    CALL OPF(FN.COUNTRY,F.COUNTRY)

    FN.REDO.H.REPORTS.PARAM = "F.REDO.H.REPORTS.PARAM"
    F.REDO.H.REPORTS.PARAM  = ""
    CALL OPF(FN.REDO.H.REPORTS.PARAM,F.REDO.H.REPORTS.PARAM)

    R.REDO.H.REPORTS.PARAM = ''
    RTE.PARAM.ERR = ''
    RTE.PARAM.ID = 'REDO.RTE.FORM'
    CALL CACHE.READ(FN.REDO.H.REPORTS.PARAM,RTE.PARAM.ID,R.REDO.H.REPORTS.PARAM,RTE.PARAM.ERR)

    IF R.REDO.H.REPORTS.PARAM THEN
        Y.FIELD.NME.ARR = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.FIELD.NAME>
        Y.FIELD.VAL.ARR = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.FIELD.VALUE>
        Y.DISP.TEXT.ARR = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.DISPLAY.TEXT>
    END
    LOCATE "TFS.LCY" IN Y.FIELD.NME.ARR<1,1> SETTING TFS.LCY.POS THEN
        Y.TFS.LCY.VERSIONS = Y.FIELD.VAL.ARR<1,TFS.LCY.POS>
    END
    Y.TFS.LCY.VERSIONS = CHANGE(Y.TFS.LCY.VERSIONS,@SM,@VM)

    LOCATE "TFS.SERVICE" IN Y.FIELD.NME.ARR<1,1> SETTING TFS.SERVICE.POS THEN
        Y.TFS.SERVICE.VERSIONS = Y.FIELD.VAL.ARR<1,TFS.SERVICE.POS>
    END
    Y.TFS.SERVICE.VERSIONS = CHANGE(Y.TFS.SERVICE.VERSIONS,@SM,@VM)

    LOCATE "CASH.FX" IN Y.FIELD.NME.ARR<1,1> SETTING FX.VER.POS THEN
        Y.FX.VERSIONS = Y.FIELD.VAL.ARR<1,FX.VER.POS>
    END
    Y.FX.VERSIONS = CHANGE(Y.FX.VERSIONS,@SM,@VM)
    AMT.LCY = ''; THRESHOLD.FCY = ''; R.CURRENCY = ''; CURR.ERR = ''
    CUR.AMLBUY.RATE = ''; YTHRESHOLD.CCY = ''
    THRESHOLD.FCY = R.DR.REG.213IF01.PARAM<DR.213IF01.THRESHOLD.AMT>
    YTHRESHOLD.CCY = R.DR.REG.213IF01.PARAM<DR.213IF01.THRESHOLD.CCY>

    CALL CACHE.READ(FN.CURRENCY,YTHRESHOLD.CCY,R.CURRENCY,CURR.ERR)
    CUR.AMLBUY.RATE = R.CURRENCY<EB.CUR.LOCAL.REF,L.CU.AMLBUY.RT.POS>
    AMT.LCY = THRESHOLD.FCY * CUR.AMLBUY.RATE

    ymend = 0
    Y.LAST.WORK.DAY = R.DATES(EB.DAT.LAST.WORKING.DAY)
    YLST.TODAY = TODAY

    Y.CAL.TODAY = OCONV(DATE(),"DYMD")
    Y.CAL.TODAY = EREPLACE(Y.CAL.TODAY,' ', '')
    YLST.TODAY = Y.CAL.TODAY

    CALL CDT('',YLST.TODAY,'-1C')
    Y.START.DATE = YLST.TODAY

*    Y.START.DATE = Y.LAST.WORK.DAY
*    IF Y.LAST.WORK.DAY[5,2] NE YLST.TODAY[5,2] THEN
*        COMI = Y.LAST.WORK.DAY[1,6]:'01'
*        Y.START.DATE = COMI
*        CALL LAST.DAY.OF.THIS.MONTH
*        Y.END.DATE = COMI
*        ymend = 1
*    END
RETURN

INIT.PARA:
**********

    R.DR.REG.213IF01.PARAM = ''; DR.REG.213IF01.PARAM.ERR = ''; YOBSERVATION = ''; Y.START.DTE = ''
    YSUSP.TXN = ''; YFLD.NAME.ARR = ''; Y.FIELD.VAL.ARR = ''; Y.END.DTE = ''
    Y.DISP.TEXT.ARR = ''; Y.TXNACC.VAL.ARR = ''; Y.TXNACC.DIS.ARR = '' Y.TXNCAT.VAL.ARR = ''; Y.TXNCAT.DIS.ARR = ''
    CALL CACHE.READ(FN.DR.REG.213IF01.PARAM, "SYSTEM", R.DR.REG.213IF01.PARAM, DR.REG.213IF01.PARAM.ERR)
    YOBSERVATION = R.DR.REG.213IF01.PARAM<DR.213IF01.OBSERVATION>
    YSUSP.TXN = R.DR.REG.213IF01.PARAM<DR.213IF01.SUSPICIOUS.TXN>
    Y.START.DTE = R.DR.REG.213IF01.PARAM<DR.213IF01.REP.START.DATE>
    Y.END.DTE = R.DR.REG.213IF01.PARAM<DR.213IF01.REP.END.DATE>
    YFLD.NAME.ARR = R.DR.REG.213IF01.PARAM<DR.213IF01.T24.FLD.NAME>
    Y.FIELD.VAL.ARR = R.DR.REG.213IF01.PARAM<DR.213IF01.T24.FLD.VALUE>
    Y.DISP.TEXT.ARR = R.DR.REG.213IF01.PARAM<DR.213IF01.REG.TAB.VALUE>

    LOCATE "TYPE.OPER" IN YFLD.NAME.ARR<1,1> SETTING TXNACC.POS THEN
        Y.TXNACC.VAL.ARR = Y.FIELD.VAL.ARR<1,TXNACC.POS>
        Y.TXNACC.DIS.ARR = Y.DISP.TEXT.ARR<1,TXNACC.POS>
    END
    Y.TXNACC.VAL.ARR = CHANGE(Y.TXNACC.VAL.ARR,@SM,@VM)
    Y.TXNACC.DIS.ARR = CHANGE(Y.TXNACC.DIS.ARR,@SM,@VM)

    LOCATE "ACC.CATEGORY" IN YFLD.NAME.ARR<1,1> SETTING TXNCAT.POS THEN
        Y.TXNCAT.VAL.ARR = Y.FIELD.VAL.ARR<1,TXNCAT.POS>
        Y.TXNCAT.DIS.ARR = Y.DISP.TEXT.ARR<1,TXNCAT.POS>
    END
    Y.TXNCAT.VAL.ARR = CHANGE(Y.TXNCAT.VAL.ARR,@SM,@VM)
    Y.TXNCAT.DIS.ARR = CHANGE(Y.TXNCAT.DIS.ARR,@SM,@VM)

    IF Y.START.DTE AND Y.END.DTE THEN
        Y.START.DATE = Y.START.DTE
        Y.END.DATE = Y.END.DTE
        ymend = 1
    END

    LOC.APP = "FUNDS.TRANSFER":@FM:"TELLER":@FM:"CUSTOMER":@FM:'ACCOUNT':@FM:'AA.PRD.DES.TERM.AMOUNT':@FM:'AZ.ACCOUNT':@FM:'SEC.TRADE':@FM:'CURRENCY':@FM:'AA.PRD.DES.ACCOUNT':@FM:'T24.FUND.SERVICES'
    FT.FLDS = "L.RTE.FORM":@VM:"L.LAST.NAME":@VM:"L.2ND.LAST.NAME":@VM:"L.1ST.NAME":@VM:"L.2ND.NAME":@VM:"L.NEW.ID.CARD":@VM:"L.OLD.ID.CARD":@VM:"L.PASSPORT":@VM:"L.ACT.INT":@VM:"L.PEP.INTERM":@VM:"L.TYPE.PEP.INT":@VM:"L.PAIS":@VM:"L.ID.PERS.BENEF":@VM:"L.FT.CLNT.TYPE":@VM:"L.NATIONALITY":@VM:"L.SEX":@VM:"BENEFIC.NAME":@VM:"L.ACT.BEN":@VM:"L.PEP.BEN":@VM:"L.TYPE.PEP.BEN"      ;* PACS00309079
    TT.FLDS = "L.RTE.FORM":@VM:"L.LAST.NAME":@VM:"L.2ND.LAST.NAME":@VM:"L.1ST.NAME":@VM:"L.2ND.NAME":@VM:"L.NEW.ID.CARD":@VM:"L.OLD.ID.CARD":@VM:"L.PASSPORT":@VM:"L.TT.FXSN.NUM":@VM:"L.ACT.INT":@VM:"L.PEP.INTERM":@VM:"L.TYPE.PEP.INT":@VM:"L.PAIS":@VM:"L.ID.PERS.BENEF":@VM:"L.TT.CLNT.TYPE":@VM:"L.NATIONALITY":@VM:"L.SEX":@VM:"L.TT.BENEFICIAR":@VM:"L.ACT.BEN":@VM:"L.PEP.BEN":@VM:"L.TYPE.PEP.BEN":@VM:"L.TT.CLIENT.COD":@VM:"L.TT.CR.ACCT.NO":@VM:"L.TT.AC.STATUS"  ;* PACS00309079
    CUS.FLDS = "L.CU.TIPO.CL":@VM:"L.CU.RNC":@VM:"L.CU.CIDENT":@VM:"L.CU.PEPS":@VM:"L.CUS.PEP":@VM:"L.APAP.INDUSTRY":@VM:"L.CU.FOREIGN"
    AC.FLDS = "L.AC.STATUS1":@VM:"L.AC.STATUS2":@VM:"L.INV.FACILITY"
    TA.FLD = "L.CR.FACILITY"
    AZ.FLD = "L.INV.FACILITY":@VM:"L.AC.STATUS1"
    ST.FLD = "L.INV.FACILITY"
    CCY.FLD = "L.CU.AMLBUY.RT":@FM:"L.CR.FACILITY"
    T24FS.FLDS = "L.T24FS.TRA.DAY":@VM:"L.1ST.NAME":@VM:"L.2ND.NAME":@VM:"L.LAST.NAME":@VM:"L.2ND.LAST.NAME":@VM:"L.SEX":@VM:"L.NATIONALITY":@VM:"L.NEW.ID.CARD":@VM:"L.OLD.ID.CARD":@VM:"L.PASSPORT":@VM:"L.TT.BENEFICIAR":@VM:"L.ACT.INT":@VM:"L.PEP.INTERM":@VM:"L.TYPE.PEP.INT":@VM:"L.ACT.BEN":@VM:"L.PEP.BEN":@VM:"L.TYPE.PEP.BEN":@VM:"L.FT.ADD.INFO"
    LOC.FLD = FT.FLDS:@FM:TT.FLDS:@FM:CUS.FLDS:@FM:AC.FLDS:@FM:TA.FLD:@FM:AZ.FLD:@FM:ST.FLD:@FM:CCY.FLD:@FM:T24FS.FLDS
    LOC.POS = ""
    CALL MULTI.GET.LOC.REF(LOC.APP, LOC.FLD, LOC.POS)
    FT.L.RTE.FORM.POS = LOC.POS<1,1>
    FT.LAST.NAME.POS = LOC.POS<1,2>
    FT.2ND.LAST.POS = LOC.POS<1,3>
    FT.L.1ST.NAME.POS = LOC.POS<1,4>
    FT.L.2ND.NAME.POS = LOC.POS<1,5>
    FT.L.NEW.ID.CARD.POS = LOC.POS<1,6>
    FT.L.OLD.ID.CARD.POS = LOC.POS<1,7>
    FT.L.PASSPORT.POS = LOC.POS<1,8>
    FT.L.ACT.INT.POS = LOC.POS<1,9>
    FT.L.PEP.INTERM.POS = LOC.POS<1,10>
    FT.L.TYPE.PEP.INT.POS = LOC.POS<1,11>
    FT.L.PAIS.POS = LOC.POS<1,12>
    FT.L.ID.PERS.BENEF.POS = LOC.POS<1,13>
    FT.L.FT.CLNT.TYPE.POS = LOC.POS<1,14>
    FT.L.NATIONALITY.POS = LOC.POS<1,15>
    FT.L.SEX.POS = LOC.POS<1,16>
    FT.BEN.NAME.POS = LOC.POS<1,17>

    FT.L.ACT.BEN.POS = LOC.POS<1,18>
    FT.L.PEP.BEN.POS = LOC.POS<1,19>
    FT.L.TYPE.PEP.BEN.POS = LOC.POS<1,20>

* PACS00309079 - end
    TT.L.RTE.FORM.POS = LOC.POS<2,1>
    TT.LAST.NAME.POS = LOC.POS<2,2>
    TT.2ND.LAST.POS = LOC.POS<2,3>
    TT.L.1ST.NAME.POS = LOC.POS<2,4>
    TT.L.2ND.NAME.POS = LOC.POS<2,5>
    TT.L.NEW.ID.CARD.POS = LOC.POS<2,6>
    TT.L.OLD.ID.CARD.POS = LOC.POS<2,7>
    TT.L.PASSPORT.POS = LOC.POS<2,8>
    L.TT.FXSN.NUM.POS = LOC.POS<2,9>
    TT.L.ACT.INT.POS = LOC.POS<2,10>
    TT.L.PEP.INTERM.POS = LOC.POS<2,11>
    TT.L.TYPE.PEP.INT.POS = LOC.POS<2,12>
    TT.L.PAIS.POS = LOC.POS<2,13>
    TT.L.ID.PERS.BENEF.POS = LOC.POS<2,14>
    TT.L.FT.CLNT.TYPE.POS = LOC.POS<2,15>
    TT.L.NATIONALITY.POS = LOC.POS<2,16>
    TT.L.SEX.POS = LOC.POS<2,17>
    TT.BEN.NAME.POS = LOC.POS<2,18>
    TT.L.ACT.BEN.POS = LOC.POS<2,19>
    TT.L.PEP.BEN.POS = LOC.POS<2,20>
    TT.L.TYPE.PEP.BEN.POS = LOC.POS<2,21>
    TT.L.CUSTOMER.CODE.POS = LOC.POS<2,22>
    TT.L.TT.CR.ACCT.NO.POS = LOC.POS<2,23>
    TT.L.TT.AC.STATUS.POS = LOC.POS<2,24>
*
    TIPO.CL.POS = LOC.POS<3,1>
    CIDENT.POS = LOC.POS<3,3>
    RNC.POS = LOC.POS<3,2>
    L.CU.PEPS.POS = LOC.POS<3,4>
    L.CUS.PEP.POS = LOC.POS<3,5>
    Y.APAP.INDUS.POS = LOC.POS<3,6>
    L.CU.FOREIGN.POS = LOC.POS<3,7>
*
    AC.STATUS1.POS = LOC.POS<4,1>
    AC.STATUS2.POS = LOC.POS<4,2>
    L.INV.FACILITY.POS = LOC.POS<4,3>
*
*    L.CR.FACILITY.POS = LOC.POS<5,1>
    L.INV.FACILITY.AZ.POS = LOC.POS<6,1>
    AZ.L.AC.STATUS.POS = LOC.POS<6,2>
    L.INV.FACILITY.ST.POS = LOC.POS<7,1>
    L.CU.AMLBUY.RT.POS = LOC.POS<8,1>
    L.CR.FACILITY.POS = LOC.POS<9,1>
*

    L.T24FS.TRA.DAY.POS = LOC.POS<10,1>
    L.1ST.NAME.POS = LOC.POS<10,2>
    L.2ND.NAME.POS = LOC.POS<10,3>
    L.LAST.NAME.POS = LOC.POS<10,4>
    L.2ND.LAST.NAME.POS = LOC.POS<10,5>
    L.SEX.POS = LOC.POS<10,6>
    L.NATIONALITY.POS = LOC.POS<10,7>
    L.NEW.ID.CARD.POS = LOC.POS<10,8>
    L.OLD.ID.CARD.POS = LOC.POS<10,9>
    L.PASSPORT.POS = LOC.POS<10,10>
    L.TT.BENEFICIAR.POS = LOC.POS<10,11>
    L.ACT.INT.POS = LOC.POS<10,12>
    L.PEP.INTERM.POS = LOC.POS<10,13>
    L.TYPE.PEP.INT.POS = LOC.POS<10,14>
    L.ACT.BEN.POS = LOC.POS<10,15>
    L.PEP.BEN.POS = LOC.POS<10,16>
    L.TYPE.PEP.BEN.POS = LOC.POS<10,17>
    L.FT.ADD.INFO.POS = LOC.POS<10,18>
RETURN
*----------------------------------------------------------------
END
