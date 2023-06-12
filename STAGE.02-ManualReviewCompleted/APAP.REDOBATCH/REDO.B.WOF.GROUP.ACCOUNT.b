* @ValidationCode : MjotMTM3NjE5ODI3NDpDcDEyNTI6MTY4MTcwODE4MDY1ODpJVFNTOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 17 Apr 2023 10:39:40
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
SUBROUTINE REDO.B.WOF.GROUP.ACCOUNT(GR.ID)
*-----------------------------------------------------------------------------
* Company Name  : APAP DEV2
* Developed By  : Marimuthu S
* Program Name  : REDO.B.WOF.GROUP.ACCOUNT
*-----------------------------------------------------------------
* Description : This routine used to raise the entry for group of aa loans
*-----------------------------------------------------------------
* Linked With   : -NA-
* In Parameter  : -NA-
* Out Parameter : -NA-
*-----------------------------------------------------------------
* Modification History :
*-----------------------
* Reference              Date                Description
* ODR-2011-12-0017      21-Nov-2011          Initial draft
* Date                   who                   Reference              
* 17-04-2023         CONVERSTION TOOL     R22 AUTO CONVERSTION - FM TO @FM AND VM TO @VM AND SM TO @SM 
* 17-04-2023          ANIL KUMAR B        R22 MANUAL CONVERSTION -NO CHANGES
*-----------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.WORK.INT.CAP.AMT
    $INSERT I_REDO.B.WOF.GROUP.ACCOUNT.COMMON
    $INSERT I_F.REDO.ACCT.MRKWOF.PARAMETER
    $INSERT I_F.STMT.ENTRY
    $INSERT I_F.COMPANY
    $INSERT I_F.ACCOUNT

MAIN:

    GOSUB PROCESS
    GOSUB PGM.END

PROCESS:


    Y.CATEG = FIELD(GR.ID,"*",2)
    Y.CURR = FIELD(GR.ID,"*",1)
    Y.AGE.ST = FIELD(GR.ID,"*",3)
    CALL F.READ(FN.REDO.WORK.INT.CAP.AMT,GR.ID,R.REDO.WORK.INT.CAP.AMT,F.REDO.WORK.INT.CAP.AMT,WOR.ERR)

    LOCATE Y.CATEG IN R.REDO.ACCT.MRKWOF.PARAMETER<RE.WOF.PAR.PROD.CATEGORY,1> SETTING POS.PR THEN
        Y.PR.DEB.CATEG = R.REDO.ACCT.MRKWOF.PARAMETER<RE.WOF.PAR.PRIN.DEB.CATEGORY,POS.PR>
        Y.PR.DEB.TXN = R.REDO.ACCT.MRKWOF.PARAMETER<RE.WOF.PAR.PRIN.DEB.TXN,POS.PR>
        Y.PR.CRED.CATEG = R.REDO.ACCT.MRKWOF.PARAMETER<RE.WOF.PAR.PRIN.CRED.CATEGORY,POS.PR>
        Y.PR.CRED.TXN = R.REDO.ACCT.MRKWOF.PARAMETER<RE.WOF.PAR.PRIN.CRED.TXN,POS.PR>
        Y.IN.DEB.CATEG = R.REDO.ACCT.MRKWOF.PARAMETER<RE.WOF.PAR.INT.DEB.CATEGORY,POS.PR>
        Y.IN.DEB.TXN = R.REDO.ACCT.MRKWOF.PARAMETER<RE.WOF.PAR.INT.DEB.TXN,POS.PR>
        Y.IN.CRED.CATEG = R.REDO.ACCT.MRKWOF.PARAMETER<RE.WOF.PAR.INT.CRED.CATEGORY,POS.PR>
        Y.IN.CRED.TXN = R.REDO.ACCT.MRKWOF.PARAMETER<RE.WOF.PAR.INT.CRED.TXN,POS.PR>

        BEGIN CASE
            CASE Y.AGE.ST EQ 'DE1'
                Y.CDE = '0003'
            CASE Y.AGE.ST EQ 'DEL'
                Y.CDE = '0004'
            CASE Y.AGE.ST EQ 'NAB'
                Y.CDE = '0005'
        END CASE

        GOSUB ENTRY.PRINCIPAL
        GOSUB ENTRY.INTEREST

        R.REDO.WORK.INT.CAP.AMT<REDO.CI.AMT.ENTRY> = 'YES'
        CALL F.WRITE(FN.REDO.WORK.INT.CAP.AMT,GR.ID,R.REDO.WORK.INT.CAP.AMT)
    END

RETURN

ENTRY.PRINCIPAL:

    AMT = '' ; R.STMT.ENTRY = ''
    Y.CAP.FORM = R.REDO.WORK.INT.CAP.AMT<REDO.CI.AMT.CAPITAL.TEXT>
    Y.CNT.CP = DCOUNT(Y.CAP.FORM,@VM)
    LOOP
    WHILE Y.CNT.CP GT 0 DO
        FLG.CP += 1
        Y.CAP.FR = Y.CAP.FORM<1,FLG.CP>
        AMT += FIELD(Y.CAP.FR,'*',2)
        Y.CNT.CP -= 1
    REPEAT

    IF AMT EQ '' OR AMT EQ 0 THEN
        RETURN
    END

    DR.ACCOUNT.NUMBER = Y.CURR:Y.PR.DEB.CATEG:Y.CDE:R.COMPANY(EB.COM.SUB.DIVISION.CODE)
    CR.ACCOUNT.NUMBER = Y.CURR:Y.PR.CRED.CATEG:Y.CDE:R.COMPANY(EB.COM.SUB.DIVISION.CODE)

    R.ACCOUNT = ''
    CALL F.READ(FN.ACCOUNT,DR.ACCOUNT.NUMBER,R.ACCOUNT,F.ACCOUNT,AC.ERR)
    IF NOT(R.ACCOUNT) THEN
        CALL INT.ACC.OPEN(DR.ACCOUNT.NUMBER,RET.CODE)
        R.ACCOUNT = ''
        CALL F.READ(FN.ACCOUNT,DR.ACCOUNT.NUMBER,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
        R.ACCOUNT<AC.LOCAL.REF,L.LOAN.STATUS.POS> = '3'
        CALL F.WRITE(FN.ACCOUNT,DR.ACCOUNT.NUMBER,R.ACCOUNT)
    END ELSE
        R.ACCOUNT<AC.LOCAL.REF,L.LOAN.STATUS.POS> = '3'
        CALL F.WRITE(FN.ACCOUNT,DR.ACCOUNT.NUMBER,R.ACCOUNT)
    END

    R.ACCOUNT = ''
    CALL F.READ(FN.ACCOUNT,CR.ACCOUNT.NUMBER,R.ACCOUNT,F.ACCOUNT,AC.ERR)
    IF NOT(R.ACCOUNT) THEN
        CALL INT.ACC.OPEN(CR.ACCOUNT.NUMBER,RET.CODE)
        R.ACCOUNT = ''
        CALL F.READ(FN.ACCOUNT,CR.ACCOUNT.NUMBER,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
        R.ACCOUNT<AC.LOCAL.REF,L.LOAN.STATUS.POS> = '3'
        CALL F.WRITE(FN.ACCOUNT,CR.ACCOUNT.NUMBER,R.ACCOUNT)
    END ELSE
        R.ACCOUNT<AC.LOCAL.REF,L.LOAN.STATUS.POS> = '3'
        CALL F.WRITE(FN.ACCOUNT,CR.ACCOUNT.NUMBER,R.ACCOUNT)
    END

    DR.TRANS.REF = 3:'-':Y.CURR:'-':Y.CATEG:'-':Y.AGE.ST
    CR.TRANS.REF = 3:'-':Y.CURR:'-':Y.CATEG:'-':Y.AGE.ST

    GOSUB FORM.DEBIT.ENTRY
    GOSUB FORM.CREDIT.ENTRY
    GOSUB CAL.ACCOUNTING

RETURN

ENTRY.INTEREST:

    AMT = '' ; R.STMT.ENTRY = ''
    Y.INT.FORM = R.REDO.WORK.INT.CAP.AMT<REDO.CI.AMT.INTEREST.TEXT>
    Y.CNT.IN = DCOUNT(Y.INT.FORM,@VM)
    LOOP
    WHILE Y.CNT.IN GT 0 DO
        FLG.IN += 1
        Y.IN.FR = Y.INT.FORM<1,FLG.IN>
        AMT += FIELD(Y.IN.FR,'*',2)
        Y.CNT.IN -= 1
    REPEAT

    IF AMT EQ '' OR AMT EQ 0 THEN
        RETURN
    END

    DR.ACCOUNT.NUMBER = Y.CURR:Y.IN.DEB.CATEG:Y.CDE:R.COMPANY(EB.COM.SUB.DIVISION.CODE)
    CR.ACCOUNT.NUMBER = Y.CURR:Y.IN.CRED.CATEG:Y.CDE:R.COMPANY(EB.COM.SUB.DIVISION.CODE)

    R.ACCOUNT = ''
    CALL F.READ(FN.ACCOUNT,DR.ACCOUNT.NUMBER,R.ACCOUNT,F.ACCOUNT,AC.ERR)
    IF NOT(R.ACCOUNT) THEN
        CALL INT.ACC.OPEN(DR.ACCOUNT.NUMBER,RET.CODE)
        R.ACCOUNT = ''
        CALL F.READ(FN.ACCOUNT,DR.ACCOUNT.NUMBER,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
        R.ACCOUNT<AC.LOCAL.REF,L.LOAN.STATUS.POS> = '3'
        CALL F.WRITE(FN.ACCOUNT,DR.ACCOUNT.NUMBER,R.ACCOUNT)
    END ELSE
        R.ACCOUNT<AC.LOCAL.REF,L.LOAN.STATUS.POS> = '3'
        CALL F.WRITE(FN.ACCOUNT,DR.ACCOUNT.NUMBER,R.ACCOUNT)
    END

    R.ACCOUNT = ''
    CALL F.READ(FN.ACCOUNT,CR.ACCOUNT.NUMBER,R.ACCOUNT,F.ACCOUNT,AC.ERR)

    IF NOT(R.ACCOUNT) THEN
        CALL INT.ACC.OPEN(CR.ACCOUNT.NUMBER,RET.CODE)
        R.ACCOUNT = ''
        CALL F.READ(FN.ACCOUNT,CR.ACCOUNT.NUMBER,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
        R.ACCOUNT<AC.LOCAL.REF,L.LOAN.STATUS.POS> = '3'
        CALL F.WRITE(FN.ACCOUNT,CR.ACCOUNT.NUMBER,R.ACCOUNT)
    END ELSE
        R.ACCOUNT<AC.LOCAL.REF,L.LOAN.STATUS.POS> = '3'
        CALL F.WRITE(FN.ACCOUNT,CR.ACCOUNT.NUMBER,R.ACCOUNT)
    END

    DR.TRANS.REF = 3:'-':Y.CURR:'-':Y.CATEG:'-':Y.AGE.ST
    CR.TRANS.REF = 3:'-':Y.CURR:'-':Y.CATEG:'-':Y.AGE.ST

    Y.PR.DEB.TXN = Y.IN.DEB.TXN
    Y.PR.CRED.TXN = Y.IN.CRED.TXN
    Y.PR.CRED.CATEG = Y.IN.CRED.CATEG
    Y.PR.DEB.CATEG = Y.IN.DEB.CATEG

    GOSUB FORM.DEBIT.ENTRY
    GOSUB FORM.CREDIT.ENTRY
    GOSUB CAL.ACCOUNTING

RETURN

FORM.DEBIT.ENTRY:

    R.DR.STMT.ENTRY = ''
    R.DR.STMT.ENTRY<AC.STE.ACCOUNT.NUMBER> = DR.ACCOUNT.NUMBER
    R.DR.STMT.ENTRY<AC.STE.COMPANY.CODE> = ID.COMPANY
    R.DR.STMT.ENTRY<AC.STE.AMOUNT.LCY> = '-':AMT
    R.DR.STMT.ENTRY<AC.STE.TRANSACTION.CODE> = Y.PR.DEB.TXN
    R.DR.STMT.ENTRY<AC.STE.PRODUCT.CATEGORY> = Y.PR.CRED.CATEG
    R.DR.STMT.ENTRY<AC.STE.VALUE.DATE> = TODAY
    R.DR.STMT.ENTRY<AC.STE.CURRENCY> = Y.CURR
    R.DR.STMT.ENTRY<AC.STE.OUR.REFERENCE> = CR.TRANS.REF
    R.DR.STMT.ENTRY<AC.STE.EXPOSURE.DATE> = TODAY
    R.DR.STMT.ENTRY<AC.STE.CURRENCY.MARKET> = '1'
    R.DR.STMT.ENTRY<AC.STE.TRANS.REFERENCE> = CR.TRANS.REF
    R.DR.STMT.ENTRY<AC.STE.SYSTEM.ID> = 'AC'
    R.DR.STMT.ENTRY<AC.STE.BOOKING.DATE> = TODAY

    CHANGE @FM TO @SM IN R.DR.STMT.ENTRY
    CHANGE @SM TO @VM IN R.DR.STMT.ENTRY

    R.STMT.ENTRY<-1> = R.DR.STMT.ENTRY

RETURN

FORM.CREDIT.ENTRY:

    R.CR.STMT.ENTRY = ''
    R.CR.STMT.ENTRY<AC.STE.ACCOUNT.NUMBER> = CR.ACCOUNT.NUMBER
    R.CR.STMT.ENTRY<AC.STE.COMPANY.CODE> = ID.COMPANY
    R.CR.STMT.ENTRY<AC.STE.AMOUNT.LCY> = AMT
    R.CR.STMT.ENTRY<AC.STE.TRANSACTION.CODE> = Y.PR.CRED.TXN
    R.CR.STMT.ENTRY<AC.STE.PRODUCT.CATEGORY> = Y.PR.DEB.CATEG
    R.CR.STMT.ENTRY<AC.STE.VALUE.DATE> = TODAY
    R.CR.STMT.ENTRY<AC.STE.CURRENCY> = Y.CURR
    R.CR.STMT.ENTRY<AC.STE.OUR.REFERENCE> = DR.TRANS.REF
    R.CR.STMT.ENTRY<AC.STE.EXPOSURE.DATE> = TODAY
    R.CR.STMT.ENTRY<AC.STE.CURRENCY.MARKET> = '1'
    R.CR.STMT.ENTRY<AC.STE.TRANS.REFERENCE> = DR.TRANS.REF
    R.CR.STMT.ENTRY<AC.STE.SYSTEM.ID> = 'AC'
    R.CR.STMT.ENTRY<AC.STE.BOOKING.DATE> = TODAY

    CHANGE @FM TO @SM IN R.CR.STMT.ENTRY
    CHANGE @SM TO @VM IN R.CR.STMT.ENTRY

    R.STMT.ENTRY<-1> = R.CR.STMT.ENTRY

RETURN

CAL.ACCOUNTING:

    ACC.PRODUCT = 'RAISE.WOF.ACCOUNTING'
    ACC.TYPE = 'SAO'

    CALL EB.ACCOUNTING(ACC.PRODUCT,ACC.TYPE,R.STMT.ENTRY,'')

RETURN

PGM.END:

END