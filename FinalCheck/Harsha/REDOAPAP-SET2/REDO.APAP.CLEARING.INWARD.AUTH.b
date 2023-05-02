* @ValidationCode : MjotNjcxMzAwNTk3OkNwMTI1MjoxNjgxMjc3NTYyNTc5OmFqaXRoOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 12 Apr 2023 11:02:42
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
$PACKAGE APAP.REDOAPAP
SUBROUTINE REDO.APAP.CLEARING.INWARD.AUTH
***********************************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: Arulprakasam P
* PROGRAM NAME: REDO.APAP.CLEARING.INWARD.AUTHORISE
* ODR NO      : ODR-2010-09-0148
*-----------------------------------------------------------------------------
*DESCRIPTION: This is authorise routine for REDO.CLEARING.INWARD template and
*raise the entries

*IN PARAMETER:  NA
*OUT PARAMETER: NA
*LINKED WITH: NA
*----------------------------------------------------------------------
* Modification History :
*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*12-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION   FM to @FM , VM to @VM,++ to +=
*12-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION NO CHANGE
*----------------------------------------------------------------------------------------
*----------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.DATES
    $INSERT I_F.USER
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CUSTOMER
    $INSERT I_F.LIMIT
    $INSERT I_F.STMT.ENTRY
    $INSERT I_F.CATEG.ENTRY
    $INSERT I_F.REDO.APAP.CLEAR.PARAM
    $INSERT I_F.REDO.APAP.CLEARING.INWARD
    $INSERT I_F.REDO.PENDING.CHARGE
    $INSERT I_F.REDO.CHARGE.PARAM
    $INSERT I_F.REDO.CUST.PRD.LIST
    $INSERT I_F.EB.CONTRACT.BALANCES
    GOSUB INIT
    GOSUB PROCESS

RETURN

*****
INIT:
*****

    FN.REDO.APAP.CLEARING.INWARD = 'F.REDO.APAP.CLEARING.INWARD'
    F.REDO.APAP.CLEARING.INWARD = ''
    R.REDO.APAP.CLEARING.INWARD = ''

    FN.REDO.APAP.CLEAR.PARAM = 'F.REDO.APAP.CLEAR.PARAM'
    F.REDO.APAP.CLEAR.PARAM  = ''
    R.REDO.APAP.CLEAR.PARAM = ''

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT  = ''
    R.ACCOUNT  = ''


    STMT.ENTRY.REC = ''
    FINAL.ENTRY.REC = ''
    CATEG.ENTRY.REC = ''

    FN.STMT.ENTRY = 'F.STMT.ENTRY'
    F.STMT.ENTRY  = ''

    FN.CATEG.ENTRY = 'F.CATEG.ENTRY'
    F.CATEG.ENTRY  = ''

    FN.LIMIT = 'F.LIMIT'
    F.LIMIT = ''

    FN.REDO.PENDING.CHARGE = 'F.REDO.PENDING.CHARGE'
    F.REDO.PENDING.CHARGE  = ''
    CALL OPF(FN.REDO.PENDING.CHARGE,F.REDO.PENDING.CHARGE)

    FN.CHARGE.PARAM = 'F.REDO.CHARGE.PARAM'
    F.CHARGE.PARAM = ''
    CALL OPF(FN.CHARGE.PARAM,F.CHARGE.PARAM)


    FN.CUSTOMER='F.CUSTOMER'
    F.CUSTOMER =''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    FN.REDO.ACCT.FROM.INACT.TO.ACT='F.REDO.ACCT.FROM.INACT.TO.ACT'
    F.REDO.ACCT.FROM.INACT.TO.ACT=''
    CALL OPF(FN.REDO.ACCT.FROM.INACT.TO.ACT,F.REDO.ACCT.FROM.INACT.TO.ACT)

    FN.CUST.PRD.LIST='F.REDO.CUST.PRD.LIST'
    F.CUST.PRD.LIST =''
    CALL OPF(FN.CUST.PRD.LIST,F.CUST.PRD.LIST)


    CALL OPF(FN.REDO.APAP.CLEARING.INWARD,F.REDO.APAP.CLEARING.INWARD)
    CALL OPF(FN.REDO.APAP.CLEAR.PARAM,F.REDO.APAP.CLEAR.PARAM)
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
    CALL OPF(FN.CATEG.ENTRY,F.CATEG.ENTRY)
    CALL OPF(FN.STMT.ENTRY,F.STMT.ENTRY)
    CALL OPF(FN.LIMIT,F.LIMIT)

RETURN

********
PROCESS:
********

    GOSUB FIND.MULTI.LOC.REF
    Y.CHEQUE.AMT = R.NEW(CLEAR.CHQ.AMOUNT)
    Y.ACCOUNT.NO = R.NEW(CLEAR.CHQ.ACCOUNT.NO)
    Y.WAIVE.CHARGE = R.NEW(CLEAR.CHQ.WAIVE.CHARGES)

    Y.CHQ.REASON   = R.NEW(CLEAR.CHQ.REASON)
    CHANGE @VM TO @FM IN Y.CHQ.REASON

    LOCATE "21" IN Y.CHQ.REASON SETTING CAN.POS THEN
        Y.CHQ.REASON.FLAG = "Y"
    END


    Y.CHARGE.AMT = R.NEW(CLEAR.CHQ.CHG.AMOUNT)

    Y.OLD.ACCOUNT = R.OLD(CLEAR.CHQ.ACCOUNT.NO)
    Y.OLD.STATUS = R.OLD(CLEAR.CHQ.STATUS)
    Y.STATUS = R.NEW(CLEAR.CHQ.STATUS)
    Y.OLD.REFER.NAME = R.OLD(CLEAR.CHQ.REFER.NAME)
    Y.REFER.NAME = R.OLD(CLEAR.CHQ.REFER.NAME)      ;* During Auth stage, we get the referer name as SYSTEM

    Y.USER = OPERATOR

    CALL CACHE.READ(FN.REDO.APAP.CLEAR.PARAM,'SYSTEM',R.REDO.APAP.CLEAR.PARAM,REDO.APAP.CLEAR.PARAM.ERR)

    CALL F.READ(FN.ACCOUNT,Y.ACCOUNT.NO,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
    IF R.ACCOUNT THEN
        Y.CUSTOMER = R.ACCOUNT<AC.CUSTOMER>
        Y.ACCT.OFFICER = R.ACCOUNT<AC.ACCOUNT.OFFICER>
        Y.CATEGORY = R.ACCOUNT<AC.CATEGORY>
        Y.CURRENCY  = R.ACCOUNT<AC.CURRENCY>
    END

    IF (Y.OLD.STATUS EQ 'REFERRED' AND Y.STATUS EQ 'REFERRED' AND Y.CHQ.REASON.FLAG EQ "Y" AND Y.WAIVE.CHARGE NE 'YES' AND Y.OLD.REFER.NAME EQ 'SYSTEM' AND Y.ACCOUNT.NO NE Y.OLD.ACCOUNT) THEN
        GOSUB MANUAL.REFER.ENTRY
    END
    GOSUB AUTO.PAID.MANUAL.REFER.ENTRY
    GOSUB AUTO.REFER.PAID.ENTRY
    GOSUB AUTO.REFER.REJECT.ENTRY
    GOSUB MANUAL.REFER.PAID.ENTRY
    GOSUB MANUAL.REFER.REJECT.ENTRY

    IF FINAL.ENTRY.REC THEN
        Y.TYPE = 'SAO'
        CALL EB.ACCOUNTING("AC",Y.TYPE,FINAL.ENTRY.REC,'')
    END

RETURN

*******************
MANUAL.REFER.ENTRY:
*******************

*Raising Debit Entry for Customer account by cheque amount

    IF Y.CHEQUE.AMT THEN
        STMT.ENTRY.REC<AC.STE.ACCOUNT.NUMBER> = Y.ACCOUNT.NO
        STMT.ENTRY.REC<AC.STE.AMOUNT.LCY> = -1 * Y.CHEQUE.AMT
        STMT.ENTRY.REC<AC.STE.TRANSACTION.CODE> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.INWARD.DR.CODE>
        GOSUB GET.STMT.DETAILS

        FINAL.ENTRY.REC<-1> = LOWER(STMT.ENTRY.REC)

*Raising Credit Entry to Internal account by cheque amount

        STMT.ENTRY.REC<AC.STE.ACCOUNT.NUMBER> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.INW.REFERRAL>
        STMT.ENTRY.REC<AC.STE.AMOUNT.LCY> = Y.CHEQUE.AMT
        STMT.ENTRY.REC<AC.STE.TRANSACTION.CODE> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.INWARD.CR.CODE>
        GOSUB GET.STMT.DETAILS

        FINAL.ENTRY.REC<-1> = LOWER(STMT.ENTRY.REC)
    END

*Raising Debit Entry for Customer account by tax calculated for cheque amount

    IF R.NEW(CLEAR.CHQ.TAX.AMOUNT) AND Y.CUSTOMER NE '' THEN

        STMT.ENTRY.REC<AC.STE.ACCOUNT.NUMBER> = Y.ACCOUNT.NO
        STMT.ENTRY.REC<AC.STE.AMOUNT.LCY> = -1 * R.NEW(CLEAR.CHQ.TAX.AMOUNT)
        STMT.ENTRY.REC<AC.STE.TRANSACTION.CODE> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.TAX.DR.CODE>
        GOSUB GET.STMT.DETAILS

        FINAL.ENTRY.REC<-1> = LOWER(STMT.ENTRY.REC)

*Raising Credit Entry to TAX.CLEARING account by tax calculated for cheque amount

        STMT.ENTRY.REC<AC.STE.ACCOUNT.NUMBER> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.TAX.CLEARING>
        STMT.ENTRY.REC<AC.STE.AMOUNT.LCY> = R.NEW(CLEAR.CHQ.TAX.AMOUNT)
        STMT.ENTRY.REC<AC.STE.TRANSACTION.CODE> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.TAX.CR.CODE>
        GOSUB GET.STMT.DETAILS

        FINAL.ENTRY.REC<-1> = LOWER(STMT.ENTRY.REC)
    END

*Raising Debit Entry for INWARD.REFERRAL internal account by cheque amount

    IF Y.CHEQUE.AMT THEN
        STMT.ENTRY.REC<AC.STE.ACCOUNT.NUMBER> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.INW.REFERRAL>
        STMT.ENTRY.REC<AC.STE.AMOUNT.LCY> = -1 * Y.CHEQUE.AMT
        STMT.ENTRY.REC<AC.STE.TRANSACTION.CODE> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.INWARD.DR.CODE>
        GOSUB GET.STMT.DETAILS

        FINAL.ENTRY.REC<-1> = LOWER(STMT.ENTRY.REC)

*Raising Creditg Entry for Customer account by cheque amount

        STMT.ENTRY.REC<AC.STE.ACCOUNT.NUMBER> = Y.ACCOUNT.NO
        STMT.ENTRY.REC<AC.STE.AMOUNT.LCY> = Y.CHEQUE.AMT
        STMT.ENTRY.REC<AC.STE.TRANSACTION.CODE> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.INWARD.CR.CODE>
        GOSUB GET.STMT.DETAILS

        FINAL.ENTRY.REC<-1> = LOWER(STMT.ENTRY.REC)
    END

*Raising Debit Entry for tax clearing internal account by check amount with tax percent

    IF R.NEW(CLEAR.CHQ.TAX.AMOUNT) AND Y.CUSTOMER NE '' THEN
        STMT.ENTRY.REC<AC.STE.ACCOUNT.NUMBER> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.TAX.CLEARING>
        STMT.ENTRY.REC<AC.STE.AMOUNT.LCY> = -1 * R.NEW(CLEAR.CHQ.TAX.AMOUNT)
        STMT.ENTRY.REC<AC.STE.TRANSACTION.CODE> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.TAX.DR.CODE>
        GOSUB GET.STMT.DETAILS

        FINAL.ENTRY.REC<-1> = LOWER(STMT.ENTRY.REC)

*Raising Creditg Entry for Customer account by check amount with tax percent

        STMT.ENTRY.REC<AC.STE.ACCOUNT.NUMBER> = Y.ACCOUNT.NO
        STMT.ENTRY.REC<AC.STE.AMOUNT.LCY> = R.NEW(CLEAR.CHQ.TAX.AMOUNT)
        STMT.ENTRY.REC<AC.STE.TRANSACTION.CODE> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.TAX.CR.CODE>
        GOSUB GET.STMT.DETAILS

        FINAL.ENTRY.REC<-1> = LOWER(STMT.ENTRY.REC)
    END

RETURN
*-------------------------------------------------------------------------
*****************************
AUTO.PAID.MANUAL.REFER.ENTRY:
*****************************
    IF (Y.OLD.STATUS EQ 'PAID' AND Y.STATUS EQ 'REFERRED') THEN

*Raising Debit Entry for INWARD.REFERRAL internal account by cheque amount

        IF Y.CHEQUE.AMT THEN
            STMT.ENTRY.REC<AC.STE.ACCOUNT.NUMBER> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.INW.REFERRAL>
            STMT.ENTRY.REC<AC.STE.AMOUNT.LCY> = -1 * Y.CHEQUE.AMT
            STMT.ENTRY.REC<AC.STE.TRANSACTION.CODE> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.INWARD.DR.CODE>
            GOSUB GET.STMT.DETAILS

            FINAL.ENTRY.REC<-1> = LOWER(STMT.ENTRY.REC)



*Raising Creditg Entry for Customer account by cheque amount

            STMT.ENTRY.REC<AC.STE.ACCOUNT.NUMBER> = Y.ACCOUNT.NO
            STMT.ENTRY.REC<AC.STE.AMOUNT.LCY> = Y.CHEQUE.AMT
            STMT.ENTRY.REC<AC.STE.TRANSACTION.CODE> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.INWARD.CR.CODE>
            GOSUB GET.STMT.DETAILS

            FINAL.ENTRY.REC<-1> = LOWER(STMT.ENTRY.REC)
        END

*Raising Debit Entry for tax clearing internal account by check amount with tax percent

        IF R.NEW(CLEAR.CHQ.TAX.AMOUNT) AND Y.CUSTOMER NE '' THEN
            STMT.ENTRY.REC<AC.STE.ACCOUNT.NUMBER> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.TAX.CLEARING>
            STMT.ENTRY.REC<AC.STE.AMOUNT.LCY> = -1 * R.NEW(CLEAR.CHQ.TAX.AMOUNT)
            STMT.ENTRY.REC<AC.STE.TRANSACTION.CODE> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.TAX.DR.CODE>
            GOSUB GET.STMT.DETAILS

            FINAL.ENTRY.REC<-1> = LOWER(STMT.ENTRY.REC)

*Raising Creditg Entry for Customer account by check amount with tax percent

            STMT.ENTRY.REC<AC.STE.ACCOUNT.NUMBER> = Y.ACCOUNT.NO
            STMT.ENTRY.REC<AC.STE.AMOUNT.LCY> = R.NEW(CLEAR.CHQ.TAX.AMOUNT)
            STMT.ENTRY.REC<AC.STE.TRANSACTION.CODE> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.TAX.CR.CODE>
            GOSUB GET.STMT.DETAILS

            FINAL.ENTRY.REC<-1> = LOWER(STMT.ENTRY.REC)
        END

*Raising Debit Entry for Customer account by charge amount
        IF Y.WAIVE.CHARGE NE 'YES' THEN
            IF Y.CHARGE.AMT THEN
                STMT.ENTRY.REC<AC.STE.ACCOUNT.NUMBER> = Y.ACCOUNT.NO
                STMT.ENTRY.REC<AC.STE.AMOUNT.LCY> = -1 * Y.CHARGE.AMT
                STMT.ENTRY.REC<AC.STE.TRANSACTION.CODE> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.COMM.DR.CODE>
                GOSUB GET.STMT.DETAILS

                FINAL.ENTRY.REC<-1> = LOWER(STMT.ENTRY.REC)



*Raising Credit PL category account by charge amount

                CATEG.ENTRY.REC = ''
                CATEG.ENTRY.REC<AC.CAT.AMOUNT.LCY>       = Y.CHARGE.AMT
                CATEG.ENTRY.REC<AC.CAT.TRANSACTION.CODE> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.COMM.CR.CODE>
                CATEG.ENTRY.REC<AC.CAT.PL.CATEGORY>      = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.CLEARING.COMM>
                GOSUB GET.CATEG.DETAILS

                FINAL.ENTRY.REC<-1> = LOWER(CATEG.ENTRY.REC)
            END
        END
    END

RETURN
**********************
AUTO.REFER.PAID.ENTRY:
**********************

    IF (Y.OLD.STATUS EQ 'REFERRED' AND Y.STATUS EQ 'PAID' AND Y.REFER.NAME EQ 'SYSTEM') THEN

        CALL F.READ(FN.ACCOUNT,Y.ACCOUNT.NO,R.ACCOUNT,F.ACCOUNT,ACCT.ERR)
        R.ECB='' ; ECB.ERR='' ;*Tus Start
        CALL EB.READ.HVT("EB.CONTRACT.BALANCES",Y.ACCOUNT.NO,R.ECB,ECB.ERR);*Tus End
        IF R.ACCOUNT THEN
            Y.LIMIT.REF   = R.ACCOUNT<AC.LIMIT.REF>
            Y.CUSTOMER    = R.ACCOUNT<AC.CUSTOMER>
*Y.WORKING.BAL = R.ACCOUNT<AC.WORKING.BALANCE>;*Tus Start
            Y.WORKING.BAL = R.ECB<ECB.WORKING.BALANCE>;*Tus End
            Y.LOCKED.AMT  = R.ACCOUNT<AC.LOCKED.AMOUNT>
            Y.TRANS.AVAIL.AMT = R.ACCOUNT<AC.LOCAL.REF,Y.L.AC.TRAN.AVAIL.POS>
            Y.TRANS.LIMIT.AMT = R.ACCOUNT<AC.LOCAL.REF,Y.L.AC.TRANS.LIM.POS>
            Y.AVAILABLE.BAL   = R.ACCOUNT<AC.LOCAL.REF,Y.L.AC.AV.BAL.POS>
        END

        IF Y.TRANS.AVAIL.AMT GT Y.TRANS.LIMIT.AMT THEN
            Y.TRANSIT.FUND = Y.TRANS.LIMIT.AMT
        END ELSE
            Y.TRANSIT.FUND = Y.TRANS.AVAIL.AMT
        END

        IF Y.LIMIT.REF NE '' THEN
            Y.LIMIT.REF = FMT(Y.LIMIT.REF,'R%10')
            LIMIT.ID = Y.CUSTOMER : '.' : Y.LIMIT.REF
            CALL F.READ(FN.LIMIT,LIMIT.ID,R.LIMIT,F.LIMIT,LIMIT.ERR)

            IF R.LIMIT THEN
                VAR.AVAIL.AMT = R.LIMIT<LI.AVAIL.AMT>
            END

            Y.CAL.BAL = Y.AVAILABLE.BAL + VAR.AVAIL.AMT + Y.TRANSIT.FUND
        END ELSE
            Y.CAL.BAL = Y.AVAILABLE.BAL + Y.TRANSIT.FUND
        END


*IF Y.CAL.BAL GT Y.CHEQUE.AMT THEN
        GOSUB AUTO.REFER.PAID.ENTRIES
*END ELSE
*Y.CUR.ACC.NO   = Y.ACCOUNT.NO[1,3]
*IF NUM(Y.CUR.ACC.NO) THEN
*AF = CLEAR.CHQ.AMOUNT
*ETEXT = "EB-INSUFFICIENT.FUND"
*CALL STORE.END.ERROR
*END
*END
        Y.CUR.ACC.NO   = Y.ACCOUNT.NO[1,3]
        IF Y.WAIVE.CHARGE EQ 'YES' AND NUM(Y.CUR.ACC.NO) THEN
            GOSUB REFER.CHARGE.ENTRY.REVERSAL
        END
        IF Y.CUSTOMER AND R.ACCOUNT THEN
            GOSUB UPDATE.ACCOUNT.STATUS
        END

    END
RETURN

************************
AUTO.REFER.PAID.ENTRIES:
************************

*Raising Debit Entry for Customer account by cheque amount

    IF Y.CHEQUE.AMT THEN
        STMT.ENTRY.REC<AC.STE.ACCOUNT.NUMBER> = Y.ACCOUNT.NO
        STMT.ENTRY.REC<AC.STE.AMOUNT.LCY> = -1 * Y.CHEQUE.AMT
        STMT.ENTRY.REC<AC.STE.TRANSACTION.CODE> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.INWARD.DR.CODE>
        GOSUB GET.STMT.DETAILS

        FINAL.ENTRY.REC<-1> = LOWER(STMT.ENTRY.REC)

*Raising Credit Entry to INW.REFERRAL Internal account by cheque amount

        STMT.ENTRY.REC<AC.STE.ACCOUNT.NUMBER> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.INW.REFERRAL>
        STMT.ENTRY.REC<AC.STE.AMOUNT.LCY> = Y.CHEQUE.AMT
        STMT.ENTRY.REC<AC.STE.TRANSACTION.CODE> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.INWARD.CR.CODE>
        GOSUB GET.STMT.DETAILS

        FINAL.ENTRY.REC<-1> = LOWER(STMT.ENTRY.REC)
    END

*Raising Debit Entry for customer account by check amount with tax percent

    IF R.NEW(CLEAR.CHQ.TAX.AMOUNT) AND Y.CUSTOMER NE '' THEN
        STMT.ENTRY.REC<AC.STE.ACCOUNT.NUMBER> = Y.ACCOUNT.NO
        STMT.ENTRY.REC<AC.STE.AMOUNT.LCY> = -1 * R.NEW(CLEAR.CHQ.TAX.AMOUNT)
        STMT.ENTRY.REC<AC.STE.TRANSACTION.CODE> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.TAX.DR.CODE>
        GOSUB GET.STMT.DETAILS

        FINAL.ENTRY.REC<-1> = LOWER(STMT.ENTRY.REC)

*Raising Credit Entry to TAX.CLEARING account by tax calculated for cheque amount

        STMT.ENTRY.REC<AC.STE.ACCOUNT.NUMBER> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.TAX.CLEARING>
        STMT.ENTRY.REC<AC.STE.AMOUNT.LCY> = R.NEW(CLEAR.CHQ.TAX.AMOUNT)
        STMT.ENTRY.REC<AC.STE.TRANSACTION.CODE> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.TAX.CR.CODE>
        GOSUB GET.STMT.DETAILS

        FINAL.ENTRY.REC<-1> = LOWER(STMT.ENTRY.REC)
    END

RETURN

************************
AUTO.REFER.REJECT.ENTRY:
************************

    IF (Y.OLD.STATUS EQ 'REFERRED' AND Y.STATUS EQ 'REJECTED' AND Y.REFER.NAME EQ 'SYSTEM') THEN
        GOSUB AUTO.REFER.REJECT.ENTRIES
        Y.CUR.ACC.NO   = Y.ACCOUNT.NO[1,3]
        IF Y.WAIVE.CHARGE EQ 'YES' AND NUM(Y.CUR.ACC.NO) THEN
            GOSUB REFER.CHARGE.ENTRY.REVERSAL
        END
    END

RETURN

**************************
AUTO.REFER.REJECT.ENTRIES:
**************************

*Raising Debit Entry for tax clearing internal account by check amount with tax percent

    IF Y.CHEQUE.AMT THEN
        STMT.ENTRY.REC<AC.STE.ACCOUNT.NUMBER> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.INW.RETURN>
        STMT.ENTRY.REC<AC.STE.AMOUNT.LCY> = -1 *  Y.CHEQUE.AMT
        STMT.ENTRY.REC<AC.STE.TRANSACTION.CODE> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.INWARD.DR.CODE>
        GOSUB GET.STMT.DETAILS

        FINAL.ENTRY.REC<-1> = LOWER(STMT.ENTRY.REC)

*Raising Credit Entry to INW.REFERRAL Internal account by cheque amount

        STMT.ENTRY.REC<AC.STE.ACCOUNT.NUMBER> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.INW.REFERRAL>
        STMT.ENTRY.REC<AC.STE.AMOUNT.LCY> = Y.CHEQUE.AMT
        STMT.ENTRY.REC<AC.STE.TRANSACTION.CODE> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.INWARD.CR.CODE>
        GOSUB GET.STMT.DETAILS

        FINAL.ENTRY.REC<-1> = LOWER(STMT.ENTRY.REC)
    END

RETURN

************************
MANUAL.REFER.PAID.ENTRY:
************************

    IF (Y.OLD.STATUS EQ 'REFERRED' AND Y.STATUS EQ 'PAID' AND Y.REFER.NAME NE 'SYSTEM') THEN

        CALL F.READ(FN.ACCOUNT,Y.ACCOUNT.NO,R.ACCOUNT,F.ACCOUNT,ACCT.ERR)
        R.ECB='' ; ECB.ERR= '' ;*Tus Start
        CALL EB.READ.HVT("EB.CONTRACT.BALANCES",Y.ACCOUNT.NO,R.ECB,ECB.ERR);*Tus End
        IF R.ACCOUNT THEN
            Y.LIMIT.REF   = R.ACCOUNT<AC.LIMIT.REF>
            Y.CUSTOMER    = R.ACCOUNT<AC.CUSTOMER>
*     Y.WORKING.BAL = R.ACCOUNT<AC.WORKING.BALANCE>;*Tus Start
            Y.WORKING.BAL = R.ECB<ECB.WORKING.BALANCE>;*Tus End
            Y.LOCKED.AMT  = R.ACCOUNT<AC.LOCKED.AMOUNT>
            Y.TRANS.AVAIL.AMT = R.ACCOUNT<AC.LOCAL.REF,Y.L.AC.TRAN.AVAIL.POS>
            Y.TRANS.LIMIT.AMT = R.ACCOUNT<AC.LOCAL.REF,Y.L.AC.TRANS.LIM.POS>
            Y.AVAILABLE.BAL   = R.ACCOUNT<AC.LOCAL.REF,Y.L.AC.AV.BAL.POS>
        END

        IF Y.TRANS.AVAIL.AMT GT Y.TRANS.LIMIT.AMT THEN
            Y.TRANSIT.FUND = Y.TRANS.LIMIT.AMT
        END ELSE
            Y.TRANSIT.FUND = Y.TRANS.AVAIL.AMT
        END

        IF Y.LIMIT.REF NE '' THEN
            LIMIT.ID = Y.CUSTOMER : '.' : Y.LIMIT.REF
            CALL F.READ(FN.LIMIT,LIMIT.ID,R.LIMIT,F.LIMIT,LIMIT.ERR)
            IF R.LIMIT THEN
                VAR.AVAIL.AMT = R.LIMIT<LI.AVAIL.AMT>
            END

            Y.CAL.BAL = Y.AVAILABLE.BAL + VAR.AVAIL.AMT + Y.TRANSIT.FUND
        END ELSE
            Y.CAL.BAL = Y.AVAILABLE.BAL + Y.TRANSIT.FUND
        END


*IF Y.CAL.BAL GT Y.CHEQUE.AMT THEN
        GOSUB AUTO.REFER.PAID.ENTRIES
*END ELSE
*Y.CUR.ACC.NO   = Y.ACCOUNT.NO[1,3]
*IF NUM(Y.CUR.ACC.NO) THEN
*AF = CLEAR.CHQ.AMOUNT
*ETEXT = "EB-INSUFFICIENT.FUND"
*CALL STORE.END.ERROR
*END
*END
*IF Y.WAIVE.CHARGE EQ 'YES' THEN
*GOSUB REFER.CHARGE.ENTRY.REVERSAL
*END
        IF Y.CUSTOMER AND R.ACCOUNT THEN
            GOSUB UPDATE.ACCOUNT.STATUS
        END
    END
RETURN
*--------------------------------------------------------
REFER.CHARGE.ENTRY.REVERSAL:
*--------------------------------------------------------

    CUSTOMER.ID         = Y.CUSTOMER
    DEAL.AMOUNT         = R.NEW(CLEAR.CHQ.AMOUNT)
    DEAL.CURRENCY       = Y.CURRENCY
    CCY.MKT             = '1'
    CROSS.RATE          = ""
    CROSS.CURRENCY      = DEAL.CURRENCY
    DRAWDOWN.CURRENCY   = CROSS.CURRENCY
    T.DATA              = ""
    TOTAL.FOREIGN.AMT   = ""
    TOTAL.LOCAL.AMT     = ""
    TOTAL.AMT           = ""
    T.DATA<1,1>         = R.NEW(CLEAR.CHQ.CHARGE.TYPE)

    CALL CALCULATE.CHARGE(CUSTOMER.ID, DEAL.AMOUNT, DEAL.CURRENCY, CCY.MKT, CROSS.RATE,CROSS.CURRENCY, DRAWDOWN.CURRENCY, T.DATA, '', TOTAL.LOCAL.AMT, TOTAL.FOREIGN.AMT)
    VAR.TOT.CHG.AMT = T.DATA<4,1>
    IF VAR.TOT.CHG.AMT GT 0 ELSE
        RETURN
    END
    Y.PENDING.CHG.FLAG = ''
    GOSUB CHECK.PL
    CATEG.ENTRY.REC = ''
    CATEG.ENTRY.REC<AC.CAT.AMOUNT.LCY>       = -1 * VAR.TOT.CHG.AMT
    CATEG.ENTRY.REC<AC.CAT.TRANSACTION.CODE> = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.COMM.DR.CODE>
    CATEG.ENTRY.REC<AC.CAT.PL.CATEGORY>      = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.CLEARING.COMM>
    GOSUB GET.CATEG.DETAILS
    FINAL.ENTRY.REC<-1> = LOWER(CATEG.ENTRY.REC)

    IF Y.PENDING.CHG.FLAG EQ 'YES' AND Y.CHARGE.CR.TXN AND Y.CHARGE.PL.CAT THEN   ;* Credit PL or Customer account based on B.120
        CATEG.ENTRY.REC = ''
        CATEG.ENTRY.REC<AC.CAT.AMOUNT.LCY>       = VAR.TOT.CHG.AMT
        CATEG.ENTRY.REC<AC.CAT.TRANSACTION.CODE> = Y.CHARGE.CR.TXN
        CATEG.ENTRY.REC<AC.CAT.PL.CATEGORY>      = Y.CHARGE.PL.CAT
        GOSUB GET.CATEG.DETAILS
        FINAL.ENTRY.REC<-1> = LOWER(CATEG.ENTRY.REC)
    END ELSE
        STMT.ENTRY.REC = ''
        STMT.ENTRY.REC<AC.STE.ACCOUNT.NUMBER>    = Y.ACCOUNT.NO
        STMT.ENTRY.REC<AC.STE.AMOUNT.LCY>        = VAR.TOT.CHG.AMT
        STMT.ENTRY.REC<AC.STE.TRANSACTION.CODE>  = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.COMM.CR.CODE>
        GOSUB GET.STMT.DETAILS
        FINAL.ENTRY.REC<-1> = LOWER(STMT.ENTRY.REC)
    END


RETURN
**************************
MANUAL.REFER.REJECT.ENTRY:
**************************

    IF (Y.OLD.STATUS EQ 'REFERRED' AND Y.STATUS EQ 'REJECTED' AND Y.REFER.NAME NE 'SYSTEM') THEN
        GOSUB AUTO.REFER.REJECT.ENTRIES
    END

RETURN

**********************
CHECK.PL:
**********************

    CALL F.READ(FN.REDO.PENDING.CHARGE,Y.ACCOUNT.NO,R.REDO.PENDING.CHARGE,F.REDO.PENDING.CHARGE,PEND.CHRG)
    IF R.REDO.PENDING.CHARGE THEN
        LOCATE ID.NEW IN R.REDO.PENDING.CHARGE<PEN.CHG.TRANSACTION,1> SETTING POS1 THEN
            DEL R.REDO.PENDING.CHARGE<PEN.CHG.TRANSACTION,POS1>
            DEL R.REDO.PENDING.CHARGE<PEN.CHG.CURRENCY,POS1>
            DEL R.REDO.PENDING.CHARGE<PEN.CHG.AMOUNT,POS1>
            DEL R.REDO.PENDING.CHARGE<PEN.CHG.DATE,POS1>
            CALL F.WRITE(FN.REDO.PENDING.CHARGE,Y.ACCOUNT.NO,R.REDO.PENDING.CHARGE)
            Y.PENDING.CHG.FLAG = 'YES'
            CALL CACHE.READ(FN.CHARGE.PARAM,"SYSTEM",R.CHARGE.PARAM,CHG.ERR)
            Y.CHARGE.CR.TXN =  R.CHARGE.PARAM<CHG.PARAM.CR.TXN.CODE>
            Y.CHARGE.PL.CAT =  R.CHARGE.PARAM<CHG.PARAM.PL.CATEGORY>
        END
    END

RETURN
*****************
GET.STMT.DETAILS:
*****************

    STMT.ENTRY.REC<AC.STE.THEIR.REFERENCE>   = ID.NEW
    STMT.ENTRY.REC<AC.STE.CUSTOMER.ID>       = Y.CUSTOMER
    STMT.ENTRY.REC<AC.STE.ACCOUNT.OFFICER>   = Y.ACCT.OFFICER
    STMT.ENTRY.REC<AC.STE.PRODUCT.CATEGORY>  = Y.CATEGORY
    STMT.ENTRY.REC<AC.STE.VALUE.DATE>        = TODAY
    STMT.ENTRY.REC<AC.STE.CURRENCY>          = Y.CURRENCY
    STMT.ENTRY.REC<AC.STE.POSITION.TYPE>     = 'TR'
    STMT.ENTRY.REC<AC.STE.OUR.REFERENCE>     = ID.NEW
    STMT.ENTRY.REC<AC.STE.EXPOSURE.DATE>     = TODAY
    STMT.ENTRY.REC<AC.STE.CURRENCY.MARKET>   = '1'
    STMT.ENTRY.REC<AC.STE.DEPARTMENT.CODE>   = R.USER<EB.USE.DEPARTMENT.CODE>
    STMT.ENTRY.REC<AC.STE.TRANS.REFERENCE>   = ID.NEW
    STMT.ENTRY.REC<AC.STE.SYSTEM.ID>         = 'AC'
    STMT.ENTRY.REC<AC.STE.BOOKING.DATE>      = TODAY
    STMT.ENTRY.REC<AC.STE.COMPANY.CODE>      = ID.COMPANY
    STMT.ENTRY.REC<AC.STE.LOCAL.REF,L.EB.IMAGE.ID.SE.POS> = R.NEW(CLEAR.CHQ.IMAGE.REFERENCE)
RETURN

******************
GET.CATEG.DETAILS:
******************

    CATEG.ENTRY.REC<AC.CAT.CUSTOMER.ID>      = Y.CUSTOMER
    CATEG.ENTRY.REC<AC.CAT.ACCOUNT.OFFICER>  = Y.ACCT.OFFICER
    CATEG.ENTRY.REC<AC.CAT.PRODUCT.CATEGORY> = Y.CATEGORY
    CATEG.ENTRY.REC<AC.CAT.VALUE.DATE>       = TODAY
    CATEG.ENTRY.REC<AC.CAT.CURRENCY>         = Y.CURRENCY
    CATEG.ENTRY.REC<AC.CAT.POSITION.TYPE>    = 'TR'
    CATEG.ENTRY.REC<AC.CAT.OUR.REFERENCE>    = ID.NEW
    CATEG.ENTRY.REC<AC.CAT.EXPOSURE.DATE>    = TODAY
    CATEG.ENTRY.REC<AC.CAT.CURRENCY.MARKET>  = '1'
    CATEG.ENTRY.REC<AC.CAT.DEPARTMENT.CODE>  = R.USER<EB.USE.DEPARTMENT.CODE>
    CATEG.ENTRY.REC<AC.CAT.TRANS.REFERENCE>  = ID.NEW
    CATEG.ENTRY.REC<AC.CAT.SYSTEM.ID>        = 'AC'
    CATEG.ENTRY.REC<AC.CAT.BOOKING.DATE>     = TODAY
    CATEG.ENTRY.REC<AC.CAT.COMPANY.CODE>     = ID.COMPANY
    CATEG.ENTRY.REC<AC.CAT.LOCAL.REF,L.EB.IMAGE.ID.CE.POS> = R.NEW(CLEAR.CHQ.IMAGE.REFERENCE)

RETURN
*******************************************
UPDATE.ACCOUNT.STATUS:
*******************************************
    IF R.ACCOUNT<AC.LOCAL.REF,POS.L.AC.STATUS1> EQ 'ACTIVE' THEN
        RETURN
    END
    R.ACCOUNT<AC.LOCAL.REF,POS.L.AC.STATUS1> = 'ACTIVE'
    R.ACCOUNT<AC.WAIVE.LEDGER.FEE>           = ''
    Y.ACC.ID = Y.ACCOUNT.NO
    Y.CUS.ID = R.ACCOUNT<AC.CUSTOMER>
    CUST.JOIN= 'CUSTOMER'
    GOSUB UPD.PRD.LIST
    GOSUB PRD.UPD.JOIN

    Y.TODAY=TODAY
    CALL F.READU(FN.REDO.ACCT.FROM.INACT.TO.ACT,Y.TODAY,R.REDO.ACCT.FROM.INACT.TO.ACT,F.REDO.ACCT.FROM.INACT.TO.ACT,ERR.ACT,'')
    LOCATE Y.ACC.ID IN R.REDO.ACCT.FROM.INACT.TO.ACT SETTING Y.CU.ACCT.IN.POS ELSE
        R.REDO.ACCT.FROM.INACT.TO.ACT<-1>=Y.ACC.ID
    END
    CALL F.WRITE(FN.REDO.ACCT.FROM.INACT.TO.ACT,Y.TODAY,R.REDO.ACCT.FROM.INACT.TO.ACT)
    CALL F.RELEASE(FN.REDO.ACCT.FROM.INACT.TO.ACT,Y.TODAY,F.REDO.ACCT.FROM.INACT.TO.ACT)
    TEMP.V=V
    V=AC.AUDIT.DATE.TIME
    CALL F.LIVE.WRITE(FN.ACCOUNT,Y.ACC.ID,R.ACCOUNT)
    V=TEMP.V

RETURN
*******************
UPD.PRD.LIST:
*******************
    R.CUST.PRD.LIST = ''
    CALL F.READ(FN.CUST.PRD.LIST,Y.CUS.ID,R.CUST.PRD.LIST,F.CUST.PRD.LIST,CUS.ERR)
    IF R.CUST.PRD.LIST THEN
        Y.PRD.LIST=R.CUST.PRD.LIST<PRD.PRODUCT.ID>
    END
    CHANGE @VM TO @FM IN Y.PRD.LIST
    LOCATE Y.ACC.ID IN Y.PRD.LIST SETTING PRD.POS ELSE
    END
    R.CUST.PRD.LIST<PRD.PRODUCT.ID,PRD.POS>   = Y.ACC.ID
    R.CUST.PRD.LIST<PRD.PRD.STATUS,PRD.POS>   = 'ACTIVE'
    R.CUST.PRD.LIST<PRD.TYPE.OF.CUST,PRD.POS> = CUST.JOIN
    R.CUST.PRD.LIST<PRD.DATE,PRD.POS>         = TODAY
    R.CUST.PRD.LIST<PRD.PROCESS.DATE> = TODAY
    CALL F.WRITE(FN.CUST.PRD.LIST,Y.CUS.ID,R.CUST.PRD.LIST)

    CALL F.READ(FN.CUSTOMER,Y.CUS.ID,R.CUSTOMER,F.CUSTOMER,ERR)
    R.CUSTOMER<EB.CUS.CUSTOMER.STATUS>='1'
    TEMP.V=V
    V=EB.CUS.AUDIT.DATE.TIME
    CALL F.LIVE.WRITE(FN.CUSTOMER,Y.CUS.ID,R.CUSTOMER)
    V=TEMP.V

RETURN
*------------
PRD.UPD.JOIN:
*-------------
    IF R.ACCOUNT<AC.JOINT.HOLDER> NE '' THEN
        Y.CUSTOMER.ID.LIST=R.ACCOUNT<AC.JOINT.HOLDER>
        CHANGE @VM TO @FM IN Y.CUSTOMER.ID.LIST
        Y.JOIN.CUST.CNT=1
        Y.JOIN.CUST.MAX=DCOUNT(Y.CUSTOMER.ID.LIST,@FM)
        LOOP
        WHILE Y.JOIN.CUST.CNT LE Y.JOIN.CUST.MAX
            Y.CUS.ID = Y.CUSTOMER.ID.LIST<Y.JOIN.CUST.CNT>
            CUST.JOIN='JOINT.HOLDER'
            GOSUB UPD.PRD.LIST
            Y.JOIN.CUST.CNT += 1 ;*R22 AUTO CODE CONVERSION
        REPEAT
    END
RETURN

*******************
FIND.MULTI.LOC.REF:
*******************

    APPL.ARRAY = 'STMT.ENTRY':@FM:'CATEG.ENTRY':@FM:'ACCOUNT':@FM:'CUSTOMER'
    FLD.ARRAY = 'L.EB.IMAGE.ID':@FM:'L.EB.IMAGE.ID':@FM:'L.AC.AV.BAL':@VM:'L.AC.TRAN.AVAIL':@VM:'L.AC.TRANS.LIM':@VM:'L.AC.STATUS1':@FM:'L.CU.SEGMENTO'
    FLD.POS = ''

    CALL MULTI.GET.LOC.REF(APPL.ARRAY,FLD.ARRAY,FLD.POS)

    L.EB.IMAGE.ID.SE.POS  = FLD.POS<1,1>
    L.EB.IMAGE.ID.CE.POS  = FLD.POS<2,1>
    Y.L.AC.AV.BAL.POS     = FLD.POS<3,1>
    Y.L.AC.TRAN.AVAIL.POS = FLD.POS<3,2>
    Y.L.AC.TRANS.LIM.POS  = FLD.POS<3,3>
    POS.L.AC.STATUS1      = FLD.POS<3,4>
    POS.L.CU.SEGMENTO     = FLD.POS<4,1>
RETURN
*--------------------------------------------------------------------------------------------------------
END
