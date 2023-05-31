* @ValidationCode : MjotNzIxNTAzMjI4OkNwMTI1MjoxNjg1NTM1NjI4ODM4OnNhbWFyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 31 May 2023 17:50:28
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
$PACKAGE APAP.REDOAPAP
SUBROUTINE REDO.NOFILE.CRB.REPORT.REGL(Y.ARRAY)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
* DATE  NAME   REFERENCE    DESCRIPTION
* 31 JAN 2023 Edwin Charles D         ACCOUNTING-CR           TSR479892
*25-05-2023            Conversion Tool             R22 Auto Code conversion                FM TO @FM,VM TO @VM,++ to +=1
*25-05-2023            Harishvikram C            R22 Manual Code conversion                CALL routine format modified
*-----------------------------------------------------------------------------

*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CUSTOMER
    $INSERT I_F.REDO.T.ACCTSTAT.BY.DATE

    GOSUB OPEN.TABLE
    GOSUB PROCESS

RETURN

OPEN.TABLE:
**********

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT, F.ACCOUNT)

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER, F.CUSTOMER)

    FN.REDO.T.ACCTSTAT.BY.DATE = 'F.REDO.T.ACCTSTAT.BY.DATE'
    F.REDO.T.ACCTSTAT.BY.DATE = ''
    CALL OPF(FN.REDO.T.ACCTSTAT.BY.DATE,F.REDO.T.ACCTSTAT.BY.DATE)

RETURN

PROCESS:
********

    DATE.ID = TODAY
    INT.NAME.LIST = '' ; BAL.NAME.LIST = ''
    CALL F.READ(FN.REDO.T.ACCTSTAT.BY.DATE,DATE.ID,DAT.REC,F.REDO.T.ACCTSTAT.BY.DATE,DAT.ER)
    LOOP
        REMOVE ACCT.ID FROM DAT.REC SETTING POS
    WHILE ACCT.ID:POS DO
        BAL.CR.AMT = '' ; BAL.DR.AMT = '' ; INT.CR.AMT = '' ;INT.DR.AMT = '' ; Y.BAL.ACC.NO = '' ; Y.INT.ACC.NO = '' ; Y.BAL.DET = '' ; Y.INT.DET = '' ; Y.BAL.NAME = '' ;Y.INT.NAME = ''
        APAP.REDOAPAP.redoVCheckBalIntEntries(ACCT.ID,BAL.CR.AMT,BAL.DR.AMT,INT.CR.AMT,INT.DR.AMT,Y.BAL.DET,Y.INT.DET) ;*Manual R22 Code Conversion
        IF BAL.CR.AMT THEN
            CUST.NAME = '' ; R.ACCOUNT = '' ; R.CUSTOMER = ''
            CALL F.READ(FN.ACCOUNT,ACCT.ID,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
            Y.CUST.ID = R.ACCOUNT<AC.CUSTOMER>
            CALL F.READ(FN.CUSTOMER,Y.CUST.ID,R.CUSTOMER,F.CUSTOMER,CUST.ERR)
            CUST.NAME = R.CUSTOMER<EB.CUS.SHORT.NAME>
            Y.BAL.ACC.NO = FIELD(Y.BAL.DET, '*', 1)
            Y.BAL.NAME = FIELD(Y.BAL.DET, '*', 2)
            Y.INT.ACC.NO = FIELD(Y.INT.DET, '*', 1)
            Y.INT.NAME = FIELD(Y.INT.DET, '*', 2)

            LOCATE Y.BAL.ACC.NO IN BAL.ACCT.LIST SETTING ACT.POS THEN
                Y.BAL.ARRAY<ACT.POS,-1> = ACCT.ID:'*':'DOP':'*':CUST.NAME:'*':BAL.CR.AMT:'*':BAL.DR.AMT
            END ELSE

                BAL.ACCT.LIST<-1> = Y.BAL.ACC.NO
                BAL.NAME.LIST<-1> = Y.BAL.NAME
                BAL.CNT = DCOUNT(BAL.ACCT.LIST,@FM)
                Y.BAL.ARRAY<BAL.CNT,-1> = ACCT.ID:'*':'DOP':'*':CUST.NAME:'*':BAL.CR.AMT:'*':BAL.DR.AMT
            END

            IF INT.CR.AMT THEN
                LOCATE Y.INT.ACC.NO IN INT.ACCT.LIST SETTING ACT.POS THEN
                    Y.INT.ARRAY<ACT.POS,-1> = ACCT.ID:'*':'DOP':'*':CUST.NAME:'*':INT.CR.AMT:'*':INT.DR.AMT
                END ELSE
                    INT.ACCT.LIST<-1> = Y.INT.ACC.NO
                    INT.NAME.LIST<-1> = Y.INT.NAME
                    INT.CNT = DCOUNT(INT.ACCT.LIST,@FM)
                    Y.INT.ARRAY<INT.CNT,-1> = ACCT.ID:'*':'DOP':'*':CUST.NAME:'*':INT.CR.AMT:'*':INT.DR.AMT
                END
            END

        END
    REPEAT

    Y.NULL = ''
    ACCNT.ACCT.LIST = BAL.ACCT.LIST
    ACCNT.NAME.LIST = BAL.NAME.LIST
    Y.SUB.ARRAY = Y.BAL.ARRAY

    GOSUB FORMAT.REPORT
    ACCNT.ACCT.LIST = INT.ACCT.LIST
    ACCNT.NAME.LIST = INT.NAME.LIST
    Y.SUB.ARRAY = Y.INT.ARRAY
    GOSUB FORMAT.REPORT

RETURN

FORMAT.REPORT:
**************
    TOT.ACCT = DCOUNT(ACCNT.ACCT.LIST, @FM)
    ACT.CNT = 1
    LOOP
    WHILE ACT.CNT LE TOT.ACCT
        Y.ARRAY<-1> = ACCNT.ACCT.LIST<ACT.CNT>:' ':ACCNT.NAME.LIST<ACT.CNT>:'*':Y.NULL:'*':Y.NULL:'*':Y.NULL:'*':Y.NULL
        TOT.SUB.ACCT = DCOUNT(Y.SUB.ARRAY<ACT.CNT>,@VM)
        SUB.CNT = 1
        LOOP
        WHILE SUB.CNT LE TOT.SUB.ACCT
            Y.ARRAY<-1> = Y.SUB.ARRAY<ACT.CNT,SUB.CNT>
            SUB.CNT += 1 ;*R22 AUTO CODE CONVERSION
        REPEAT
        ACT.CNT += 1  ;*R22 AUTO CODE CONVERSION
    REPEAT

RETURN
END
