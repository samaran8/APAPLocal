* @ValidationCode : MjoxODE5MTg0OTI2OkNwMTI1MjoxNjg1NTM1NzA0NjY5OnNhbWFyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 31 May 2023 17:51:44
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
SUBROUTINE REDO.B.CRF.NWGL(ACCT.ID)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
* DATE  NAME   REFERENCE DESCRIPTION
* 31 JAN 2023 Edwin Charles D         ACCOUNTING-CR   TSR479892
*25-05-2023       Conversion Tool          R22 Auto Code conversion             No Changes
*25-05-2023       Harishvikram C             Manual R22 Code Conversion         CALL routine format modified
*-----------------------------------------------------------------------------

*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CUSTOMER
    $INSERT I_F.REDO.RE.CRF.NWGL
    $INSERT I_F.EB.CONTRACT.BALANCES
    $INSERT I_REDO.CRF.NWGL.COMMON
    $INSERT I_REDO.PREVAL.STATUS.COMMON

    Y.COMP = ID.COMPANY
****ACCOUNT RECORDS*****
    CALL F.READ(FN.ACCOUNT,ACCT.ID,ACCT.REC,F.ACCOUNT,ACCT.ERR)
    Y.CURRENCY = ACCT.REC<AC.CURRENCY>
    Y.CUSTOMER = ACCT.REC<AC.CUSTOMER>
    Y.COMPANY = ACCT.REC<AC.CO.CODE>
    IF Y.COMP EQ Y.COMPANY THEN

****CUSTOMER RECORD***
        CALL F.READ(FN.CUSTOMER,Y.CUSTOMER,R.CUSTOMER,F.CUSTOMER,CUST.ERR)
        Y.SECTOR = R.CUSTOMER<EB.CUS.SECTOR>

****ECB CONSOLE *****
        CALL F.READ(FN.ECB,ACCT.ID,R.EB.CONTRACT.BALANCES,F.ECB,EB.CONT.ERROR)
        CONSOLE.KEY = R.EB.CONTRACT.BALANCES<ECB.CONSOL.KEY>

*****CHECK BAL ENTRIES*****
        APAP.REDOAPAP.redoVCheckBalIntEntries(ACCT.ID,BAL.CR.AMT,BAL.DR.AMT,INT.CR.AMT,INT.DR.AMT,Y.BAL.DET,Y.INT.DET) ;*Manual R22 Code Conversion

        Y.BAL.ACC.NO = FIELD(Y.BAL.DET, '*', 1)
        Y.BAL.NAME = FIELD(Y.BAL.DET, '*', 2)
        Y.INT.ACC.NO = FIELD(Y.INT.DET, '*', 1)
        Y.INT.NAME = FIELD(Y.INT.DET, '*', 2)

        IF BAL.CR.AMT THEN
            Y.CRF.ID = "NWGL.3367.":Y.COMPANY:'*':Y.CURRENCY:'*':CONSOLE.KEY:'*':ACCT.ID:'*':'CREDIT'
            Y.DESC1 = Y.BAL.ACC.NO
            Y.DESC2 = Y.BAL.NAME
            Y.DEAL.BAL = BAL.CR.AMT
            Y.D.L.BAL = BAL.CR.AMT
            GOSUB UPDATE.CRF.VALUES
        END

        IF BAL.DR.AMT THEN
            Y.CRF.ID = "NWGL.3367.":Y.COMPANY:'*':Y.CURRENCY:'*':CONSOLE.KEY:'*':ACCT.ID:'*':'DEBIT'
            Y.DESC1 = Y.BAL.ACC.NO
            Y.DESC2 = Y.BAL.NAME
            Y.DEAL.BAL = BAL.DR.AMT
            Y.D.L.BAL = BAL.DR.AMT
            GOSUB UPDATE.CRF.VALUES
        END

        IF INT.CR.AMT THEN
            Y.CRF.ID = "NWGL.5386.":Y.COMPANY:'*':Y.CURRENCY:'*':CONSOLE.KEY:'*':ACCT.ID:'*':'ACCCRINTEREST'
            Y.DESC1 = Y.INT.ACC.NO
            Y.DESC2 = Y.INT.NAME
            Y.DEAL.BAL = INT.CR.AMT
            Y.D.L.BAL = INT.CR.AMT
            GOSUB UPDATE.CRF.VALUES
        END

        IF INT.DR.AMT THEN
            Y.CRF.ID = "NWGL.5386.":Y.COMPANY:'*':Y.CURRENCY:'*':CONSOLE.KEY:'*':ACCT.ID:'*':'ACCCRINTERESTDEBIT'
            Y.DESC1 = Y.INT.ACC.NO
            Y.DESC2 = Y.INT.NAME
            Y.DEAL.BAL = INT.DR.AMT
            Y.D.L.BAL = INT.DR.AMT
            GOSUB UPDATE.CRF.VALUES
        END
    END

RETURN
UPDATE.CRF.VALUES:
******************
*    CALL F.READ(FN.REDO.CRF,Y.CRF.ID,R.REDO.CRF,F.REDO.CRF,CRF.ERR)
*    R.REDO.CRF<CRF.CURRENCY> = Y.CURRENCY
*    R.REDO.CRF<CRF.DESC.1> = Y.DESC1
*    R.REDO.CRF<CRF.DESC.2> = Y.DESC2
*    R.REDO.CRF<CRF.SECTOR> = Y.SECTOR
*    R.REDO.CRF<CRF.CUSTOMER.NO> = Y.CUSTOMER
*    R.REDO.CRF<CRF.CONSOL.KEY> = CONSOLE.KEY
*    R.REDO.CRF<CRF.DEAL.BALANCE> = Y.DEAL.BAL
*    R.REDO.CRF<CRF.DEAL.LCY.BALANCE> = Y.D.L.BAL
*    R.REDO.CRF<CRF.DEAL.RATE> = '0.000'
*    R.REDO.CRF<CRF.DEAL.VALUE.DATE> = TODAY
*    CALL F.WRITE(FN.REDO.CRF,Y.CRF.ID,R.REDO.CRF)

    R.RE.CRF.NWGL = '' ; RE.CRF.ERR = ''
    CALL F.READ(FN.RE.CRF.NWGL,Y.CRF.ID,R.RE.CRF.NWGL,F.RE.CRF.NWGL,RE.CRF.ERR)
    R.RE.CRF.NWGL<1> = Y.CURRENCY
    R.RE.CRF.NWGL<2> = ''
    R.RE.CRF.NWGL<3> = Y.DESC2
    R.RE.CRF.NWGL<4> = Y.DESC1
    R.RE.CRF.NWGL<5> = Y.SECTOR
    R.RE.CRF.NWGL<11> = CONSOLE.KEY
    R.RE.CRF.NWGL<12> = Y.CUSTOMER
    R.RE.CRF.NWGL<13> = Y.DEAL.BAL
    R.RE.CRF.NWGL<14> = Y.D.L.BAL
    R.RE.CRF.NWGL<15> = '0.000'
    R.RE.CRF.NWGL<16> = TODAY
    CALL F.WRITE(FN.RE.CRF.NWGL,Y.CRF.ID,R.RE.CRF.NWGL)

RETURN
END
