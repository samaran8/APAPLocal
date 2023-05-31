* @ValidationCode : Mjo2OTcwMDAwOTY6Q3AxMjUyOjE2ODUwNzk5ODIxNTk6SVRTUzotMTotMTo3OTg6MTpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 26 May 2023 11:16:22
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 798
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOAPAP
SUBROUTINE REDO.B.CRF.NWGL.LOAD
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
* DATE  NAME   REFERENCE DESCRIPTION
* 31 JAN 2023 Edwin Charles D         ACCOUNTING-CR     TSR479892
*25-05-2023        Conversion Tool          R22 Auto Code conversion      FM TO @FM,VM TO @VM ,SM TO @SM
*25-05-2023        Harishvikram C            Manual R22 Code Conversion    No Changes
*-----------------------------------------------------------------------------

*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CUSTOMER
    $INSERT I_F.REDO.T.ACCTSTAT.BY.DATE
    $INSERT I_F.EB.CONTRACT.BALANCES
    $INSERT I_REDO.CRF.NWGL.COMMON
    $INSERT I_F.REDO.RE.CRF.NWGL
    $INSERT I_F.REDO.PREVALANCE.STATUS
    $INSERT I_REDO.PREVAL.STATUS.COMMON

    FN.REDO.T.ACCTSTAT.BY.DATE = 'F.REDO.T.ACCTSTAT.BY.DATE'
    F.REDO.T.ACCTSTAT.BY.DATE = ''
    CALL OPF(FN.REDO.T.ACCTSTAT.BY.DATE,F.REDO.T.ACCTSTAT.BY.DATE)

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER, F.CUSTOMER)

    FN.ECB = 'F.EB.CONTRACT.BALANCES'
    F.ECB = ''
    CALL OPF(FN.ECB,F.ECB)

    FN.REDO.CRF = 'F.REDO.RE.CRF.NWGL'
    F.REDO.CRF = ''
    CALL OPF(FN.REDO.CRF,F.REDO.CRF)

    FN.RE.CRF.NWGL = 'F.RE.CRF.NWGL'
    F.RE.CRF.NWGL = ''
    CALL OPF(FN.RE.CRF.NWGL,F.RE.CRF.NWGL)

    FN.DATES = 'F.DATES'
    F.DATES = ''
    CALL OPF(FN.DATES, F.DATES)

    Y.BAL.RECLASSIFY.LIST = ''; Y.INT.RECLASSIFY.LIST = '';   PARAM.STATUS = ''; PREVALANCE.STATUS = ''; STAT.FM.CNTR = 0 ; ACCT.TYPE.LIST = ''
    FN.REDO.PREVALANCE.STATUS = 'F.REDO.PREVALANCE.STATUS'
    F.REDO.PREVALANCE.STATUS = ''
    CALL OPF(FN.REDO.PREVALANCE.STATUS,F.REDO.PREVALANCE.STATUS)

    CALL CACHE.READ(FN.REDO.PREVALANCE.STATUS,'SYSTEM',R.REDO.PREVALANCE.STATUS,F.ERR)

    PARAM.STATUS.VAL = R.REDO.PREVALANCE.STATUS<REDO.PRE.STATUS>
    PREVALANCE.STATUS.VAL = CHANGE(R.REDO.PREVALANCE.STATUS<REDO.PRE.PREVALANT.STATUS>,@VM,@FM)
    ACCT.TYPE.VAL = CHANGE(R.REDO.PREVALANCE.STATUS<REDO.PRE.ACCT.TYPE>,@VM,@FM)
    Y.BAL.RECLASSIFY.LIST = CHANGE(R.REDO.PREVALANCE.STATUS<REDO.PRE.BAL.RECLASS>,@VM,@FM)
    Y.INT.RECLASSIFY.LIST = CHANGE(R.REDO.PREVALANCE.STATUS<REDO.PRE.INT.RECLASS>,@VM,@FM)

    STAT.FM.CNTR = DCOUNT(PARAM.STATUS.VAL,@VM)
    LOOP.FM.CNTR = 1

    LOOP
    WHILE LOOP.FM.CNTR LE STAT.FM.CNTR
        Y.FM.STATUS = PARAM.STATUS.VAL<1,LOOP.FM.CNTR>
        Y.FM.STATUS = CHANGE(Y.FM.STATUS,@FM,':')
        Y.FM.STATUS = CHANGE(Y.FM.STATUS,@SM,':')
        PARAM.STATUS<-1> = Y.FM.STATUS
        PREVALANCE.STATUS<-1> = PREVALANCE.STATUS.VAL<LOOP.FM.CNTR>
        ACCT.TYPE.LIST<-1> = ACCT.TYPE.VAL<LOOP.FM.CNTR>
*        Y.BAL.RECLASSIFY.LIST<-1> = Y.BAL.RECLASSIFY.VAL<LOOP.FM.CNTR>
*       Y.INT.RECLASSIFY.LIST<-1> = Y.INT.RECLASSIFY.VAL<LOOP.FM.CNTR>
        LOOP.FM.CNTR + = 1
    REPEAT

RETURN
END
