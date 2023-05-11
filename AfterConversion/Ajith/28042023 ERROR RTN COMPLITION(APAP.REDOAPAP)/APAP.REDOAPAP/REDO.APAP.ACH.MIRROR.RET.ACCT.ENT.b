* @ValidationCode : MjotODA2MDEwODgwOkNwMTI1MjoxNjgyNjY4NTIxNzUxOmFqaXRoOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 28 Apr 2023 13:25:21
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
*-----------------------------------------------------------------------------
* <Rating>-72</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE REDO.APAP.ACH.MIRROR.RET.ACCT.ENT(AMOUNT.VAL,VAR.CURRENCY,REDO.OUTWARD.RETURN.ID)
*-----------------------------------------------------------------------------
* Description:
* This routine is a call routine to generate Entries
*------------------------------------------------------------------------------------------
* * Input / Output
*
* --------------
* IN     : -NA-
* OUT    : -NA-
*------------------------------------------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : RIYAS
* PROGRAM NAME : REDO.APAP.ACH.MIRROR.ACCT.ENT
*------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
* DATE             WHO            REFERENCE         DESCRIPTION
* 12.04.2012      RIYAS             PACS00163293       PACS00163293 FIX
*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*11-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION   NO CHANGE
*11-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION  VM to@VM ,FMto@FM
*----------------------------------------------------------------------------------------

*------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.STMT.ENTRY
    $INSERT I_F.USER
    $INSERT I_F.CATEG.ENTRY
    $INSERT I_F.REDO.APAP.CLEAR.PARAM
    $INSERT I_F.REDO.COLLECT.PARAM
    $INSERT I_F.T24.FUND.SERVICES
    $INSERT I_F.ACCOUNT
    $INSERT I_F.COMPANY


    FN.REDO.APAP.CLEAR.PARAM = 'F.REDO.APAP.CLEAR.PARAM'
    F.REDO.APAP.CLEAR.PARAM = ''
    CALL OPF(FN.REDO.APAP.CLEAR.PARAM,F.REDO.APAP.CLEAR.PARAM)

    FN.T24.FUND.SERVICES = 'F.T24.FUND.SERVICES'
    F.T24.FUND.SERVICES = ''
    CALL OPF(FN.T24.FUND.SERVICES,F.T24.FUND.SERVICES)

    FN.REDO.COLLECT.PARAM = 'F.REDO.COLLECT.PARAM'
    F.REDO.COLLECT.PARAM = ''
    CALL OPF(FN.REDO.COLLECT.PARAM,F.REDO.COLLECT.PARAM)

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    Y.ACH.MIR.CCY = ''
    Y.ACH.MIR.ACCT = ''
    ACH.MIR.CR.CODE = ''
    ACH.MIR.DR.CODE = ''
    Y.OUT.RETURN.ACCT = ''
    Y.OUT.RETURN.CCY = ''
    MULTI.STMT = ''

    GOSUB READ.PARAM.LCY
    GOSUB READ.PARAM.FCY
    GOSUB GET.ACCT.NUMBER

    IF MULTI.STMT THEN

        CALL EB.ACCOUNTING("AC","SAO",MULTI.STMT,'')

    END

RETURN
*****************
READ.PARAM.LCY:
*****************
    IF VAR.CURRENCY EQ 'DOP' THEN
        CALL CACHE.READ(FN.REDO.APAP.CLEAR.PARAM,'SYSTEM',R.REDO.APAP.CLEAR.PARAM,PARAM.ERR)
        Y.ACH.MIR.ACCT = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.CAT.ACH.ACCT>
        ACH.MIR.CR.CODE = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.CAT.ACH.CR.CODE>
        ACH.MIR.DR.CODE = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.CAT.ACH.DR.CODE>
        Y.OUT.RETURN.ACCT = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.OUT.RETURN>
    END
RETURN
***************
READ.PARAM.FCY:
***************
    IF VAR.CURRENCY NE 'DOP' THEN
        CALL CACHE.READ(FN.REDO.COLLECT.PARAM,'SYSTEM',R.REDO.COLLECT.PARAM,REDO.COLLECT.PARAM.ERR)
        Y.ACH.MIR.CCY =   R.REDO.COLLECT.PARAM<COLLECT.PARAM.CAT.ACH.CCY>
        Y.ACH.MIR.ACCT =  R.REDO.COLLECT.PARAM<COLLECT.PARAM.CAT.ACH.ACCT>
        Y.OUT.RETURN.ACCT = R.REDO.COLLECT.PARAM<COLLECT.PARAM.RETURN.ACCOUNT>
        Y.OUT.RETURN.CCY = R.REDO.COLLECT.PARAM<COLLECT.PARAM.RETURN.CCY>
        CHANGE @VM TO @FM IN Y.ACH.MIR.CCY
        CHANGE @VM TO @FM IN Y.ACH.MIR.ACCT ;*R22 MANUAL CODE CONVERSION
        CHANGE@VM TO @FM IN Y.OUT.RETURN.CCY ;*R22 MANUAL CODE CONVERSION
        CHANGE @VM TO @FM IN Y.OUT.RETURN.ACCT
        LOCATE VAR.CURRENCY IN Y.ACH.MIR.CCY SETTING ACH.MIR.CCY.POS THEN
            Y.ACH.MIR.ACCT = Y.ACH.MIR.ACCT<ACH.MIR.CCY.POS>
        END

        LOCATE VAR.CURRENCY IN Y.OUT.RETURN.CCY SETTING OUT.RET.CCY.POS THEN
            Y.OUT.RETURN.ACCT = Y.OUT.RETURN.ACCT<OUT.RET.CCY.POS>
        END
        ACH.MIR.CR.CODE =  R.REDO.COLLECT.PARAM<COLLECT.PARAM.CAT.ACH.CR.CODE>
        ACH.MIR.DR.CODE =  R.REDO.COLLECT.PARAM<COLLECT.PARAM.CAT.ACH.DR.CODE>


    END
RETURN
****************
GET.ACCT.NUMBER:
*****************

    GOSUB ACCT.INFORMATION
    GOSUB ACCT.ENTRIES1
    GOSUB ACCT.ENTRIES2

RETURN
*****************
ACCT.INFORMATION:
*****************
    Y.DEBIT.ACCOUNT  = Y.OUT.RETURN.ACCT
    Y.DEBIT.CURRENCY = VAR.CURRENCY
    CALL F.READ(FN.ACCOUNT,Y.DEBIT.ACCOUNT,R.ACCOUNT,F.ACCOUNT,ACCT.ERR)
    IF R.ACCOUNT THEN
        ACCT.OFF.VAL  = R.ACCOUNT<AC.ACCOUNT.OFFICER>
        CUSTOMER.VAL  = R.ACCOUNT<AC.CUSTOMER>
        PROD.CATEGORY = R.ACCOUNT<AC.CATEGORY>
        Y.CURRENCY    = R.ACCOUNT<AC.CURRENCY>
    END
RETURN

**************
ACCT.ENTRIES1:
**************

    R.STMT.ARR = ''
    R.STMT.ARR<AC.STE.ACCOUNT.NUMBER> = Y.ACH.MIR.ACCT
    R.STMT.ARR<AC.STE.CURRENCY> = Y.CURRENCY
    IF Y.DEBIT.CURRENCY NE LCCY THEN
        R.STMT.ARR<AC.STE.AMOUNT.FCY> = -1 * AMOUNT.VAL
    END
    ELSE
        R.STMT.ARR<AC.STE.AMOUNT.LCY> = -1 * AMOUNT.VAL
    END
    R.STMT.ARR<AC.STE.CRF.TYPE> = "DEBIT"
    R.STMT.ARR<AC.STE.TRANSACTION.CODE> = ACH.MIR.DR.CODE
    GOSUB BASIC.ACC.ENTRY
    MULTI.STMT<-1> = LOWER(R.STMT.ARR)

RETURN

***************
ACCT.ENTRIES2:
***************

    R.STMT.ARR = ''
    R.STMT.ARR<AC.STE.ACCOUNT.NUMBER> = Y.DEBIT.ACCOUNT
    R.STMT.ARR<AC.STE.CURRENCY> = Y.CURRENCY
    IF Y.DEBIT.CURRENCY NE LCCY THEN
        R.STMT.ARR<AC.STE.AMOUNT.FCY> = AMOUNT.VAL
    END
    ELSE
        R.STMT.ARR<AC.STE.AMOUNT.LCY> = AMOUNT.VAL
    END
    R.STMT.ARR<AC.STE.CRF.TYPE> = "CREDIT"
    R.STMT.ARR<AC.STE.TRANSACTION.CODE> = ACH.MIR.CR.CODE
    GOSUB BASIC.ACC.ENTRY
    MULTI.STMT<-1> = LOWER(R.STMT.ARR)
RETURN

****************
BASIC.ACC.ENTRY:
****************
*Common Call for raising Entries

    R.STMT.ARR<AC.STE.COMPANY.CODE> = ID.COMPANY
    R.STMT.ARR<AC.STE.CUSTOMER.ID> = CUSTOMER.VAL
    R.STMT.ARR<AC.STE.ACCOUNT.OFFICER> = ACCT.OFF.VAL
    R.STMT.ARR<AC.STE.PRODUCT.CATEGORY> = PROD.CATEGORY
    R.STMT.ARR<AC.STE.VALUE.DATE> = TODAY
    R.STMT.ARR<AC.STE.POSITION.TYPE> = "TR"
    R.STMT.ARR<AC.STE.OUR.REFERENCE> = REDO.OUTWARD.RETURN.ID
    R.STMT.ARR<AC.STE.TRANS.REFERENCE> = REDO.OUTWARD.RETURN.ID
    R.STMT.ARR<AC.STE.SYSTEM.ID> = "AC"
    R.STMT.ARR<AC.STE.BOOKING.DATE> = TODAY
    R.STMT.ARR<AC.STE.EXPOSURE.DATE> = TODAY
    R.STMT.ARR<AC.STE.CURRENCY.MARKET> = 1
    R.STMT.ARR<AC.STE.DEPARTMENT.CODE> = 1
    R.STMT.ARR<AC.STE.PROCESSING.DATE> = TODAY
    R.STMT.ARR<AC.STE.ORIG.CCY.MARKET> = 1
RETURN
END
