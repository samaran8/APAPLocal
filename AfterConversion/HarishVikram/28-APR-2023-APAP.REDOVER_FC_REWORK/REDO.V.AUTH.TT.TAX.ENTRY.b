* @ValidationCode : MjotNTQ1OTc4NTU1OkNwMTI1MjoxNjgyNDEyMzQwNDMyOkhhcmlzaHZpa3JhbUM6LTE6LTE6MDoxOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 25 Apr 2023 14:15:40
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.V.AUTH.TT.TAX.ENTRY
***********************************************************
*----------------------------------------------------------
* COMPANY NAME    : APAP
* DEVELOPED BY    : Pradeep S
* PROGRAM NAME    : REDO.V.AUTH.TT.TAX.ENTRY
*----------------------------------------------------------


* DESCRIPTION     : This routine is a auth routine attached to verisons
* TELLER,REDO.TRANSF.CTAS.ACCT and TELLER,REDO.TRANSF.CTAS.ACCT.FCY to post the accounting
* entry for the calculated TAX.
*------------------------------------------------------------

* LINKED WITH    : TELLER INPUT ROUTINE
* IN PARAMETER   : NONE
* OUT PARAMETER  : NONE

*----------------------------------------------------------------------
* Modification History :
*-----------------------
*DATE             WHO             REFERENCE       DESCRIPTION
*13.10.2011       Pradeep S       PACS00141531    INITIAL CREATION
*----------------------------------------------------------------------
*Modification History
*DATE                       WHO                         REFERENCE                                   DESCRIPTION
*10-04-2023            Conversion Tool             R22 Auto Code conversion                VM TO @VM,F.READ TO CACHE.READ
*10-04-2023              Samaran T                R22 Manual Code conversion                         No Changes
*--------------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.TELLER
    $INSERT I_F.TELLER.TRANSACTION
    $INSERT I_F.TAX
    $INSERT I_F.STMT.ENTRY
    $INSERT I_F.COMPANY
    $INSERT I_F.ACCOUNT
    $INSERT I_F.USER
    $INSERT I_GTS.COMMON

    GOSUB INIT
    GOSUB OPEN.FILES
    GOSUB PRE.PROCESS

    IF PROCESS.GOHEAD THEN
        GOSUB PROCESS
    END

RETURN  ;* END

*********
PROCESS:
*********

    GOSUB GET.OTHER.INFO
    GOSUB DR.ENTRY
    Y.COMMON.ARRAY = LOWER(R.STMT.ARR)
    GOSUB CR.ENTRY
    Y.COMMON.ARRAY<-1> = LOWER(R.STMT.ARR)
    GOSUB EB.ACCOUNTING

RETURN  ;* Return PROCESS

***************
GET.OTHER.INFO:
***************

    CALL F.READ(FN.ACCOUNT,Y.DR.ACCT.NO,R.ACCOUNT,F.ACCOUNT,ERR.ACC)
    IF R.ACCOUNT THEN
        Y.CUST.ID = R.ACCOUNT<AC.CUSTOMER>
        Y.ACCT.OFFICER = R.ACCOUNT<AC.ACCOUNT.OFFICER>
        Y.ACCT.CATEGORY = R.ACCOUNT<AC.CATEGORY>
    END

    Y.SUB.DIV.CODE  = R.COMPANY(EB.COM.SUB.DIVISION.CODE)
    Y.EXCHANGE.RATE = R.NEW(TT.TE.RATE.1)

    Y.TAX.AMT.FCY = ''
    Y.TAX.AMT.LCY = ''

    IF Y.TXN.CCY EQ LCCY THEN
        Y.TAX.AMT.LCY = Y.TAX.AMT
        Y.TAX.AMT.FCY = ''
    END

    IF Y.TXN.CCY NE LCCY THEN
        Y.TAX.AMT.FCY = Y.TAX.AMT
        Y.TAX.AMT.LCY = Y.TAX.AMT * Y.EXCHANGE.RATE
    END

RETURN  ;* Return GET.OTHER.INFO

**********
DR.ENTRY:
**********

    R.STMT.ARR = ''
    R.STMT.ARR<AC.STE.ACCOUNT.NUMBER> = Y.DR.ACCT.NO
    R.STMT.ARR<AC.STE.COMPANY.CODE> = ID.COMPANY
    R.STMT.ARR<AC.STE.AMOUNT.LCY> = Y.TAX.AMT.LCY * '-1'
    R.STMT.ARR<AC.STE.TRANSACTION.CODE> = Y.TR.CODE.DR
    R.STMT.ARR<AC.STE.THEIR.REFERENCE>=''
    R.STMT.ARR<AC.STE.CUSTOMER.ID> = Y.CUST.ID
    R.STMT.ARR<AC.STE.ACCOUNT.OFFICER> = Y.ACCT.OFFICER
    R.STMT.ARR<AC.STE.PRODUCT.CATEGORY> = Y.ACCT.CATEGORY
    R.STMT.ARR<AC.STE.VALUE.DATE> = R.NEW(TT.TE.VALUE.DATE.1)<1,1>
    R.STMT.ARR<AC.STE.CURRENCY> = Y.TXN.CCY
    R.STMT.ARR<AC.STE.POSITION.TYPE> = 'TR'
    IF Y.TXN.CCY EQ LCCY THEN
        R.STMT.ARR<AC.STE.AMOUNT.FCY> = ''
        R.STMT.ARR<AC.STE.EXCHANGE.RATE> = ''
    END ELSE
        R.STMT.ARR<AC.STE.AMOUNT.FCY> = Y.TAX.AMT.FCY * '-1'
        R.STMT.ARR<AC.STE.EXCHANGE.RATE> = Y.EXCHANGE.RATE
    END
    R.STMT.ARR<AC.STE.DEPARTMENT.CODE> = R.USER<EB.USE.DEPARTMENT.CODE>
    R.STMT.ARR<AC.STE.CURRENCY.MARKET> = '1'
    R.STMT.ARR<AC.STE.TRANS.REFERENCE> = ID.NEW
    R.STMT.ARR<AC.STE.SYSTEM.ID> = "TT"
    R.STMT.ARR<AC.STE.BOOKING.DATE> = TODAY

RETURN  ;* Return DR.ENTRY


**********
CR.ENTRY:
**********

    R.STMT.ARR = ''
    R.STMT.ARR<AC.STE.ACCOUNT.NUMBER> = Y.TXN.CCY:Y.TAX.CATEGORY:"0001":Y.SUB.DIV.CODE
    R.STMT.ARR<AC.STE.COMPANY.CODE> = ID.COMPANY
    R.STMT.ARR<AC.STE.AMOUNT.LCY> = Y.TAX.AMT.LCY
    R.STMT.ARR<AC.STE.TRANSACTION.CODE> = Y.TR.CODE.CR
    R.STMT.ARR<AC.STE.THEIR.REFERENCE>=''
    R.STMT.ARR<AC.STE.CUSTOMER.ID> = Y.CUST.ID
    R.STMT.ARR<AC.STE.ACCOUNT.OFFICER> = Y.ACCT.OFFICER
    R.STMT.ARR<AC.STE.PRODUCT.CATEGORY> = Y.ACCT.CATEGORY
    R.STMT.ARR<AC.STE.VALUE.DATE> = R.NEW(TT.TE.VALUE.DATE.1)<1,1>
    R.STMT.ARR<AC.STE.CURRENCY> = Y.TXN.CCY
    R.STMT.ARR<AC.STE.POSITION.TYPE> = 'TR'
    IF Y.TXN.CCY EQ LCCY THEN
        R.STMT.ARR<AC.STE.AMOUNT.FCY> = ''
        R.STMT.ARR<AC.STE.EXCHANGE.RATE> = ''
    END ELSE
        R.STMT.ARR<AC.STE.AMOUNT.FCY> = Y.TAX.AMT.FCY
        R.STMT.ARR<AC.STE.EXCHANGE.RATE> = Y.EXCHANGE.RATE
    END
    R.STMT.ARR<AC.STE.DEPARTMENT.CODE> = R.USER<EB.USE.DEPARTMENT.CODE>
    R.STMT.ARR<AC.STE.CURRENCY.MARKET> = '1'
    R.STMT.ARR<AC.STE.TRANS.REFERENCE> = ID.NEW
    R.STMT.ARR<AC.STE.SYSTEM.ID> = "TT"
    R.STMT.ARR<AC.STE.BOOKING.DATE> = TODAY

RETURN  ;* Return CR.ENTRY

***************
EB.ACCOUNTING:
***************
    V = TT.TE.AUDIT.DATE.TIME
    CALL EB.ACCOUNTING("TT","SAO",Y.COMMON.ARRAY,'')

RETURN  ;* Return EB.ACCOUNTING

******
INIT:
******

    PROCESS.GOHEAD = @FALSE
    LOC.REF.APPLICATION="TELLER"
    LOC.REF.FIELDS='WAIVE.TAX':@VM:'L.TT.WAI.CHARGE':@VM:'L.TT.BASE.AMT':@VM:'L.LETTER.ID':@VM:'L.TT.ADD.INFO':@VM:'L.TT.TAX.TYPE':@VM:'TAX.AMOUNT'
    LOC.REF.POS=''

    CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LREF.POS)
    POS.WAIVE.TAX    = LREF.POS<1,1>
    POS.WAIVE.CHARGE = LREF.POS<1,2>
    POS.BASE.AMT     = LREF.POS<1,3>

    POS.STACK.TAX    = LREF.POS<1,4>
    POS.STACK.CHARGE = LREF.POS<1,5>
    TT.TAX.POS       = LREF.POS<1,6>
    POS.TAX.AMOUNT   = LREF.POS<1,7>

RETURN  ;* Return INIT

************
OPEN.FILES:
************

    FN.TELLER.TRANSACTION = 'F.TELLER.TRANSACTION'
    F.TELLER.TRANSACTION = ''
    R.TELLER.TRANSACTION = ''
    CALL OPF(FN.TELLER.TRANSACTION,F.TELLER.TRANSACTION)

    FN.TAX = 'F.TAX'
    F.TAX = ''
    R.TAX = ''
    CALL OPF(FN.TAX,F.TAX)

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT  = ''
    R.ACCOUNT  = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

RETURN  ;* Return OPEN.FILES

*************
PRE.PROCESS:
*************

    Y.TAX.AMT    = R.NEW(TT.TE.LOCAL.REF)<1,POS.TAX.AMOUNT>
    Y.TXN.CCY    = R.NEW(TT.TE.CURRENCY.1)
    Y.DR.ACCT.NO = R.NEW(TT.TE.ACCOUNT.1)<1,1>
    TAXATION.CODE = R.NEW(TT.TE.LOCAL.REF)<1,TT.TAX.POS>

    IF TAXATION.CODE THEN
        SEL.CMD = "SELECT ":FN.TAX:" WITH @ID LIKE ":TAXATION.CODE:"... BY-DSND @ID"
        CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.REC,RET.ERR)
        TAXATION.CODE = SEL.LIST<1>
        CALL CACHE.READ(FN.TAX, TAXATION.CODE, R.TAX, ERR.TAX)   ;*R22 AUTO CODE CONVERSION
        IF R.TAX THEN
            Y.TR.CODE.DR   = R.TAX<EB.TAX.TR.CODE.DR>
            Y.TR.CODE.CR   = R.TAX<EB.TAX.TR.CODE.CR>
            Y.TAX.CATEGORY = R.TAX<EB.TAX.CATEGORY>
        END
    END

    IF Y.TAX.AMT GT 0 THEN
        PROCESS.GOHEAD = @TRUE
    END
RETURN  ;* Return PRE.PROCESS


END
