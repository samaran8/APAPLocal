* @ValidationCode : Mjo3ODcyNzExMjg6Q3AxMjUyOjE2ODI2NjkwNzc1NDI6YWppdGg6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 28 Apr 2023 13:34:37
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
SUBROUTINE REDO.APAP.BRANCH.CLR.ACCT.ENT

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
* PROGRAM NAME : REDO.APAP.CLR.ACCT.ENTRIES
*------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
* DATE             WHO            REFERENCE         DESCRIPTION
* 12.04.2012      RIYAS             PACS00163293       PACS00163293 FIX
*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*11-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION   NO CHANGE
*11-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION FMto@FM
*----------------------------------------------------------------------------------------



*------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.STMT.ENTRY
    $INSERT I_F.USER
    $INSERT I_F.CATEG.ENTRY
    $INSERT I_REDO.B.INW.PROCESS.COMMON
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

    GOSUB READ.PARAM.LCY
    GOSUB READ.PARAM.FCY
    GOSUB GET.ACCT.NUMBER

    IF MULTI.STMT THEN
        Y.BACKUP = R.NEW(TFS.LOCAL.REF)
        CALL EB.ACCOUNTING("AC","SAO",MULTI.STMT,'')
        R.NEW(TFS.LOCAL.REF) = Y.BACKUP
    END

RETURN
*****************
READ.PARAM.LCY:
*****************
    IF PGM.VERSION EQ ",LCY.COLLECT" THEN
        CALL CACHE.READ(FN.REDO.APAP.CLEAR.PARAM,'SYSTEM',R.REDO.APAP.CLEAR.PARAM,PARAM.ERR)
        ARR.LIST       = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.TT.FT.TRAN.TYPE>
        Y.UNIV.ACCOUNT = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.UNIV.CLEAR.ACCT>
        ADJUST.CR.CODE = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.CLEARING.CR.CODE>
        ADJUST.DR.CODE = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.CLEARING.DR.CODE>
    END
RETURN
***************
READ.PARAM.FCY:
***************
    IF PGM.VERSION EQ ",FCY.COLLECT" THEN
        CALL CACHE.READ(FN.REDO.COLLECT.PARAM,'SYSTEM',R.REDO.COLLECT.PARAM,REDO.COLLECT.PARAM.ERR)
        ARR.LIST       =  R.REDO.COLLECT.PARAM<COLLECT.PARAM.TT.FT.TRAN.TYPE>
        Y.UNIV.ACCOUNT =  R.REDO.COLLECT.PARAM<COLLECT.PARAM.UNIV.CLEAR.ACCT>
        ADJUST.CR.CODE =  R.REDO.COLLECT.PARAM<COLLECT.PARAM.CLEARING.CR.CODE>
        ADJUST.DR.CODE =  R.REDO.COLLECT.PARAM<COLLECT.PARAM.CLEARING.DR.CODE>
    END
RETURN
****************
GET.ACCT.NUMBER:
*****************
    CHANGE @VM TO @FM IN ARR.LIST
    TFS.TXN.LIST = R.NEW(TFS.TRANSACTION)
    TFS.LIST.COUNT = DCOUNT(TFS.TXN.LIST,@VM) ;*R22 MANUAL CODE CONVERSION
    VAR.COUNT = 1
    LOOP
        REMOVE TXN.ID FROM TFS.TXN.LIST SETTING TXN.POS
    WHILE VAR.COUNT LE TFS.LIST.COUNT
        LOCATE TXN.ID IN ARR.LIST SETTING ARR.POS THEN
            Y.CHQ.FLAG = 1
            GOSUB ACCT.INFORMATION
            GOSUB ACCT.ENTRIES1
            GOSUB ACCT.ENTRIES2
        END
        VAR.COUNT++
        Y.CHQ.FLAG = ''
    REPEAT
RETURN
*****************
ACCT.INFORMATION:
*****************
    Y.DEBIT.ACCOUNT  = R.NEW(TFS.ACCOUNT.DR)<1,VAR.COUNT>
    Y.DEBIT.CURRENCY = Y.DEBIT.ACCOUNT[1,3]
    Y.CHQ.AMOUNT =R.NEW(TFS.AMOUNT)<1,VAR.COUNT>
    IF Y.DEBIT.ACCOUNT[13,4] THEN
    END ELSE
        Y.SUB.CODE = R.COMPANY(EB.COM.SUB.DIVISION.CODE)
        Y.DEBIT.ACCOUNT = Y.DEBIT.ACCOUNT:Y.SUB.CODE
    END
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
    R.STMT.ARR<AC.STE.ACCOUNT.NUMBER> = Y.UNIV.ACCOUNT
    R.STMT.ARR<AC.STE.CURRENCY> = Y.CURRENCY
    IF Y.DEBIT.CURRENCY NE LCCY THEN
        R.STMT.ARR<AC.STE.AMOUNT.FCY> = -1 * Y.CHQ.AMOUNT
    END
    ELSE
        R.STMT.ARR<AC.STE.AMOUNT.LCY> = -1 * Y.CHQ.AMOUNT
    END
    R.STMT.ARR<AC.STE.CRF.TYPE> = "DEBIT"
    R.STMT.ARR<AC.STE.TRANSACTION.CODE> = ADJUST.DR.CODE
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
        R.STMT.ARR<AC.STE.AMOUNT.FCY> = Y.CHQ.AMOUNT
    END
    ELSE
        R.STMT.ARR<AC.STE.AMOUNT.LCY> = Y.CHQ.AMOUNT
    END
    R.STMT.ARR<AC.STE.CRF.TYPE> = "CREDIT"
    R.STMT.ARR<AC.STE.TRANSACTION.CODE> = ADJUST.CR.CODE
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
    R.STMT.ARR<AC.STE.VALUE.DATE> = R.NEW(TFS.DR.VALUE.DATE)
    R.STMT.ARR<AC.STE.POSITION.TYPE> = "TR"
    R.STMT.ARR<AC.STE.OUR.REFERENCE> = ID.NEW
    R.STMT.ARR<AC.STE.TRANS.REFERENCE> = ID.NEW
    R.STMT.ARR<AC.STE.SYSTEM.ID> = "AC"
    R.STMT.ARR<AC.STE.BOOKING.DATE> = TODAY
    R.STMT.ARR<AC.STE.EXPOSURE.DATE> = TODAY
    R.STMT.ARR<AC.STE.CURRENCY.MARKET> = 1
    R.STMT.ARR<AC.STE.DEPARTMENT.CODE> = 1
    R.STMT.ARR<AC.STE.PROCESSING.DATE> = TODAY
    R.STMT.ARR<AC.STE.ORIG.CCY.MARKET> = 1
RETURN
END
