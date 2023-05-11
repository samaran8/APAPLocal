$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.E.GET.DISP.AMT
*-----------------------------------------------------------------
* Description: This routine is to calculate the outstanding balance of Loan.
*-----------------------------------------------------------------
* Input Arg: O.DATA -> Arrangement ID.
* Out   Arg: O.DATA  -> Outstanding Amount.
*------------------------------------------------------------------------
* Modification History :
*------------------------------------------------------------------------
*  DATE             WHO                   REFERENCE             DESCRIPTION
* 19-APR-2012     R GANESH              PACS00180415 - R.163     Initial Draft.
* 11-APRIL-2023      Harsha                R22 Auto Conversion  - FM to @FM
* 11-APRIL-2023      Harsha                R22 Manual Conversion - No changes 
*------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AA.OVERDUE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.ACCT.ACTIVITY

    GOSUB PROCESS
RETURN
*-----------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------

    IN.ACC.ID     = ''
    Y.PROP.AMT    = ''
    Y.ARR.ACC.ID  = ''
    Y.TOTAL.AMT   = ''
    IN.ARR.ID = O.DATA
    CALL REDO.CONVERT.ACCOUNT(IN.ACC.ID,IN.ARR.ID,Y.ARR.ACC.ID,ERR.TEXT)

    IF Y.ARR.ACC.ID ELSE
        RETURN
    END
    GOSUB GET.ACCOUNT.PROP.BALANCE

    O.DATA =Y.ACC.BAL

RETURN
*-----------------------------------------------------------------
GET.ACCOUNT.PROP.BALANCE:
*-----------------------------------------------------------------
* Here we will get the account property Balances.

    Y.ACC.BAL = 0
    Y.ACCOUNT.PROPERTY = ''
    CALL REDO.GET.PROPERTY.NAME(IN.ARR.ID,'ACCOUNT',R.OUT.AA.RECORD,Y.ACCOUNT.PROPERTY,OUT.ERR)

    ACC.BALANCE.TYPE = 'CURCOMMITMENT'
    Y.PROPERTY.LIST = Y.ACCOUNT.PROPERTY
    Y.BALANCE.TYPE  = ACC.BALANCE.TYPE
    GOSUB GET.BALANCE
    Y.ACC.BAL = Y.BALANCE
RETURN
*-----------------------------------------------------------------
GET.BALANCE:
*-----------------------------------------------------------------

    Y.BALANCE = 0
    Y.PROPERTY.CNT = DCOUNT(Y.PROPERTY.LIST,@FM)
    Y.BALANCE.CNT  = DCOUNT(Y.BALANCE.TYPE,@FM)
    DATE.OPTIONS  = ''
    DATE.OPTIONS<4>  = 'ECB'
    BALANCE.AMOUNT=''

    CALL AA.GET.PERIOD.BALANCES(Y.ARR.ACC.ID,ACC.BALANCE.TYPE,DATE.OPTIONS,TODAY, "", "",BALANCE.AMOUNT, "")
    Y.BALANCE += ABS(BALANCE.AMOUNT<IC.ACT.TURNOVER.CREDIT>)

RETURN
END
