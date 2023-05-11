*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE REDO.V.ATM.NON.ACCT.DEF

* Description: The validation routine to update the credit account for ATM money transfer
* Dev by : V.P.Ashokkumar
*
******************************************************************************
    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_EQUATE
    $INCLUDE T24.BP I_F.TELLER
    $INCLUDE T24.BP I_F.COMPANY
    $INCLUDE T24.BP I_F.TELLER.TRANSACTION

    IF V$FUNCTION EQ 'I' THEN
        GOSUB INIT
        GOSUB PROCESS
    END
    RETURN

INIT:
*****
    R.TT.TR = ''; ERR.TT.TR = ''; TT.TR.ID = ''; GET.CURRENCY = ''; GET.TELLER.ID = ''
    GET.DIVISION.CODE = ''; GET.CATEGORY = ''; Y.GET.ACCT2 = ''
    FN.TELLER.TRANSACTION = 'F.TELLER.TRANSACTION'
    F.TELLER.TRANSACTION = ''
    CALL OPF(FN.TELLER.TRANSACTION, F.TELLER.TRANSACTION)
    RETURN

PROCESS:
********
    GET.CURRENCY = R.NEW(TT.TE.CURRENCY.1)
    TT.TR.ID = R.NEW(TT.TE.TRANSACTION.CODE)
    CALL F.READ(FN.TELLER.TRANSACTION,TT.TR.ID,R.TT.TR,F.TELLER.TRANSACTION,ERR.TT.TR)
    GET.DIVISION.CODE = R.COMPANY(EB.COM.SUB.DIVISION.CODE)

    Y.GET.ACCT2 = R.NEW(TT.TE.ACCOUNT.2)
    IF NOT(Y.GET.ACCT2) AND AF EQ TT.TE.ACCOUNT.2 THEN
        Y.GET.ACCT2 = COMI
    END
    GET.TELLER.ID = Y.GET.ACCT2[9,4]
    GET.CATEGORY = R.TT.TR<TT.TR.CAT.DEPT.CODE.1>
    R.NEW(TT.TE.ACCOUNT.1) = GET.CURRENCY:GET.CATEGORY:GET.TELLER.ID:GET.DIVISION.CODE
    RETURN

END
