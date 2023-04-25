*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE L.APAP.VAL.CUENTA.CLIENTE.RT
*-----------------------------------------------------------------------------
* Proposito: Identifica si la cuenta corresponde a una cuenta de cliente o interna.
* Parametro de entrada: TT o FT de la transaccion
* Parametro de salida: Indicador S = si una cuenta de cliente. N = si es una cuenta interna.

$INCLUDE T24.BP I_COMMON
$INCLUDE T24.BP I_EQUATE
$INCLUDE T24.BP I_F.FUNDS.TRANSFER
$INCLUDE T24.BP I_F.TELLER

Y.RETURN = 'S'
ERR.TT = ''; ERR.FT = ''; Y.ACCT.1 = ''; Y.ACCT.2 = ''; Y.CONSULTED = 0
Y.NUM.TXN = COMI

IF SUBSTRINGS(Y.NUM.TXN, 1, 2) EQ 'TT' THEN
GOSUB TT.PROCESS
END ELSE
GOSUB FT.PROCESS
END

IF Y.CONSULTED = 1 THEN
IF SUBSTRINGS(Y.ACCT.1, 1, 1) NE '1' AND SUBSTRINGS(Y.ACCT.2, 1, 1) NE '1' THEN
Y.RETURN = 'N'
END
END

COMI = Y.RETURN

RETURN

*---------------
TT.PROCESS:
*---------------
FN.TELLER = 'F.TELLER'
F.TELLER = ''
CALL OPF(FN.TELLER, F.TELLER)
CALL F.READ(FN.TELLER, Y.NUM.TXN, R.TELLER, F.TELLER, ERR.TT)

IF NOT(ERR.TT) THEN
Y.ACCT.1 = R.TELLER<TT.TE.ACCOUNT.1>
Y.ACCT.2 = R.TELLER<TT.TE.ACCOUNT.2>
Y.CONSULTED = 1
END

RETURN

*---------------
FT.PROCESS:
*---------------
FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER'
F.FUNDS.TRANSFER = ''
CALL OPF(FN.FUNDS.TRANSFER, F.FUNDS.TRANSFER)
CALL F.READ(FN.FUNDS.TRANSFER, Y.NUM.TXN, R.FUNDS.TRANSFER, F.FUNDS.TRANSFER, ERR.FT)

IF NOT(ERR.FT) THEN
Y.ACCT.1 = R.FUNDS.TRANSFER<FT.DEBIT.ACCT.NO>
Y.ACCT.2 = R.FUNDS.TRANSFER<FT.CREDIT.ACCT.NO>
Y.CONSULTED = 1
END

RETURN

END
