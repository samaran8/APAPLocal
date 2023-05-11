$PACKAGE APAP.TAM
*---------------------------------------------------------------------------------------
*MODIFICATION HISTORY:
*DATE           WHO                 REFERENCE               DESCRIPTION
*24-APR-2023    CONVERSION TOOL     R22 AUTO CONVERSION     NO CHANGE
*24-APR-2023    VICTORIA S          R22 MANUAL CONVERSION   NO CHANGE
*----------------------------------------------------------------------------------------
SUBROUTINE REDO.UPD.ORD.CUST
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.ACCOUNT

    GOSUB OPEN.FILES
    GOSUB PROCESS
RETURN

*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------

*PACS00273369 - S
    Y.DR.ACCT  = COMI
    Y.CR.ACCT = R.NEW(FT.CREDIT.ACCT.NO)
    IF Y.DR.ACCT AND Y.CR.ACCT THEN
        CALL F.READ(FN.ACCOUNT,Y.DR.ACCT,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
        Y.DR.CUST = R.ACCOUNT<AC.CUSTOMER>
        R.ACCOUNT = ''
        CALL F.READ(FN.ACCOUNT,Y.CR.ACCT,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
        Y.CR.CUST = R.ACCOUNT<AC.CUSTOMER>

        IF NOT(Y.DR.CUST) AND NOT(Y.CR.CUST) THEN
            R.NEW(FT.ORDERING.CUST) = 'APAP'
        END
*PACS00273369 - E
    END
RETURN
*-----------------------------------------------------------------------------
OPEN.FILES:
*-----------------------------------------------------------------------------

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

RETURN
*-----------------------------------------------------------------------------

END
