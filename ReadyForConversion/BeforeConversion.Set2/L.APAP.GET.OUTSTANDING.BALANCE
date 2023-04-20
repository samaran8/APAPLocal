*-----------------------------------------------------------------------------
* <Rating>-21</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.GET.OUTSTANDING.BALANCE (ACCOUNT, OUTSTANDING.BALANCE)
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_ENQUIRY.COMMON
    $INSERT T24.BP I_F.EB.CONTRACT.BALANCES

    Y.TOTAL.CAPITAL = 0

    IF ACCOUNT NE '' THEN
        GOSUB INIT
        GOSUB GET.BALANCE.EN.EB
    END

    RETURN

*----------
INIT:
*----------
    FN.CB = "F.EB.CONTRACT.BALANCES"
    FV.CB = ""
    CB.ERROR = ''
    R.CB = ''
    CALL OPF (FN.CB, FV.CB)

    RETURN

*-----------------------
GET.BALANCE.EN.EB:
*----------------------
    CALL F.READ(FN.CB, ACCOUNT, R.CB, FV.CB, CB.ERROR)
    Y.TYPE.CB =  R.CB<ECB.TYPE.SYSDATE>

    Y.CNT = DCOUNT(Y.TYPE.CB,@VM)
    Y.CB.ACCOUN.TYPE = ""

    FOR I = 1 TO Y.CNT

        Y.CB.ACCOUN.TYPE = R.CB<ECB.TYPE.SYSDATE,I>

        FINDSTR "ACCOUNT" IN Y.CB.ACCOUN.TYPE SETTING Ap, Vp THEN

            FINDSTR "UNCACCOUNT" IN Y.CB.ACCOUN.TYPE SETTING Ap, Vp THEN
*-- LOS SALDOS UNCACCOUNT NO APLICAN
            END ELSE
                Y.TO.OPEN = R.CB<ECB.OPEN.BALANCE,I,1>
                Y.CREDIT.AMT = R.CB<ECB.CREDIT.MVMT,I,1>
                Y.DEBIT.AMT = R.CB<ECB.DEBIT.MVMT,I,1>
                Y.SUMA.AMT = Y.TO.OPEN + Y.CREDIT.AMT + Y.DEBIT.AMT
                Y.TOTAL.AMT = Y.TOTAL.AMT + Y.SUMA.AMT
                Y.TOTAL.CAPITAL = Y.TOTAL.AMT
            END

        END

    NEXT I

    OUTSTANDING.BALANCE = Y.TOTAL.CAPITAL

    RETURN

END
