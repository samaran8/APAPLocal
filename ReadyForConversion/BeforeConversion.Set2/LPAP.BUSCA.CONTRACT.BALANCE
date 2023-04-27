*-----------------------------------------------------------------------------
* <Rating>-2</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LPAP.BUSCA.CONTRACT.BALANCE
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.EB.CONTRACT.BALANCES
    $INSERT T24.BP I_ENQUIRY.COMMON

    Y.CB.ID = O.DATA

    FN.CB = "F.EB.CONTRACT.BALANCES"
    FV.CB = ""

    CALL OPF (FN.CB, FV.CB)
    Y.CB.ID = O.DATA
*Y.CB.ID = 1012313204
    R.CB = ""; CB.ERROR = ""
*variable para retornal el balance pendiete
    Y.TOTAL.AMT = 0
    Y.TO.OPEN = 0
    Y.CREDIT.AMT = 0
    Y.DEBIT.AMT = 0
    Y.SUMA.AMT = 0
    CALL F.READ(FN.CB, Y.CB.ID, R.CB, FV.CB, CB.ERROR)

    Y.TYPE.CB =  R.CB<ECB.TYPE.SYSDATE>

    Y.CNT = DCOUNT(Y.TYPE.CB,@VM)
    Y.CB.ACCOUN.TYPE = ""
    FOR I = 1 TO Y.CNT
        Y.CB.ACCOUN.TYPE = R.CB<ECB.TYPE.SYSDATE,I>

        FINDSTR "ACCOUNT" IN Y.CB.ACCOUN.TYPE SETTING Ap, Vp THEN

            Y.TO.OPEN = R.CB<ECB.OPEN.BALANCE,I,1>
            Y.CREDIT.AMT = R.CB<ECB.CREDIT.MVMT,I,1>
            Y.DEBIT.AMT = R.CB<ECB.DEBIT.MVMT,I,1>
            Y.SUMA.AMT = Y.TO.OPEN + Y.CREDIT.AMT + Y.DEBIT.AMT
            Y.TOTAL.AMT = Y.TOTAL.AMT + Y.SUMA.AMT

        END

    NEXT I
    O.DATA =  ABS(Y.TOTAL.AMT)

    RETURN

END
