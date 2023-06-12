SUBROUTINE LAPAP.INT.ACCOUN.DES.USD.RT

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.REDO.FT.TT.TRANSACTION

    GOSUB INIT
    GOSUB PROCESS
RETURN

*====
INIT:
*====
    Y.VERSION.NAME = APPLICATION:PGM.VERSION
    Y.APPLICATION = APPLICATION
    Y.OFS.BDY     = '' ;
    Y.INTERNAL.ACCT.NO          = "DOP1763500020017"
    Y.TRANSACTION.ID            = ID.NEW
RETURN

*=======
PROCESS:
*=======

    BEGIN CASE
        CASE Y.VERSION.NAME EQ 'REDO.FT.TT.TRANSACTION,L.APAP.TRANS.DOLARES' OR Y.VERSION.NAME EQ 'REDO.FT.TT.TRANSACTION,L.APAP.PART.TRANS.ACH.DOLARES'
            Y.INT.ACCOUNT                       = Y.INTERNAL.ACCT.NO
            R.NEW(FT.TN.CREDIT.ACCT.NO)         = Y.INT.ACCOUNT
        CASE 1
    END CASE

RETURN
END