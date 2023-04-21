SUBROUTINE LAPAP.INT.ACCOUN.DES.USD.RT
*-----------------------------------------------------------------------------

*MODIFICATION HISTORY:

*

* DATE              WHO                REFERENCE                 DESCRIPTION

* 21-APR-2023     Conversion tool    R22 Auto conversion       No changes

*-----------------------------------------------------------------------------

    $INSERT I_COMMON ;*R22 Auto conversion - START
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.REDO.FT.TT.TRANSACTION ;*R22 Auto conversion - END

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
