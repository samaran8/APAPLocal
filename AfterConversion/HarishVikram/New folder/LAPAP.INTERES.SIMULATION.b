SUBROUTINE LAPAP.INTERES.SIMULATION
*-----------------------------------------------------------------------------

*MODIFICATION HISTORY:

*

* DATE              WHO                REFERENCE                 DESCRIPTION

* 21-APR-2023     Conversion tool    R22 Auto conversion       No changes

*-----------------------------------------------------------------------------
    
    $INSERT I_COMMON ;*R22 Auto conversion - START
    $INSERT I_EQUATE
    $INSERT I_F.AA.INTEREST.ACCRUALS
    $INSERT I_F.AA.INTEREST ;*R22 Auto conversion -END
    AA.ID = COMI
    CALL REDO.B.CON.LNS.BY.DEBTOR.AA.RECS(AA.ID,OUT.RECORD)
    R.AA.INTEREST.APP         = FIELD(OUT.RECORD,"*",7)
    Y.RATE= R.AA.INTEREST.APP<AA.INT.FIXED.RATE,1>
    COMI = Y.RATE
END
