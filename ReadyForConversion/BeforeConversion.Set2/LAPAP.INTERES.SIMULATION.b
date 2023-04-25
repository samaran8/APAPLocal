*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.INTERES.SIMULATION
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.AA.INTEREST.ACCRUALS
    $INSERT T24.BP I_F.AA.INTEREST
    AA.ID = COMI
    CALL REDO.B.CON.LNS.BY.DEBTOR.AA.RECS(AA.ID,OUT.RECORD)
    R.AA.INTEREST.APP         = FIELD(OUT.RECORD,"*",7)
    Y.RATE= R.AA.INTEREST.APP<AA.INT.FIXED.RATE,1>
    COMI = Y.RATE
END
