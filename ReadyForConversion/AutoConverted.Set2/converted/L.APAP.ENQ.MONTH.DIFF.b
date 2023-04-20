SUBROUTINE L.APAP.ENQ.MONTH.DIFF
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON

    Y.VALUE.DATE = FIELD(O.DATA,";",1)
    Y.MATURITY.DATE = FIELD(O.DATA,";",2)

    CALL EB.NO.OF.MONTHS(Y.VALUE.DATE, Y.MATURITY.DATE, NO.OF.MONTHS)

    O.DATA = NO.OF.MONTHS

RETURN

END
