*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.ENQ.MONTH.DIFF
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_ENQUIRY.COMMON

    Y.VALUE.DATE = FIELD(O.DATA,";",1)
    Y.MATURITY.DATE = FIELD(O.DATA,";",2)

   CALL EB.NO.OF.MONTHS(Y.VALUE.DATE, Y.MATURITY.DATE, NO.OF.MONTHS)

    O.DATA = NO.OF.MONTHS

    RETURN

END
