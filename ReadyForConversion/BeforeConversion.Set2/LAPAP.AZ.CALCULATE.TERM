*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.AZ.CALCULATE.TERM

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_ENQUIRY.COMMON

    Y.VALUE.DATE.MATURITY = O.DATA;
    Y.FIELD.POS = CHANGE(Y.VALUE.DATE.MATURITY,',',FM);

    Y.VALUE.DATE    = Y.FIELD.POS<1>
    Y.MATURITY.DATE = Y.FIELD.POS<2>
    Y.PLAZO.FORMAT = '';

    IF Y.VALUE.DATE NE '' AND  Y.MATURITY.DATE NE '' THEN
        DAYS = "C";
        CALL CDD("",Y.VALUE.DATE,Y.MATURITY.DATE,DAYS)
        Y.PLAZO.FORMAT = EREPLACE(DAYS,"D","")
    END

    O.DATA = Y.PLAZO.FORMAT;

    RETURN

END
