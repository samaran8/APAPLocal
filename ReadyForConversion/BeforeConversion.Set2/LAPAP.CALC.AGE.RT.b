*-----------------------------------------------------------------------------
* <Rating>-2</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.CALC.AGE.RT(START.DATE,END.DATE, OUT.AGE)
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.DATES

    REMAIN.CAL.DAYS = "C"

    IF START.DATE < END.DATE THEN
        CALL CDD('',START.DATE, END.DATE, REMAIN.CAL.DAYS)

    END ELSE
        OUT.AGE = 0
        RETURN
    END
    Y.TOTAL.DAYS = 0
    IF REMAIN.CAL.DAYS GE 1 THEN
*CRT "Dias: " : REMAIN.CAL.DAYS
        Y.TOTAL.DAYS = REMAIN.CAL.DAYS
        Y.DIVIDED = Y.TOTAL.DAYS / 365
        Y.DIVIDED = INT(Y.DIVIDED)
    END
    Y.HYPOTETICAL.BIRTHDAY = END.DATE[1,4] : START.DATE[5,4]

    IF Y.HYPOTETICAL.BIRTHDAY > END.DATE THEN

*Si cumple año un dia despues de la asamblea y la edad que cumple es 18, entonces tiene 17 años...
        REMAIN.CAL.DAYS = "C"
        CALL CDD('',END.DATE, Y.HYPOTETICAL.BIRTHDAY, REMAIN.CAL.DAYS)

        IF REMAIN.CAL.DAYS GE 1 AND REMAIN.CAL.DAYS LE 4 THEN
            IF Y.DIVIDED EQ 18 THEN
                Y.DIVIDED = Y.DIVIDED - 1
            END
        END

    END
    IF Y.DIVIDED NE 0 THEN
        OUT.AGE = Y.DIVIDED
    END ELSE

        ENDOUT.AGE = 0
    END
