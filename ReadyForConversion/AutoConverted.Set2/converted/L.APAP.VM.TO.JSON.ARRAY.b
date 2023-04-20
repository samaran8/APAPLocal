SUBROUTINE  L.APAP.VM.TO.JSON.ARRAY(VM.IN.FIELDS, JSON.ARRAY.OUT)
    $INSERT I_COMMON
    $INSERT I_EQUATE

*Subroutine Convert a @VM to Json Array.
*----------------------------------------------------------------------------------------------------------------------------------------------------
*DEBUG
    Y.VM.FIELD = DCOUNT(VM.IN.FIELDS,@VM)
    JSON.ARRAY.OUT  = ''

    IF Y.VM.FIELD GT 0 THEN
        FOR Y.I = 1 TO Y.VM.FIELD
            IF Y.I EQ 1 THEN
                JSON.ARRAY.OUT =  QUOTE(VM.IN.FIELDS<1,Y.I>)
            END
            ELSE
                JSON.ARRAY.OUT := ',' : QUOTE(VM.IN.FIELDS<1,Y.I>)
            END
        NEXT Y.I
    END
    JSON.ARRAY.OUT  =  '[' : JSON.ARRAY.OUT : ']'

RETURN
END
