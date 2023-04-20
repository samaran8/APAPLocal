*-----------------------------------------------------------------------------
* <Rating>-2</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE  L.APAP.TRANSL.OUTPUT.FIELD(Y.DYN.RESPONSE.KEY, Y.DYN.RESPONSE.VALUE, Y.DYN.RESPONSE.TYPE, Y.DYN.MAPPING.OUT, Y.ERROR)
    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_EQUATE

*Subroutine to Translate T24 Field name TO external field name.
*----------------------------------------------------------------------------------------------------------------------------------------------------
*CLEAR OUTPUTS VARIABLE 
Y.DYN.RESPONSE.TYPE = ''
Y.ERROR = ''
Y.ERROR<3> = 'L.APAP.TRANSL.OUTPUT.FIELD'

Y.ITEM = ''
    
    *DEBUG
    Y.CNT = DCOUNT(Y.DYN.RESPONSE.KEY, @FM)
    FOR V.I = 1 TO Y.CNT STEP 1
        Y.ITEM = Y.DYN.RESPONSE.KEY<V.I>

        IF Y.ITEM = 'ID' THEN 
            Y.ITEM = '@ID'
        END

        LOCATE Y.ITEM IN Y.DYN.MAPPING.OUT<6,1> SETTING Y.POS THEN
            Y.DYN.RESPONSE.KEY<V.I> = Y.DYN.MAPPING.OUT<5,Y.POS>
            Y.DYN.RESPONSE.TYPE<V.I> = Y.DYN.MAPPING.OUT<7,Y.POS>

            Y.ITEM.MASK = Y.DYN.MAPPING.OUT<8,Y.POS>
            Y.ITEM.MASK = TRIM(Y.ITEM.MASK, ' ', 'R')
            IF Y.ITEM.MASK NE '' THEN
                Y.ITEM.VALUE = Y.DYN.RESPONSE.VALUE<V.I>
                CALL L.APAP.MASK.DYN.OUT(Y.ITEM.VALUE, Y.ITEM.MASK, Y.ERROR)
                Y.DYN.RESPONSE.VALUE<V.I> = Y.ITEM.VALUE 
            END

        END
        ELSE
            Y.DYN.RESPONSE.KEY<V.I> = '*' : Y.ITEM 
            Y.DYN.RESPONSE.TYPE<V.I> = ''
        END
    NEXT V.I

    RETURN
END
