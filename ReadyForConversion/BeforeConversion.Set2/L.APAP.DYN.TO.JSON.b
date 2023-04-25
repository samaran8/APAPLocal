*-----------------------------------------------------------------------------
* <Rating>-3</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE  L.APAP.DYN.TO.JSON(Y.DYN.RESPONSE.KEY, Y.DYN.RESPONSE.VALUE, Y.DYN.RESPONSE.TYPE, Y.OBJECT.TYPE, Y.JSON.RESPONSE, Y.ERROR)
    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_EQUATE

*Subroutine Convert a Dynamic Array to Json .
*----------------------------------------------------------------------------------------------------------------------------------------------------
*CLEAR OUTPUT VARIAVLE
    Y.JSON.RESPONSE = ''
    Y.ERROR = ''
    Y.ERROR<3> = 'L.APAP.DYN.TO.JSON'
    Y.ITEM.KEY = ''
    Y.ITEM.VALUE = ''
    Y.JSON.ITEM = ''

*DEBUG
    Y.CNT = DCOUNT(Y.DYN.RESPONSE.KEY, @FM)
    FOR V.I = 1 TO Y.CNT STEP 1
        Y.ITEM.KEY = Y.DYN.RESPONSE.KEY<V.I>
        Y.ITEM.VALUE = Y.DYN.RESPONSE.VALUE<V.I>

*JSON ESCAPE CHARATERS
        CHANGE '\' TO '\\' IN Y.ITEM.VALUE
        CHANGE '"' TO '\"' IN Y.ITEM.VALUE
        CHANGE '/' TO '\/' IN Y.ITEM.VALUE
        CHANGE CHAR(9) TO '\t' IN Y.ITEM.VALUE
        CHANGE CHAR(10) TO '\n' IN Y.ITEM.VALUE
        CHANGE CHAR(13) TO '\r' IN Y.ITEM.VALUE

        BEGIN CASE
        CASE Y.ITEM.KEY[1,1] EQ '*'
            Y.JSON.ITEM = '*'
        CASE COUNT(Y.ITEM.VALUE,@SM) GT 0
            CALL L.APAP.SM.TO.JSON.ARRAY(Y.ITEM.VALUE, Y.JSON.ITEM)
            Y.JSON.ITEM = QUOTE(Y.ITEM.KEY) : ':' : Y.JSON.ITEM
        CASE COUNT(Y.ITEM.VALUE,@VM) GT 0
            CALL L.APAP.VM.TO.JSON.ARRAY(Y.ITEM.VALUE, Y.JSON.ITEM)
            Y.JSON.ITEM = QUOTE(Y.ITEM.KEY) : ':' : Y.JSON.ITEM
        CASE 1
            Y.JSON.ITEM = QUOTE(Y.ITEM.KEY) : ':' : QUOTE(Y.ITEM.VALUE)
        END CASE

        IF Y.JSON.ITEM NE '*' THEN
            IF Y.JSON.RESPONSE = '' THEN
                Y.JSON.RESPONSE = Y.JSON.ITEM
            END
            ELSE
                Y.JSON.RESPONSE := ',' : Y.JSON.ITEM
            END
        END
    NEXT V.I

    Y.JSON.RESPONSE = '{' : Y.JSON.RESPONSE : '}'

    RETURN
END
