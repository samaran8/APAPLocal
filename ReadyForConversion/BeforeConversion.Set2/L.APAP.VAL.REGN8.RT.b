*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.VAL.REGN8.RT
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT TAM.BP I_F.REDO.H.REPORTS.PARAM


    Y.FIELD.NAME = R.NEW(REDO.REP.PARAM.FIELD.NAME)
    Y.FIELD.VAL  = R.NEW(REDO.REP.PARAM.FIELD.VALUE)
    STR.DATE = ''; END.DATE = ''

    LOCATE 'FROM.DATE' IN Y.FIELD.NAME<1,1> SETTING Y.FRM.POS THEN
        STR.DATE = Y.FIELD.VAL<1,Y.FRM.POS>
    END
    LOCATE 'TO.DATE' IN Y.FIELD.NAME<1,1> SETTING Y.TO.POS THEN
        END.DATE = Y.FIELD.VAL<1,Y.TO.POS>
    END

    IF (STR.DATE EQ '') OR (END.DATE EQ '') THEN
        MESSAGE = "PARAMETROS FROM.DATE Y TO.DATE NO PUEDEN ESTAR EN BLANCO."
        E = MESSAGE
        ETEXT = E
        CALL ERR
    END
    IF END.DATE LT STR.DATE THEN
        MESSAGE = "LA FECHA FINAL NO PUEDE SER  INFERIOR A LA FECHA INICIAL."
        E = MESSAGE
        ETEXT = E
        CALL ERR
    END



END
