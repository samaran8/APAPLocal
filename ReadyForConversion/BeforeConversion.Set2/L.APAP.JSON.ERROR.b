*-----------------------------------------------------------------------------
* <Rating>-1</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.JSON.ERROR(Y.ERROR, JSON.OUTPUT)
    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_EQUATE
*Subroutine to return standard error
*----------------------------------------------------------------------------------------------------------------------------------------------------
*DEBUG
    IF Y.ERROR<1> = 1 THEN
        Y.CLEAN = Y.ERROR<2>
        CHANGE '\' TO '\\' IN Y.CLEAN
        CHANGE '"' TO '\"' IN Y.CLEAN
        CHANGE '/' TO '\/' IN Y.CLEAN
        CHANGE CHAR(9) TO '\t' IN Y.CLEAN
        CHANGE CHAR(10) TO '\n' IN Y.CLEAN
        CHANGE CHAR(13) TO '\r' IN Y.CLEAN
        JSON.OUTPUT = '{"error": true, "error_description":"' : Y.CLEAN : '", "source":"' :  Y.ERROR<3> : '"}'
    END
    ELSE
        JSON.OUTPUT = '{"error": false, "error_description":"","source":""}'
    END
    RETURN
END
