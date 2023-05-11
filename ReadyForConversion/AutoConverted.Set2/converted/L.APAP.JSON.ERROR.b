SUBROUTINE L.APAP.JSON.ERROR(Y.ERROR, JSON.OUTPUT)
    $INSERT I_COMMON
    $INSERT I_EQUATE
*Subroutine to return standard error
*----------------------------------------------------------------------------------------------------------------------------------------------------
*DEBUG
    IF Y.ERROR<1> EQ 1 THEN
        Y.CLEAN = Y.ERROR<2>
        CHANGE '\' TO '\\' IN Y.CLEAN
        CHANGE '"' TO '\"' IN Y.CLEAN
        CHANGE '/' TO '\/' IN Y.CLEAN
        CHANGE CHARX(9) TO '\t' IN Y.CLEAN
        CHANGE CHARX(10) TO '\n' IN Y.CLEAN
        CHANGE CHARX(13) TO '\r' IN Y.CLEAN
        JSON.OUTPUT = '{"error": true, "error_description":"' : Y.CLEAN : '", "source":"' :  Y.ERROR<3> : '"}'
    END
    ELSE
        JSON.OUTPUT = '{"error": false, "error_description":"","source":""}'
    END
RETURN
END
