SUBROUTINE LAPAP.QUERY.ACCUITY.RT(P.IDENTITY,P.NAME,P.NATIONALITY,P.GENDER,P.DOB,O.DATA)
*-----------------------------------------------------------------------------

*MODIFICATION HISTORY:

*

* DATE              WHO                REFERENCE                 DESCRIPTION

* 21-APR-2023     Conversion tool    R22 Auto conversion       BP is removed in Insert File

*-----------------------------------------------------------------------------
    $INSERT I_COMMON ;*R22 Auto conversion - START
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON ;*R22 Auto conversion - END

    V.EB.API.ID = 'LAPAP.ACCUITY.ITF'
    Y.PARAMETRO.ENVIO = P.IDENTITY:'::NORMAL::':P.NAME:'::':P.IDENTITY:'::':P.NATIONALITY:'::':P.GENDER:'::':P.DOB

    CALL EB.CALL.JAVA.API(V.EB.API.ID,Y.PARAMETRO.ENVIO,Y.RESPONSE,Y.CALLJ.ERROR)
    IF Y.CALLJ.ERROR NE '' THEN
        P.ERROR =Y.CALLJ.ERROR
        RETURN
    END
    Y.RESULT = Y.RESPONSE

    CHANGE '&' TO @FM IN Y.RESULT

    O.DATA = Y.RESULT

END
