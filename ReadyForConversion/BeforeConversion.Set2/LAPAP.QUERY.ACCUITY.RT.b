*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.QUERY.ACCUITY.RT(P.IDENTITY,P.NAME,P.NATIONALITY,P.GENDER,P.DOB,O.DATA)
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_GTS.COMMON

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
