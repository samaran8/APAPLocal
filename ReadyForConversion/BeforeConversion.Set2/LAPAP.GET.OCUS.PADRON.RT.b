*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.GET.OCUS.PADRON.RT(P.CEDULA,P.OUT.ARR,P.ERROR)
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_GTS.COMMON

    V.EB.API.ID = 'LAPAP.PADRON.INTERFACE'
    Y.PARAMETRO.ENVIO = P.CEDULA

    CALL EB.CALL.JAVA.API(V.EB.API.ID,Y.PARAMETRO.ENVIO,Y.RESPONSE,Y.CALLJ.ERROR)
    IF Y.CALLJ.ERROR NE '' THEN
        P.ERROR =Y.CALLJ.ERROR
        RETURN
    END
    Y.F.ARRAY = Y.RESPONSE

    CHANGE '::' TO @VM IN Y.F.ARRAY
    CHANGE '^^' TO @FM IN Y.F.ARRAY

    IF Y.F.ARRAY<1,3> EQ 'success' THEN
        P.OUT.ARR = Y.F.ARRAY<2>
    END ELSE
        Y.CALLJ.ERROR<-1> = Y.F.ARRAY<1,4>
        Y.CALLJ.ERROR<-1> = Y.F.ARRAY<1,5>
    END



END
