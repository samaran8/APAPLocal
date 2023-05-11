*-----------------------------------------------------------------------------
* <Rating>-5</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.ENQ.CHQ.SINGLE.MAND.F(ENQ.DATA)

    $INCLUDE T24.BP I_EQUATE
    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_ENQUIRY.COMMON

********************************************************************************************************
**DEBUG
    V.USR.F = "NULL"
    V.AUTH.F = "NULL"
********************************************************************************************************
**RECORRO EL ARREGLO ENQ.DATA  PARA BUSCAR LOS CAMPOS USER(DIGITALIZADOR) Y AUTHORISER
    FOR I = 1 TO 10
**SI LA POSICION 2,N REPRESENTA EL AUTORIZADOR, ENTONCES PASO SU VALOR QUE ES LA POSICION 4,N
        IF ENQ.DATA<2,I> EQ "AUTHORISER" THEN
            V.AUTH.F = ENQ.DATA<4,I>
        END
**SI LA POSICION 2,N REPRESENTA EL USUARIO, ENTONCES PASO SU VALOR QUE ES LA POSICION 4,N
        IF ENQ.DATA<2,I> EQ "USER" THEN
            V.USR.F = ENQ.DATA<4,I>
        END
    NEXT I
**DEBUG
**VALIDACION: RESTRINJO QUE ALMENOS UNO DE LOS DOS CAMPOS SEA RELLENADO.
    IF (V.USR.F EQ "NULL" AND V.AUTH.F EQ "NULL") THEN
**DEBUG
***CALL REM
***ETEXT = "CAMPOS DIGITADOR/AUTORIZADOR SIN VALOR, COMPLETAR AL MENOS UNO."
***CALL STORE.END.ERROR
ENQ.ERROR = "CAMPOS DIGITADOR/AUTORIZADOR SIN VALOR, COMPLETAR AL MENOS UNO."
        ENQ.ERROR<1,2> = 2
       *** ENQ.ERROR<2,1> = "CAMPOS DIGITADOR/AUTORIZADOR SIN VALOR, COMPLETAR AL MENOS UNO."
**DEBUG
    END
    RETURN
END
