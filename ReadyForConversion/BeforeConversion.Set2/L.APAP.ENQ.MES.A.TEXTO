*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.ENQ.MES.A.TEXTO
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_ENQUIRY.COMMON


    Y.NUMERO = O.DATA

    BEGIN CASE

    CASE Y.NUMERO = 1
        Y.MES.TEXTO = "ENERO"
    CASE Y.NUMERO = 2
        Y.MES.TEXTO = "FEBRERO"
    CASE Y.NUMERO = 3
        Y.MES.TEXTO = "MARZO"
    CASE Y.NUMERO = 4
        Y.MES.TEXTO = "ABRIL"
    CASE Y.NUMERO = 5
        Y.MES.TEXTO = "MAYO"
    CASE Y.NUMERO = 6
        Y.MES.TEXTO = "JUNIO"
    CASE Y.NUMERO = 7
        Y.MES.TEXTO = "JULIO"
    CASE Y.NUMERO = 8
        Y.MES.TEXTO = "AGOSTO"
    CASE Y.NUMERO = 9
        Y.MES.TEXTO = "SEPTIEMBRE"
    CASE Y.NUMERO = 10
        Y.MES.TEXTO = "OCTUBRE"
    CASE Y.NUMERO = 11
        Y.MES.TEXTO = "NOVIEMBRE"
    CASE Y.NUMERO = 12
        Y.MES.TEXTO = "DICIEMBRE"

    END CASE

    O.DATA = Y.MES.TEXTO

    RETURN

END
