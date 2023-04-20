SUBROUTINE L.APAP.ENQ.MES.A.TEXTO2
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON

    Y.NUMERO = O.DATA

    BEGIN CASE

        CASE Y.NUMERO EQ 1
            Y.MES.TEXTO = "Enero"
        CASE Y.NUMERO EQ 2
            Y.MES.TEXTO = "Febrero"
        CASE Y.NUMERO EQ 3
            Y.MES.TEXTO = "Marzo"
        CASE Y.NUMERO EQ 4
            Y.MES.TEXTO = "Abril"
        CASE Y.NUMERO EQ 5
            Y.MES.TEXTO = "Mayo"
        CASE Y.NUMERO EQ 6
            Y.MES.TEXTO = "Junio"
        CASE Y.NUMERO EQ 7
            Y.MES.TEXTO = "Julio"
        CASE Y.NUMERO EQ 8
            Y.MES.TEXTO = "Agosto"
        CASE Y.NUMERO EQ 9
            Y.MES.TEXTO = "Septiembre"
        CASE Y.NUMERO EQ 10
            Y.MES.TEXTO = "Octubre"
        CASE Y.NUMERO EQ 11
            Y.MES.TEXTO = "Noviembre"
        CASE Y.NUMERO EQ 12
            Y.MES.TEXTO = "Diciembre"

    END CASE

    O.DATA = Y.MES.TEXTO

RETURN

END
