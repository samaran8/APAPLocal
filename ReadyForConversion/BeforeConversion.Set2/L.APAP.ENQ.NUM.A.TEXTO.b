*-----------------------------------------------------------------------------
* <Rating>650</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.ENQ.NUM.A.TEXTO
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_ENQUIRY.COMMON

    GOSUB INICIO

    RETURN


INICIO:
*************************************

    Y.NUMERO.DECENA = ""
    Y.NUMERO.CENTENA = ""
    Y.NUMERO.UND.MIL = ""
    Y.NUMERO.UND.MILLON = ""
    Y.NUMERO.UND.BILLON = ""
    Y.NUMERO.TEXTO = ""
    Y.RESULTADO = ""
    Y.FINAL = ""
    Y.TEXTO = ""
    Y.DECIMAL = ""
    Y.NUMERO = ""

    Y.NUMERO = FIELD(O.DATA,".",1)
    Y.DECIMAL =INT(FIELD(O.DATA,".",2))
    GOSUB PROCESO

    Y.TEXTO = Y.FINAL

    IF Y.DECIMAL > 0 THEN
        Y.NUMERO = Y.DECIMAL[1,2]

        GOSUB PROCESO
        O.DATA = TRIM(Y.TEXTO) : " PUNTO " : TRIM(Y.FINAL)
        RETURN
    END ELSE
        O.DATA = TRIM(Y.FINAL)
        RETURN
    END

*************************************

PROCESO:
    BEGIN CASE

    CASE Y.NUMERO = 0
        Y.NUMERO.TEXTO = ""

    CASE Y.NUMERO = 1

        IF Y.RESULTADO NE "" THEN
            Y.NUMERO.TEXTO = "UN "
        END ELSE
            Y.NUMERO.TEXTO="UNO "
        END

    CASE Y.NUMERO = 2
        Y.NUMERO.TEXTO = "DOS "

    CASE Y.NUMERO = 3
        Y.NUMERO.TEXTO = "TRES "

    CASE Y.NUMERO = 4
        Y.NUMERO.TEXTO = "CUATRO "

    CASE Y.NUMERO = 5
        Y.NUMERO.TEXTO = "CINCO "

    CASE Y.NUMERO = 6
        Y.NUMERO.TEXTO = "SEIS "

    CASE Y.NUMERO = 7
        Y.NUMERO.TEXTO = "SIETE "

    CASE Y.NUMERO = 8
        Y.NUMERO.TEXTO = "OCHO "

    CASE Y.NUMERO = 9
        Y.NUMERO.TEXTO = "NUEVE "

    CASE Y.NUMERO = 10
        Y.NUMERO.TEXTO = "DIEZ "

    CASE Y.NUMERO = 11
        Y.NUMERO.TEXTO = "ONCE "

    CASE Y.NUMERO = 12
        Y.NUMERO.TEXTO = "DOCE "

    CASE Y.NUMERO = 13
        Y.NUMERO.TEXTO = "TRECE "

    CASE Y.NUMERO = 14
        Y.NUMERO.TEXTO = "CATORCE "

    CASE Y.NUMERO = 15
        Y.NUMERO.TEXTO = "QUINCE "

    CASE Y.NUMERO < 20
        Y.NUMERO = Y.NUMERO - 10
        GOSUB PROCESO
        Y.NUMERO.TEXTO = "DIECI" : Y.NUMERO.TEXTO

    CASE Y.NUMERO = 20
        Y.NUMERO.TEXTO = "VEINTE "

    CASE Y.NUMERO < 30
        Y.NUMERO = Y.NUMERO - 20
        GOSUB PROCESO
        Y.NUMERO.TEXTO = "VEINTI" : Y.NUMERO.TEXTO

    CASE Y.NUMERO = 30
        Y.NUMERO.TEXTO = "TREINTA "

    CASE Y.NUMERO = 40
        Y.NUMERO.TEXTO = "CUARENTA "

    CASE Y.NUMERO = 50
        Y.NUMERO.TEXTO = "CINCUENTA "

    CASE Y.NUMERO = 60
        Y.NUMERO.TEXTO = "SESENTA "

    CASE Y.NUMERO = 70
        Y.NUMERO.TEXTO = "SETENTA "

    CASE Y.NUMERO = 80
        Y.NUMERO.TEXTO = "OCHENTA "

    CASE Y.NUMERO = 90
        Y.NUMERO.TEXTO = "NOVENTA "

    CASE Y.NUMERO < 100

        Y.NUMERO.CENTENA = Y.NUMERO
        Y.NUMERO=INT(Y.NUMERO/10)*10
        GOSUB PROCESO

        Y.RESULTADO = Y.RESULTADO : Y.NUMERO.TEXTO : "Y "

        Y.NUMERO=MOD(Y.NUMERO.CENTENA,10)

        IF Y.NUMERO > 0 THEN
            GOSUB PROCESO
        END ELSE
            Y.NUMERO.TEXTO = ""
        END

    CASE Y.NUMERO = 100

        Y.NUMERO.TEXTO = "CIEN "

    CASE Y.NUMERO < 200

        Y.RESULTADO = Y.RESULTADO : "CIENTO "
        Y.NUMERO=Y.NUMERO - 100
        GOSUB PROCESO

    CASE Y.NUMERO = 200 OR Y.NUMERO=300 OR Y.NUMERO=400 OR Y.NUMERO=600 OR Y.NUMERO=800
        Y.NUMERO=INT(Y.NUMERO/100)
        GOSUB PROCESO
        Y.NUMERO.TEXTO = TRIM(Y.NUMERO.TEXTO) : "CIENTOS " :

    CASE Y.NUMERO = 500

        Y.NUMERO.TEXTO = "QUINIENTOS "

    CASE Y.NUMERO = 700

        Y.NUMERO.TEXTO = "SETECIENTOS "

    CASE Y.NUMERO = 900

        Y.NUMERO.TEXTO = "NOVECIENTOS "

    CASE Y.NUMERO < 1000

        Y.NUMERO.UND.MIL = Y.NUMERO
        Y.NUMERO=INT(Y.NUMERO/100)*100
        GOSUB PROCESO

        Y.RESULTADO = Y.RESULTADO : Y.NUMERO.TEXTO
        Y.NUMERO = MOD(Y.NUMERO.UND.MIL,100)

        IF Y.NUMERO > 0 THEN
            GOSUB PROCESO
        END ELSE
            Y.NUMERO.TEXTO = ""
        END

    CASE Y.NUMERO = 1000

        Y.NUMERO.TEXTO="MIL"

    CASE Y.NUMERO < 2000
        Y.RESULTADO = Y.RESULTADO :  "MIL "
        Y.NUMERO= MOD(Y.NUMERO,1000)

        IF Y.NUMERO > 0 THEN
            GOSUB PROCESO
        END ELSE
            Y.NUMERO.TEXTO = ""
        END

    CASE Y.NUMERO < 1000000

        Y.NUMERO.UND.MILLON = Y.NUMERO
        Y.NUMERO = INT(Y.NUMERO/1000)
        GOSUB PROCESO
        Y.RESULTADO = Y.RESULTADO : Y.NUMERO.TEXTO : "MIL "

        Y.NUMERO = MOD(Y.NUMERO.UND.MILLON,1000)

        IF Y.NUMERO > 0 THEN
            GOSUB PROCESO
        END ELSE
            Y.NUMERO.TEXTO = ""
        END

    CASE Y.NUMERO = 1000000
        Y.NUMERO.TEXTO= "UN MILLON"

    CASE Y.NUMERO < 2000000
        Y.NUMERO=MOD(Y.NUMERO,1000000)

        IF Y.NUMERO > 0 THEN
            GOSUB PROCESO
        END ELSE
            Y.NUMERO.TEXTO = ""
        END

        Y.RESULTADO = "UN MILLON " : Y.RESULTADO

    CASE Y.NUMERO < 1000000000000
        Y.NUMERO.UND.BILLON = Y.NUMERO
        Y.NUMERO=INT(Y.NUMERO/1000000)
        GOSUB PROCESO

        Y.RESULTADO = Y.RESULTADO : Y.NUMERO.TEXTO : "MILLONES "
        Y.NUMERO = (Y.NUMERO.UND.BILLON-INT(Y.NUMERO.UND.BILLON/1000000)*1000000)
        GOSUB PROCESO

    END CASE

    Y.FINAL = Y.RESULTADO : Y.NUMERO.TEXTO

    RETURN

END
