*-----------------------------------------------------------------------------
* <Rating>620</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.ENQ.NUM.DC.A.TXT
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
    Y.DECIMAL =FIELD(O.DATA,".",2)
    GOSUB PROCESO

    Y.TEXTO = Y.FINAL
    IF Y.DECIMAL > 0 THEN
        Y.NUMERO = Y.DECIMAL[1,2]

        GOSUB PROCESO
        O.DATA = CHANGE(TRIM(Y.TEXTO),"CERO", " ") : " PUNTO " : TRIM(Y.FINAL)
        O.DATA = CHANGE(O.DATA, "PUNTO UNO", "PUNTO DIEZ")
        O.DATA = CHANGE(O.DATA, "PUNTO DOS", "PUNTO VEINTE")
        O.DATA = CHANGE(O.DATA, "PUNTO TRES", "PUNTO TREINTA")
        O.DATA = CHANGE(O.DATA, "PUNTO CUATRO", "PUNTO CUARENTA")
        O.DATA = CHANGE(O.DATA, "PUNTO CINCO", "PUNTO CINCUENTA")
        O.DATA = CHANGE(O.DATA, "PUNTO SEIS", "PUNTO SESENTA")
        O.DATA = CHANGE(O.DATA, "PUNTO SIETE", "PUNTO SETENTA")
        O.DATA = CHANGE(O.DATA, "PUNTO OCHO", "PUNTO OCHENTA")
        O.DATA = CHANGE(O.DATA, "PUNTO NUEVE", "PUNTO NOVENTA")
        
        RETURN
    END ELSE
        O.DATA = CHANGE(TRIM(Y.FINAL),"CERO"," ")
        RETURN
    END

*************************************
PROCESO:
    BEGIN CASE

    CASE Y.NUMERO EQ "0"
        Y.NUMERO.TEXTO = "CERO"
    CASE Y.NUMERO EQ "01"
        IF Y.NUMERO[1,1] EQ "0" THEN
         Y.NUMERO.TEXTO = "CERO UNO "
        END ELSE
         Y.NUMERO.TEXTO="UNO "
        END
    CASE Y.NUMERO EQ "02"
        IF Y.NUMERO[1,1] EQ "0" THEN
         Y.NUMERO.TEXTO = "CERO DOS "
        END ELSE
         Y.NUMERO.TEXTO="DOS "
        END
    CASE Y.NUMERO EQ "03"
        IF Y.NUMERO[1,1] EQ "0" THEN
         Y.NUMERO.TEXTO = "CERO TRES "
        END ELSE
         Y.NUMERO.TEXTO="TRES "
        END
    CASE Y.NUMERO EQ "04"
        IF Y.NUMERO[1,1] EQ "0" THEN
         Y.NUMERO.TEXTO = "CERO CUATRO "
        END ELSE
         Y.NUMERO.TEXTO="CUATRO "
        END
    CASE Y.NUMERO EQ "05"
        IF Y.NUMERO[1,1] EQ "0" THEN
         Y.NUMERO.TEXTO = "CERO CINCO "
        END ELSE
         Y.NUMERO.TEXTO="CINCO "
        END 
    CASE Y.NUMERO EQ "06"
        IF Y.NUMERO[1,1] EQ "0" THEN
         Y.NUMERO.TEXTO = "CERO SEIS "
        END ELSE
         Y.NUMERO.TEXTO="SEIS "
        END
    CASE Y.NUMERO EQ "07"
        IF Y.NUMERO[1,1] EQ "0" THEN
         Y.NUMERO.TEXTO = "CERO SIETE "
        END ELSE
         Y.NUMERO.TEXTO="SIETE "
        END
    CASE Y.NUMERO EQ "08"
        IF Y.NUMERO[1,1] EQ "0" THEN
         Y.NUMERO.TEXTO = "CERO OCHO "
        END ELSE
         Y.NUMERO.TEXTO="OCHO "
        END  
    CASE Y.NUMERO EQ "09"
        IF Y.NUMERO[1,1] EQ "0" THEN
         Y.NUMERO.TEXTO = "CERO NUEVE "
        END ELSE
         Y.NUMERO.TEXTO="NUEVE "
        END                     

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
    END CASE

    Y.FINAL = Y.RESULTADO : Y.NUMERO.TEXTO

    RETURN

END
