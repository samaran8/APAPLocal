*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.DATEADD.MONTH (Y.NUM.MONTH,Y.FECHA)
*-----------------------------------------------------------------------------
*
* Developed By            : BY APAP
*
* Developed On            : 19-DIC-2018
* Development Description : Esta rutina recibe una fecha como parametro en formato YYYYMMDD y el mes y retorna la fecha siguiente
*                           sumandole la cantidad de meses enviado en la variable Y.NUM.MONTH
*
*-----------------------------------------------------------------------------------------------------------------
* Include files
*-----------------------------------------------------------------------------------------------------------------
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE

    Y.DATE =  Y.FECHA[1,4]
    Y.DATE1 = Y.FECHA[5,2]
    Y.DATE3 = Y.FECHA[7,2]
    IF Y.DATE1 EQ 12 THEN
        Y.DATE1 = '1'
        Y.DATE = Y.DATE + 1
    END ELSE
        Y.DATE1 = Y.DATE1 + Y.NUM.MONTH
    END
    IF LEN(Y.DATE1) EQ 1 THEN
        Y.DATE1 = "0":Y.DATE1
    END
    Y.FECHA.FINAL = Y.DATE:Y.DATE1:Y.DATE3
    Y.FECHA = Y.FECHA.FINAL
    RETURN

END
