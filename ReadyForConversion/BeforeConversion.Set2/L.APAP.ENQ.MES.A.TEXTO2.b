*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
     SUBROUTINE L.APAP.ENQ.MES.A.TEXTO2
     $INSERT T24.BP I_COMMON
     $INSERT T24.BP I_EQUATE
     $INSERT T24.BP I_ENQUIRY.COMMON

     Y.NUMERO = O.DATA

     BEGIN CASE

     CASE Y.NUMERO = 1
         Y.MES.TEXTO = "Enero"
     CASE Y.NUMERO = 2
         Y.MES.TEXTO = "Febrero"
     CASE Y.NUMERO = 3
         Y.MES.TEXTO = "Marzo"
     CASE Y.NUMERO = 4
         Y.MES.TEXTO = "Abril"
     CASE Y.NUMERO = 5
         Y.MES.TEXTO = "Mayo"
     CASE Y.NUMERO = 6
         Y.MES.TEXTO = "Junio"
     CASE Y.NUMERO = 7
         Y.MES.TEXTO = "Julio"
     CASE Y.NUMERO = 8
         Y.MES.TEXTO = "Agosto"
     CASE Y.NUMERO = 9
         Y.MES.TEXTO = "Septiembre"
     CASE Y.NUMERO = 10
         Y.MES.TEXTO = "Octubre"
     CASE Y.NUMERO = 11
         Y.MES.TEXTO = "Noviembre"
     CASE Y.NUMERO = 12
         Y.MES.TEXTO = "Diciembre"

     END CASE

     O.DATA = Y.MES.TEXTO

     RETURN

 END
