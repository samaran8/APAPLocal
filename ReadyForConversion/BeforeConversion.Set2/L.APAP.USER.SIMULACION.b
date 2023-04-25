*-------------------------------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.USER.SIMULACION
*--------------------------------------------------------------------------------------------------
* Description           : Rutina utilizada para formatear el usuaio INPUTTER
*                         recibe como parametro el usuario y los retorna en formato correcto.
* Developed On          : 16-juan-2019
* Developed By          : Bienvenido Romero
* Development Reference : Para el proceso de monitor req: CI009444
*--------------------------------------------------------------------------------------------------
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    Y.USUARIO = COMI
    Y.USUARIO = Y.USUARIO<1,1>
    Y.USUARIO = FIELD(Y.USUARIO, "_",2)
    COMI = Y.USUARIO
    RETURN
END
