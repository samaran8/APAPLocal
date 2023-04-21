* @ValidationCode : Mjo4OTQyNDMyODpDcDEyNTI6MTY4MjA2OTEyNjg4NzpJVFNTQk5HOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 21 Apr 2023 14:55:26
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.LAPAP
SUBROUTINE L.APAP.USER.SIMULACION
*--------------------------------------------------------------------------------------------------
* Description           : Rutina utilizada para formatear el usuaio INPUTTER
*                         recibe como parametro el usuario y los retorna en formato correcto.
* Developed On          : 16-juan-2019
* Developed By          : Bienvenido Romero
* Development Reference : Para el proceso de monitor req: CI009444
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*21-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*21-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*--------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    Y.USUARIO = COMI
    Y.USUARIO = Y.USUARIO<1,1>
    Y.USUARIO = FIELD(Y.USUARIO, "_",2)
    COMI = Y.USUARIO
RETURN
END
