
*-----------------------------------------------------------------------------
* <Rating>-10</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.ALERT.IMPUESTO(MENSAJE.ALERT)
*---------------------------------------------------------------------------------
* Company Name  : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By  : Raquel P. S.
* Program Name  : L.APAP.ALERT.IMPUESTO
* ODR NUMBER    :
*----------------------------------------------------------------------------------
* Description   : Deal slip routine attached to TT to retrieve ALERT WHEN AMMOUNT EXCEED OR BE EQUAL TO AMMOUNT
* In parameter  : None
* out parameter : None
*----------------------------------------------------------------------------------
* Date             Author             Reference         Description
* 7-9-2018      Raquel P. S.     CN009464    Initial creation
*----------------------------------------------------------------------------------

    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_EQUATE
    $INCLUDE T24.BP I_F.ACCOUNT
    $INCLUDE T24.BP I_F.CUSTOMER
    $INCLUDE T24.BP I_F.TELLER

    GOSUB PROCESS

    RETURN


*----------------------------------------------------------------------------------
PROCESS:
*----------------------------------------------------------------------------------

    Y.MONTO = R.NEW(TT.TE.AMOUNT.LOCAL.1 )

    IF Y.MONTO LE 20000.00 THEN
        MENSAJE.ALERT = "* Acepto cobro comision por retiro en caja, de monto inferior al definido en tarifario vigente."
    END ELSE
        MENSAJE.ALERT = ""
    END

    RETURN

END
