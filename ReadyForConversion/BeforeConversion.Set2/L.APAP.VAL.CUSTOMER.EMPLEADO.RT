*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE L.APAP.VAL.CUSTOMER.EMPLEADO.RT
*-----------------------------------------------------------------------------
* Proposito: Identifica si el cliente consultado es o no empleado.
* Parametro de entrada: CUSTOMER.CODE
* Parametro de salida: Indicador S = si es empleado. N = no es empleado.

$INCLUDE T24.BP I_COMMON
$INCLUDE T24.BP I_EQUATE
$INCLUDE T24.BP I_F.CUSTOMER

Y.RETURN = 'N'
Y.FAX.1 = ''
ERR.CUS = ''

Y.CUST.CODE = COMI

FN.CUSTOMER = 'F.CUSTOMER'
F.CUSTOMER = ''
CALL OPF(FN.CUSTOMER, F.CUSTOMER)

CALL F.READ(FN.CUSTOMER, Y.CUST.CODE, R.CUSTOMER, F.CUSTOMER, ERR.CUS)

IF NOT(ERR.CUS) THEN
Y.FAX.1 = R.CUSTOMER<EB.CUS.FAX.1>

IF Y.FAX.1 THEN
Y.RETURN = 'S'
END
END

COMI = Y.RETURN

RETURN

END
