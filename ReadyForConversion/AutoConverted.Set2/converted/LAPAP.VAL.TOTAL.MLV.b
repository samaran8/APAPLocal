SUBROUTINE LAPAP.VAL.TOTAL.MLV
***********************************************************
* esvalerio - 03/01/22
* PAGO DE CANCELACION MLV:
* VERSIONES: TELLER,LAPAP.PAYOFF.CANCEL.MLV.CASHIN,TELLER,LAPAP.PAYOFF.CANCEL.MLV.CHQOBCO,
* TELLER,LAPAP.PAYOFF.CANCEL.MLV.ACCT.TFR
* EB.API,RAD>LAPAP.WS.VPLUS.T24.MLV
* jars/LAPAP/ws-vplus-t24-mlv.jar
***********************************************************
* 500 PESOS! Se ve mucho doc pero luego me lo vas agradecer
***********************************************************
**Errores Y.CAJJJ.ERROR
*1 Fatal error creating thread
*2 Cannot create JVM
*3 Cannot find class
*4 Unicode conversion error
*5 Cannot find method
*6 Cannot find object constructor
*7 Cannot instantiate object
*************************************************************

    $INSERT I_EQUATE
    $INSERT I_COMMON
    $INSERT I_F.TELLER
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_GTS.COMMON
    $INSERT I_F.DATES


    GOSUB LOAD.VARIABLES
    GOSUB INIT
RETURN


LOAD.VARIABLES:
***************

    Y.LOCAL.REF = 'LOCAL.REF';
    Y.LOCAL.FIELDS = '';
    Y.LOCAL.FIELDS.POS = '';

    Y.LOCAL.FIELDS<1,1>   = 'L.TT.MSG.DESC'

    CALL EB.FIND.FIELD.NO(APPLICATION, Y.LOCAL.REF)
    CALL MULTI.GET.LOC.REF(APPLICATION, Y.LOCAL.FIELDS, Y.LOCAL.FIELDS.POS)

    MSG.DESC.POS          = Y.LOCAL.FIELDS.POS<1,1>
RETURN


INIT:
*****
    GOSUB PROCESS
RETURN

PROCESS:

************************
    Y.MONTO.INGRESADO  = R.NEW(TT.TE.AMOUNT.LOCAL.1)
    Y.MSG              = R.NEW(Y.LOCAL.REF)<1,MSG.DESC.POS>
    CHANGE ':'  TO @FM IN Y.MSG
    Y.MONTO.ADEUDADO = Y.MSG<2>

    IF Y.MONTO.ADEUDADO NE Y.MONTO.INGRESADO THEN
        MESSAGE = "EL MONTO INGRESADO ES DIFERENTE AL MONTO ADEUDADO"
        E = MESSAGE
        ETEXT = E
        CALL ERR
        RETURN
    END

RETURN
END
