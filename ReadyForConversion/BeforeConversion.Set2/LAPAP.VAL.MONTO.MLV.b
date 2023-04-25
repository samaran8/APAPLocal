*-----------------------------------------------------------------------------
* <Rating>-30</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.VAL.MONTO.MLV
***********************************************************
* esvalerio - 03/01/22
* VALIDACION ABONO MLV:
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

    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_F.TELLER
    $INSERT T24.BP I_F.FUNDS.TRANSFER
    $INSERT T24.BP I_GTS.COMMON
    $INSERT T24.BP I_F.DATES


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

    IF Y.MONTO.INGRESADO GE Y.MONTO.ADEUDADO THEN
        MESSAGE = "EL MONTO INGRESADO ES MAYOR O IGUAL AL MONTO ADEUDADO"
        E = MESSAGE
        ETEXT = E
        CALL ERR
        RETURN
    END 

    RETURN
END 
