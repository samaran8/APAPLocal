*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.VAL.FREQ.AZ
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AZ.ACCOUNT

    VAR.FREQ.ACT  =  R.NEW(AZ.FREQUENCY)<1,1>

    VAR.FREQ.UNO = R.OLD(AZ.FREQUENCY)<1,1>

    IF  VAR.FREQ.ACT NE VAR.FREQ.UNO AND LEN(VAR.FREQ.ACT) NE 8 THEN

        AF = AZ.FREQUENCY
        ETEXT = "1 PARA CAMBIO DE DIA DE PAGO, INGRESAR DIA EN CAMPO *DIA PAGO INTERESES*"
        CALL STORE.END.ERROR

    END


END
