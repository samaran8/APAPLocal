
*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.VAL.AZ.DATE

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AZ.ACCOUNT

    PAY.DATE = COMI

    IF PAY.DATE NE "" THEN

        IF PAY.DATE > 31 THEN

            ETEXT = "PARA CAMBIO DE DIA DE PAGO, INGRESAR VALORES ENTRE 1 Y 31"
            CALL STORE.END.ERROR

        END

    END ELSE

        ETEXT = "PARA CAMBIO DE DIA DE PAGO, INGRESAR VALORES ENTRE 1 Y 31"

        CALL STORE.END.ERROR

    END

END
