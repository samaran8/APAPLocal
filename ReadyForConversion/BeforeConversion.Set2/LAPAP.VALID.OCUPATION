*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.VALID.OCUPATION
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.CUSTOMER


    FN.CUSTOMER = "F.CUSTOMER"
    F.CUSTOMER = ""

    OCUPATION = COMI ;*  R.NEW(EB.CUS.LOCAL.REF)<1,CUS.POS>

    CALL LAPAP.REGEXs(OCUPATION,code)

    IF ISDIGIT(OCUPATION) OR code EQ 1 THEN

        ETEXT = "NO INTRODUCIR NUMEROS EN ESTE CAMPO "
        CALL STORE.END.ERROR

    END

    RETURN

END
