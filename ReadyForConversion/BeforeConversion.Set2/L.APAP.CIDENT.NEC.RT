*-----------------------------------------------------------------------------
* <Rating>-1</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.CIDENT.NEC.RT
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_ENQUIRY.COMMON
    $INSERT T24.BP I_F.CUSTOMER

**---------------------------------------------------------------------------------------------
**VARIABLES
    Y.CU.ID = ""
    FN.CUS = "F.CUSTOMER"
    FV.CUS = ""

**---------------------------------------------------------------------------------------------
**ASIGNACION
    Y.CU.ID = O.DATA
**DEBUG
**---------------------------------------------------------------------------------------------
**PRIMERO LEO DESDE LA TABLA CUSTOMER
    CALL F.READ(FN.CUS,Y.CU.ID,R.CUS, FV.CUS, CUS.ERR)

    CALL GET.LOC.REF("CUSTOMER", "L.CU.CIDENT",CUS.POS)
    Y.CUS.CIDENT = R.CUS<EB.CUS.LOCAL.REF,CUS.POS>

    Y.IDENTIFICACION = "NeC"
**DEBUG
    IF Y.CUS.CIDENT NE "" THEN
        Y.IDENTIFICACION = Y.CUS.CIDENT
    END
**DEBUG
    O.DATA = Y.IDENTIFICACION

    RETURN

END
