*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.SEGMENTO.VALIDATE

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_GTS.COMMON
    $INSERT T24.BP I_System
    $INSERT JBC.h
    $INSERT T24.BP I_F.EB.LOOKUP
    $INSERT T24.BP I_F.VERSION

    GOSUB LOAD.APLICATIONS
    GOSUB VALIDATE.SEGMENTO

LOAD.APLICATIONS:

    FN.EB.LOOKUP = 'F.EB.LOOKUP'; F.EB.LOOKUP = ''
    CALL OPF (FN.EB.LOOKUP,F.EB.LOOKUP)

    Y.SEGMENTO                = ID.NEW
    RETURN

VALIDATE.SEGMENTO:

    Y.SEGMENTO.ID             = "SEGMENTO*":Y.SEGMENTO
    R.LOOKUP =''; LOOKUP.ERR ='';
    CALL F.READ(FN.EB.LOOKUP,Y.SEGMENTO.ID,R.LOOKUP,F.EB.LOOKUP,LOOKUP.ERR)
    IF NOT (R.LOOKUP) THEN

        MESSAGE = "EL SEGMENTO ":Y.SEGMENTO:" NO ES VALIDO"
        E = MESSAGE
        ETEXT = E
        CALL ERR
        RETURN

    END
    RETURN
END




