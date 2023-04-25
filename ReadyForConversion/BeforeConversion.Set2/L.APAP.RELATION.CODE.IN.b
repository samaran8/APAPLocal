*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.RELATION.CODE.IN
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.ACCOUNT
    Y.RELACION.ID = R.NEW(AC.RELATION.CODE)
    Y.CONTADOR = 0
    Y.CAN.CS = DCOUNT(Y.RELACION.ID,@VM)
    FOR I = 1 TO Y.CAN.CS
        IF Y.RELACION.ID<1,I> EQ '501' THEN
            Y.CONTADOR = Y.CONTADOR + 1
        END
        IF Y.RELACION.ID<1,I> EQ '500' THEN
            Y.CONTADOR = Y.CONTADOR + 1
        END
    NEXT I
    IF Y.CONTADOR GT 1 THEN
        MESSAGE = "NO PERMITIDO AGREGAR MAS DE UNA RELACION 500 O 501"
        E = MESSAGE
        ETEXT = E
        CALL ERR
    END
END
