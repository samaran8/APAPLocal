*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.VAL.CARD.TYPE.RT
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.CARD.TYPE

    FN.CT = "F.CARD.TYPE"
    F.CT = ""
    R.CT = ""
    CT.ERR = ""
    CALL OPF(FN.CT,F.CT)

    Y.ID = ''
    Y.ID = COMI

    CALL F.READ(FN.CT,Y.ID,R.CT, F.CT, CT.ERR)
    IF R.CT EQ '' THEN
        MESSAGE = "REGISTRO NO EXISTE EN CARD.TYPE."
        E = MESSAGE
        ETEXT = E
        CALL ERR
    END

END
