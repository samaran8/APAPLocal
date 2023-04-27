*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.MON.NARRATIVE

    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_EQUATE
    $INCLUDE T24.BP I_F.TELLER

    FN.TELLER = 'F.TELLER'; F.TELLER = ''
    CALL OPF(FN.TELLER,F.TELLER)

    Y.TELLER.ID = COMI
    CALL F.READ(FN.TELLER, Y.TELLER.ID, R.TELLER, F.TELLER, ERR.TELLER)

    IF NOT(ERR.TELLER) THEN

        Y.TELLER.NARRA =  R.TELLER<TT.TE.NARRATIVE.2>

        Y.TELLER.NARRA = CHANGE(Y.TELLER.NARRA,@VM," ")

        COMI = Y.TELLER.NARRA
    END

    RETURN
END
