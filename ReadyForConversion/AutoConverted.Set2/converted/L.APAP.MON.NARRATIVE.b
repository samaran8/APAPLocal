SUBROUTINE L.APAP.MON.NARRATIVE

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.TELLER

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