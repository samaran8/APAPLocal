*-----------------------------------------------------------------------------
* <Rating>-1</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.ACTUAL.STATUS.ACC
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_ENQUIRY.COMMON
    $INSERT T24.BP I_F.ACCOUNT
    $INSERT T24.BP I_F.TELLER

*-----------------------------------------------------------------------------------
*-----------CONSULTAMOS EL PENULTIMO REGISTRO DEL HIS DE LA TABLA ACCOUNT-----------
*-----------------------------------------------------------------------------------

*OBTENEMOS EL NUMERO DE TT
    Y.TT.TNX = COMI
    Y.RESULT = ""

*CONSULTA DEL LIVE
    FN.DAT = "F.TELLER"
    FV.DAT = ""
    CALL OPF(FN.DAT, FV.DAT)
    R.DAT = ""
    DAT.ERR = ""

    FN.TELLENAU = "F.TELLER$NAU"
    FV.TELLENAU = ""
    CALL OPF (FN.TELLENAU,FV.TELLENAU)

    FN.ACCOUNT = "F.ACCOUNT"
    FV.ACCOUNT = ""
    CALL OPF(FN.ACCOUNT, FV.ACCOUNT)

    CALL F.READ(FN.DAT,Y.TT.TNX, R.DAT, FV.DAT, DAT.ERR)
    Y.OVERRIDE = R.DAT<TT.TE.OVERRIDE>
    Y.ACC.ID   = R.DAT<TT.TE.ACCOUNT.1>

    IF NOT (R.DAT) THEN
        CALL F.READ(FN.TELLENAU,Y.TT.TNX, R.DAT, FV.TELLENAU, DAT.ERR)
        Y.OVERRIDE = R.DAT<TT.TE.OVERRIDE>
        Y.ACC.ID   = R.DAT<TT.TE.ACCOUNT.1>
    END

    R.DAT = ""
    DAT.ERR = ""
    CALL F.READ(FN.ACCOUNT, Y.ACC.ID, R.ACCOUNT, FV.ACCOUNT, DAT.ERR)
    CALL GET.LOC.REF("ACCOUNT","L.AC.STATUS1", Y.L.AC.STATUS1.POS)
    COMI = R.ACCOUNT<AC.LOCAL.REF, Y.L.AC.STATUS1.POS>

    RETURN

END
