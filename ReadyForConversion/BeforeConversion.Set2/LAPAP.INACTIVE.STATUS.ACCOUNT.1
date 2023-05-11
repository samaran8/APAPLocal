*-----------------------------------------------------------------------------
* <Rating>-2</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.INACTIVE.STATUS.ACCOUNT.1
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

    CALL F.READ(FN.DAT,Y.TT.TNX, R.DAT, FV.DAT, DAT.ERR)
    Y.OVERRIDE = R.DAT<TT.TE.OVERRIDE>
    Y.ACC.ID   = R.DAT<TT.TE.ACCOUNT.1>
    IF NOT (R.DAT) THEN
        CALL F.READ(FN.TELLENAU,Y.TT.TNX, R.DAT, FV.TELLENAU, DAT.ERR)
        Y.OVERRIDE = R.DAT<TT.TE.OVERRIDE>
        Y.ACC.ID   = R.DAT<TT.TE.ACCOUNT.1>
    END
    FINDSTR "REDO.AC.CHECK.ACTIVE" IN Y.OVERRIDE SETTING F.P, V.P THEN

*CONSULTA DEL HIS
        FN.AC.HIS = 'F.ACCOUNT$HIS'
        F.AC.HIS = ""
        HIST.REC = ""
        YERROR = ""

        CALL OPF(FN.AC.HIS,F.AC.HIS)
        CALL EB.READ.HISTORY.REC(F.AC.HIS, Y.ACC.ID, HIST.REC, YERROR)

        CALL GET.LOC.REF("ACCOUNT", "L.AC.STATUS1", Y.L.AC.STATUS1.POS)
        Y.RESULT = HIST.REC<AC.LOCAL.REF, Y.L.AC.STATUS1.POS>

    END ELSE

        FN.DAT = "F.ACCOUNT"
        FV.DAT = ""
        CALL OPF(FN.DAT, FV.DAT)
        R.DAT = ""
        DAT.ERR = ""

        CALL F.READ(FN.DAT, Y.ACC.ID, R.DAT, FV.DAT, DAT.ERR)

        CALL GET.LOC.REF("ACCOUNT","L.AC.STATUS1", Y.L.AC.STATUS1.POS)

        Y.RESULT = R.DAT<AC.LOCAL.REF, Y.L.AC.STATUS1.POS>

    END

    COMI = Y.RESULT

    RETURN

END
