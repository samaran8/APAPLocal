*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.VAL.AZ.FA
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AZ.ACCOUNT

    VAR.ID  = ID.NEW

    FN.AZ.ACCOUNT = 'F.AZ.ACCOUNT'
    F.AZ.ACCOUNT = ''
    CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)

    CALL F.READ(FN.AZ.ACCOUNT,VAR.ID,R.AZ.ACCOUNT,F.AZ.ACCOUNT,YERR)

    CALL GET.LOC.REF("AZ.ACCOUNT","L.AC.STATUS2",ACC.POS)

    VAR.AZ.STATUS = R.AZ.ACCOUNT<AZ.LOCAL.REF,ACC.POS>

    FINDSTR "DECEASED" IN VAR.AZ.STATUS SETTING STATUS.POS,VALUE.POS THEN

        TEXT="L.APAP.AZ.FA"

        CURR.NO=1

        CALL STORE.OVERRIDE(CURR.NO)

    END

    RETURN
