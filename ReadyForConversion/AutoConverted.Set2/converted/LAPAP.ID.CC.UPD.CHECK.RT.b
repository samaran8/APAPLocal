SUBROUTINE LAPAP.ID.CC.UPD.CHECK.RT
    $INSERT I_COMMON
    $INSERT I_EQUATE


    IF V$FUNCTION EQ 'S' THEN
        RETURN
    END

    FN.APP = 'FBNK.ST.LAPAP.MOD.DIRECCIONES'
    F.APP =''
    CALL OPF(FN.APP,F.APP)

    Y.REC.ID = V$DISPLAY

    CALL F.READ(FN.APP,Y.REC.ID,R.CC,F.APP,ERR.CC)

    IF R.CC NE '' THEN
        E = 'OPERACION NO PERMITIDA'
*TEXT = E
*CALL REM
    END
*DEBUG
RETURN

END
