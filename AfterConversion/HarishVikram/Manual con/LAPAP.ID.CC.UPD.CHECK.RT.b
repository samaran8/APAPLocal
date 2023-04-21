SUBROUTINE LAPAP.ID.CC.UPD.CHECK.RT

*-----------------------------------------------------------------------------

*MODIFICATION HISTORY:

*

* DATE              WHO                REFERENCE                 DESCRIPTION

* 21-APR-2023     Conversion tool    R22 Auto conversion       No changes

*-----------------------------------------------------------------------------
    $INSERT I_COMMON ;*R22 Auto conversion - START
    $INSERT I_EQUATE ;*R22 Auto conversion - END


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
