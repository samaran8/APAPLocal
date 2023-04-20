*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.VAL.FR.CL.CLM.AMT.RT
    $INSERT  I_COMMON
    $INSERT  I_EQUATE
    $INSERT  I_F.REDO.FRONT.CLAIMS

    Y.AMOUNT = COMI

    IF (Y.AMOUNT GT 99999999.99) THEN
        TEXT = "VALOR MAXIMO PERMITIDO 99,999,999.99"
        CALL REM
        ETEXT = TEXT
        CALL STORE.END.ERROR
    END
    RETURN
END
