*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------


    SUBROUTINE LAPAP.VAL.IDENT.CEDULA


    $INSERT T24.BP I_COMMON

    $INSERT T24.BP I_EQUATE

    $INSERT T24.BP I_F.DEPT.ACCT.OFFICER

    VAL.IDENTIFICACION   =  COMI

    CALL REDO.S.CALC.CHECK.DIGIT(VAL.IDENTIFICACION)

    IF VAL.IDENTIFICACION = "PASS" THEN

        TEXT = "LA CEDULA ES VALIDA"
        CALL REM

    END ELSE

        TEXT = "LA CEDULA NO ES VALIDA, VERIFIQUE SI ES UN PASAPORTE"
        CALL REM

    END
