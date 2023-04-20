* Item ID        : GDC-274
*-------------------------------------------------------------------------------------
* Description :
* ------------
* This program raise an error message whether the processed check belong to APAP.
* Este programa lanza error si el cheque procesado pertenece a APAP.
*-------------------------------------------------------------------------------------
* Modification History :
* ----------------------
* Date           Author            Modification Description
* -------------  -----------       ---------------------------
* 2019/03/19     Raquel P.S.         Initial development
*-------------------------------------------------------------------------------------
* Content summary :
* -----------------
* Versions : TELLER,L.APAP.COMPRA.CHQOBCO.ML ATTACHED AS VALIDATION ROUTINE
* EB.API record      : LAPAP.VERIFY.CK.APAP
* Routines       : LAPAP.VERIFY.CK.APAP
*-------------------------------------------------------------------------------------


SUBROUTINE LAPAP.VERIFY.CK.APAP

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.TELLER
    $INSERT I_GTS.COMMON

    FN.TT = "F.TELLER"
    F.TT = ""

    CALL OPF(FN.TT,F.TT)
    Y.L.TT.CONCEPT=COMI

    IF Y.L.TT.CONCEPT EQ '661'
    THEN
        ETEXT='CHEQUE DE APAP, PROCESAR POR LA OPCION CORRECTA'
        CALL STORE.END.ERROR

    END

END
