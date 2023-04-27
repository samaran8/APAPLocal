*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
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

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.TELLER
    $INSERT T24.BP I_GTS.COMMON

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
