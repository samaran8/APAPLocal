SUBROUTINE REDO.V.VAL.REV.FORM
*-----------------------------------------------------------------------------
* Company Name: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Program Name: REDO.V.VAL.REV.FORM
*-----------------------------------------------------------------------------
* Description:
*       This is a validation routine used to make the local ref field form of
* review and first review rate date to be mandatory if the type of review is periodic
*-----------------------------------------------------------------------------
*Modification History:
*------------------------------------------------------------------------------
*DATE             WHO         REFERENCE             DESCRIPTION
*29-06-2010      PREETHI MD    ODR-2009-10-0326 N.3  INITIAL CREATION
*------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AA.INTEREST

    GOSUB INIT
    GOSUB PROCESS
RETURN

*------------------------------------------------------------------------------
INIT:
*------------------------------------------------------------------------------

    APPL.ARRAY="AA.PRD.DES.INTEREST"
    FLD.ARRAY='L.AA.REV.RT.TY':@VM:'L.AA.REV.FORM'
    FLD.POS=''

RETURN

*-------------------------------------------------------------------------------
PROCESS:
*-------------------------------------------------------------------------------

    CALL MULTI.GET.LOC.REF(APPL.ARRAY,FLD.ARRAY,FLD.POS)

    Y.TYPE.POS=FLD.POS<1,1>
    Y.FORM.POS=FLD.POS<1,2>

    Y.REV.TY=R.NEW(AA.INT.LOCAL.REF)<1,Y.TYPE.POS>
    Y.FORM=R.NEW(AA.INT.LOCAL.REF)<1,Y.FORM.POS>

    IF Y.REV.TY EQ "PERIODICO" THEN
        IF Y.FORM EQ "" THEN
            AF=AA.INT.LOCAL.REF
            AV=Y.FORM.POS
            ETEXT="AA-REVIEW.FORM"
            CALL  STORE.END.ERROR
        END
    END

RETURN

END
