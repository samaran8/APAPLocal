*-----------------------------------------------------------------------------
* <Rating>-10</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.CHK.CUS.NAME.RT

    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_F.TELLER

    GOSUB MAKE.NO.INPUT

    RETURN

*------------
MAKE.NO.INPUT:
*------------
    LREF.POS   = ''
    LREF.FIELD = 'L.TT.CLIENT.NME'

    CALL MULTI.GET.LOC.REF('TELLER',LREF.FIELD,LREF.POS)
    POS.L.TT.CLIENT.NME = LREF.POS<1,1>

    T.LOCREF<POS.L.TT.CLIENT.NME,7> = 'NOINPUT'

    RETURN

END
