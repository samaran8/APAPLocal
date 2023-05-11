$PACKAGE APAP.TAM
SUBROUTINE REDO.OUTWARD.RETURN.RECORD
*---------------------------------------------
* This is the record routine for REDO.OUTWARD.RETURN template for enrichment.
** 13-04-2023 R22 Auto Conversion no changes
** 13-04-2023 Skanda R22 Manual Conversion - No changes
*---------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_F.REDO.H.ROUTING.NUMBER
    $INSERT I_F.REDO.OUTWARD.RETURN

    GOSUB PROCESS
RETURN
*---------------------------------------------
PROCESS:
*---------------------------------------------

    FN.REDO.H.ROUTING.NUMBER = 'F.REDO.H.ROUTING.NUMBER'
    F.REDO.H.ROUTING.NUMBER  = ''
    CALL OPF(FN.REDO.H.ROUTING.NUMBER,F.REDO.H.ROUTING.NUMBER)

    Y.BANK.CODE = R.NEW(CLEAR.RETURN.ROUTE.NO)
    CALL F.READ(FN.REDO.H.ROUTING.NUMBER,Y.BANK.CODE,R.REDO.H.ROUTING.NUMBER,F.REDO.H.ROUTING.NUMBER,ROT.ERR)
    IF R.REDO.H.ROUTING.NUMBER THEN
        OFS$ENRI<CLEAR.RETURN.ROUTE.NO> = R.REDO.H.ROUTING.NUMBER<REDO.ROUT.BANK.NAME>
    END

RETURN
END
