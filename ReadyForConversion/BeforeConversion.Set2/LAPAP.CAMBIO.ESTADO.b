*-----------------------------------------------------------------------------
* <Rating>-21</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.CAMBIO.ESTADO

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.AA.ARRANGEMENT

    GOSUB MAIN.PROCESS
    RETURN

MAIN.PROCESS:

    Y.ID = ID.NEW.LAST
    FN.AA.STATUS = 'F.ST.L.APAP.ARR.ESTATUS$NAU'
    FV.AA.STATUS = ''
    CALL OPF(FN.AA.STATUS,FV.AA.STATUS)
    CALL F.READ(FN.AA.STATUS,Y.ID,R.SS,FV.AA.STATUS,AA.SS.ERR)
    Y.STATUS = R.SS<2>
    AA.ID = R.SS<1>
    GOSUB SET.PROCESO
SET.PROCESO:
    FN.AA.ARR='F.AA.ARRANGEMENT'
    F.AA.ARR=''
    CALL OPF(FN.AA.ARR,F.AA.ARR)
    CALL F.READ(FN.AA.ARR,AA.ID,R.AA,F.AA.ARR,AA.ARR.ERR)
    R.AA<AA.ARR.ARR.STATUS>=Y.STATUS
    CALL F.WRITE(FN.AA.ARR,AA.ID,R.AA)
* CALL JOURNAL.UPDATE("")
    AA.ID = ""
    Y.STATUS = ""
    RETURN

END
