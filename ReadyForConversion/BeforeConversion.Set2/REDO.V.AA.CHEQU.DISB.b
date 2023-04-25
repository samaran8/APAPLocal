*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE REDO.V.AA.CHEQU.DISB
*
* Description: Auth routine to validate the cheque duplication for Loans
* Attached to: FUNDS.TRANSFER,CHQ.OTHERS.LOAN.DUM
* Dev by     : V.P.Ashokkumar
*

    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_EQUATE
    $INCLUDE T24.BP I_System
    $INCLUDE TAM.BP I_F.REDO.MTS.DISBURSE

    GOSUB INIT
    GOSUB PROCESS
    RETURN

INIT:
******
    FT.ID = ''; ERR.REDO.MTS.DISBURSE = ''; R.REDO.MTS.DISBURSE = ''; YREF.ID = ''
    FN.REDO.MTS.DISBURSE = 'F.REDO.MTS.DISBURSE'; F.REDO.MTS.DISBURSE = ''
    CALL OPF(FN.REDO.MTS.DISBURSE,F.REDO.MTS.DISBURSE)
    RETURN

PROCESS:
********
    FT.ID = System.getVariable('CURRENT.FT')
    IF NOT(FT.ID) THEN
        RETURN
    END

    CALL F.READ(FN.REDO.MTS.DISBURSE,FT.ID,R.REDO.MTS.DISBURSE,F.REDO.MTS.DISBURSE,ERR.REDO.MTS.DISBURSE)
    YREF.ID = R.REDO.MTS.DISBURSE<MT.REF.ID>
    IF YREF.ID THEN
        ETEXT = "FT-DUP"
        CALL STORE.END.ERROR
    END

    RETURN
END
