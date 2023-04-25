*-----------------------------------------------------------------------------
* <Rating>-41</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE REDO.AA.GET.OVERPAY.DET(LOAN.ID,RECORD.VAL)
*--------------------------------------------------------------------------------------------------
*  M O D I F I C A T I O N S
* ***************************
*--------------------------------------------------------------------------------------------------
* Defect Reference       Modified By                    Date of Change        Change Details
*
*                       Ashokkumar.V.P                  12/11/2015
*--------------------------------------------------------------------------------------------------
    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_EQUATE
    $INCLUDE T24.BP I_F.DATES
    $INCLUDE T24.BP I_F.AA.ARRANGEMENT
    $INCLUDE TAM.BP I_F.REDO.AA.OVERPAYMENT

    GOSUB INIT
    GOSUB PROCESS
    RETURN

INIT:
*****
    RECORD.VAL = ''; OVERPAY.ID = ''; TOT.AMT = 0
    FN.REDO.AA.OVERPAYMENT = 'F.REDO.AA.OVERPAYMENT'; F.REDO.AA.OVERPAYMENT = ''
    CALL OPF(FN.REDO.AA.OVERPAYMENT,F.REDO.AA.OVERPAYMENT)
    FN.REDO.AA.OVERPAY.CONCAT = 'F.REDO.AA.OVERPAY.CONCAT'; F.REDO.AA.OVERPAY.CONCAT = ''
    CALL OPF(FN.REDO.AA.OVERPAY.CONCAT,F.REDO.AA.OVERPAY.CONCAT)
    Y.TODAY = TODAY
    Y.LSTW.DATE = R.DATES(EB.DAT.LAST.WORKING.DAY)
    RETURN

PROCESS:
********
    ERR.REDO.AA.OVERPAY.CONCAT = ''; R.REDO.AA.OVERPAY.CONCAT = ''; Y.REC.CNT = 0
    CALL F.READ(FN.REDO.AA.OVERPAY.CONCAT,LOAN.ID,R.REDO.AA.OVERPAY.CONCAT,F.REDO.AA.OVERPAY.CONCAT,ERR.REDO.AA.OVERPAY.CONCAT)
    IF NOT(R.REDO.AA.OVERPAY.CONCAT) THEN
        RETURN
    END
    YREC.CNT = DCOUNT(R.REDO.AA.OVERPAY.CONCAT,FM)
    IF YREC.CNT EQ 1 THEN
        OVERPAY.ID = R.REDO.AA.OVERPAY.CONCAT
        GOSUB READ.AA.OVERPAY
        TOT.AMT = YOVERPAY.AMT
    END ELSE
        GOSUB GET.OVERPAY.LOOP
    END
    RECORD.VAL = TOT.AMT
    RETURN

READ.AA.OVERPAY:
****************
    ERR.REDO.AA.OVERPAYMENT = ''; R.REDO.AA.OVERPAYMENT = ''; YOVERPAY.AMT = 0; YSTATUS = ''
    CALL F.READ(FN.REDO.AA.OVERPAYMENT,OVERPAY.ID,R.REDO.AA.OVERPAYMENT,F.REDO.AA.OVERPAYMENT,ERR.REDO.AA.OVERPAYMENT)
    YOVERPAY.AMT = R.REDO.AA.OVERPAYMENT<REDO.OVER.AMOUNT>
    YSTATUS  = R.REDO.AA.OVERPAYMENT<REDO.OVER.STATUS>
    RETURN

GET.OVERPAY.LOOP:
****************
    LOOP
        REMOVE OVERPAY.ID FROM R.REDO.AA.OVERPAY.CONCAT SETTING OVER.POSN
    WHILE OVERPAY.ID:OVER.POSN

        GOSUB READ.AA.OVERPAY
        IF YSTATUS NE 'REVERSADO' THEN
            TOT.AMT += YOVERPAY.AMT
        END
    REPEAT
    RETURN
END
