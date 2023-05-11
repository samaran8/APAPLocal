*-----------------------------------------------------------------------------
* <Rating>-31</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE REDO.AA.EXTRA.PAY.CONCAT.UPD
*------------------------------------------------------------------------
* Description: This is routine to update the concat file to link the REDO.AA.OVERPAYMENT file with loan no for regulatory and SAP reports
*-----------------------------------------------------------------------------------------------------------------
* Modification History :
* ----------------------
*   Date          Author              Modification Description
* 12-11-2015     V.P.Ashokkumar         Initial Release
*------------------------------------------------------------------------------------------------------------------
    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_EQUATE
    $INCLUDE T24.BP I_F.DATES
    $INCLUDE TAM.BP I_F.REDO.AA.OVERPAYMENT


    GOSUB INIT
    GOSUB SEL.OVERPAY
    GOSUB PROCESS
    RETURN

INIT:
*****
    FN.REDO.AA.OVERPAYMENT = 'F.REDO.AA.OVERPAYMENT'; F.REDO.AA.OVERPAYMENT = ''
    CALL OPF(FN.REDO.AA.OVERPAYMENT,F.REDO.AA.OVERPAYMENT)
    FN.REDO.AA.OVERPAY.CONCAT = 'F.REDO.AA.OVERPAY.CONCAT'; F.REDO.AA.OVERPAY.CONCAT = ''
    CALL OPF(FN.REDO.AA.OVERPAY.CONCAT,F.REDO.AA.OVERPAY.CONCAT)
    Y.TODAY = TODAY
    Y.LSTW.DATE = R.DATES(EB.DAT.LAST.WORKING.DAY)
    RETURN

SEL.OVERPAY:
************
    CALL EB.CLEAR.FILE(FN.REDO.AA.OVERPAY.CONCAT,F.REDO.AA.OVERPAY.CONCAT)
    SEL.CMD.OVER = '';  SEL.LSTOVER = ''; SEL.CNT = ''; SEL.ERR = ''
    SEL.CMD.OVER = "SELECT ":FN.REDO.AA.OVERPAYMENT:" WITH STATUS EQ 'PENDIENTE' OR (STATUS EQ 'APLICADO' AND NEXT.DUE.DATE GT ":Y.LSTW.DATE:" AND NEXT.DUE.DATE LE ":Y.TODAY:") BY LOAN.NO"
    CALL EB.READLIST(SEL.CMD.OVER,SEL.LSTOVER,'',SEL.CNT,SEL.ERR)
    RETURN

PROCESS:
********
    LOOP
        OVERPAY.ID = ''
        REMOVE OVERPAY.ID FROM SEL.LSTOVER SETTING S.POSN
    WHILE OVERPAY.ID:S.POSN
        ERR.REDO.AA.OVERPAYMENT = ''; R.REDO.AA.OVERPAYMENT = ''
        CALL F.READ(FN.REDO.AA.OVERPAYMENT,OVERPAY.ID,R.REDO.AA.OVERPAYMENT,F.REDO.AA.OVERPAYMENT,ERR.REDO.AA.OVERPAYMENT)
        Y.LOAN.NO = ''; Y.AMT = ''
        Y.LOAN.NO = R.REDO.AA.OVERPAYMENT<REDO.OVER.LOAN.NO>
        Y.AMT = R.REDO.AA.OVERPAYMENT<REDO.OVER.AMOUNT>

        ERR.REDO.AA.OVERPAY.CONCAT = ''; R.REDO.AA.OVERPAY.CONCAT = ''
        CALL F.READ(FN.REDO.AA.OVERPAY.CONCAT,Y.LOAN.NO,R.REDO.AA.OVERPAY.CONCAT,F.REDO.AA.OVERPAY.CONCAT,ERR.REDO.AA.OVERPAY.CONCAT)
        IF R.REDO.AA.OVERPAY.CONCAT THEN
            R.REDO.AA.OVERPAY.CONCAT<-1> = OVERPAY.ID
        END ELSE
            R.REDO.AA.OVERPAY.CONCAT = OVERPAY.ID
        END
        WRITE R.REDO.AA.OVERPAY.CONCAT TO F.REDO.AA.OVERPAY.CONCAT,Y.LOAN.NO ON ERROR NULL
    REPEAT
    RETURN
END
