*---------------------------------------------------------------------
* <Rating>-13</Rating>
*---------------------------------------------------------------------

    SUBROUTINE LAPAP.CLEAR.ECB.BALANCES.POST

*=====================================================================
* Routine is developed for BOAB client. This routine is used to do the below
* Its used to clear all available balances of AA account
* Update AA.SCHEDULED.ACTIVITY and AA.LENDING.NEXT.ACTIVITY
* PRODUCT.LINE - LENDING - Can modify as required for other lines
* Amount will parked in the Internal account enter by bank.
*======================================================================

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_BATCH.FILES
    $INSERT T24.BP I_TSA.COMMON
    $INSERT T24.BP I_F.AA.ARRANGEMENT
    $INSERT T24.BP I_F.AA.BILL.DETAILS
    $INSERT T24.BP I_F.AA.ACCOUNT.DETAILS
    $INSERT T24.BP I_F.EB.CONTRACT.BALANCES
    $INSERT T24.BP I_F.AA.OVERDUE
    $INSERT T24.BP I_F.ACCT.ACTIVITY
    $INSERT T24.BP I_F.AC.BALANCE.TYPE
    $INSERT T24.BP I_F.AA.SCHEDULED.ACTIVITY
    $INSERT T24.BP I_F.AA.ACTIVITY.BALANCES
    $INSERT TAM.BP I_F.REDO.H.REPORTS.PARAM
*$INSERT LAPAP.BP I_LAPAP.CLEAR.ECB.BALANCES.IN.COMMON

    Y.ARREGLO = '';
    FN.ST.LAPAP.INFILEPRESTAMO = "F.LAPAP.CLEAR.ECB.WRITE"
    FV.ST.LAPAP.INFILEPRESTAMO = "";
    CALL OPF (FN.ST.LAPAP.INFILEPRESTAMO,FV.ST.LAPAP.INFILEPRESTAMO)

    SEL.CMD = "SELECT ":FN.ST.LAPAP.INFILEPRESTAMO
    LIST.ACC = ''; NO.OF.ACC = ''; ACC.ERR = ''
    CALL EB.READLIST(SEL.CMD,LIST.ACC,'',NO.OF.ACC,ACC.ERR)

    LOOP
        REMOVE ACC.ID FROM LIST.ACC SETTING POS
    WHILE ACC.ID : POS
        CALL F.READ (FN.ST.LAPAP.INFILEPRESTAMO,ACC.ID,R.ST.LAPAP.INFILEPRESTAMO.1,FV.ST.LAPAP.INFILEPRESTAMO,ERROR.PR1)
        IF R.ST.LAPAP.INFILEPRESTAMO.1 THEN
            Y.ARREGLO<-1> = R.ST.LAPAP.INFILEPRESTAMO.1
        END
    REPEAT
    GOSUB WRITE.DATA

    RETURN
*=====================================================================
WRITE.DATA:

    FN.CHK.DIR = "&SAVEDLISTS&" ; F.CHK.DIR = "" ; Y.FILE.NAME = "AA.ADJ.CLEAR.BAL";
    CALL OPF(FN.CHK.DIR,F.CHK.DIR)
    R.FIL = ''; READ.FIL.ERR = ''
    CALL F.READ(FN.CHK.DIR,Y.FILE.NAME,R.FIL,F.CHK.DIR,READ.FIL.ERR)
    IF R.FIL THEN
        DELETE F.CHK.DIR,Y.FILE.NAME
    END
    WRITE Y.ARREGLO ON F.CHK.DIR, Y.FILE.NAME ON ERROR
        CALL OCOMO("Error en la escritura del archivo en el directorio":F.CHK.DIR)
    END
    CALL JOURNAL.UPDATE('')
*OPEN "&SAVEDLISTS&" TO VV.SAVELISTS ELSE STOP "Unable to open SaveLists File"
*WRITE UPDATE.STRING TO VV.SAVELISTS, "AA.ADJ.CLEAR.BAL"
    RETURN
*=====================================================================
END
