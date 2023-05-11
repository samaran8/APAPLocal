$PACKAGE APAP.REDOENQ
SUBROUTINE  REDO.CONV.THEIR.REFERENCE
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :PRABHU.N
*Program   Name    :REDO.CONV.THEIR.REFERENCE
*---------------------------------------------------------------------------------

*DESCRIPTION       :This routine is used to get their reference. if not, get the transaction code narrative
*LINKED WITH       :AI.REDO.STMT.LTST.TXN
*------------------------------------------------------------------------
* Modification History :
*------------------------------------------------------------------------
*  DATE             WHO                   REFERENCE                  
* 10-APRIL-2023      Conversion Tool       R22 Auto Conversion  - F.READ to CACHE.READ
* 10-APRIL-2023      Harsha                R22 Manual Conversion - No changes                             
*------------------------------------------------------------------------
* ----------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.TRANSACTION
    $INSERT I_F.STMT.ENTRY
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.FUNDS.TRANSFER
    GOSUB INIT
    GOSUB EXCHANGE
RETURN
INIT:

    FN.TRANSACTION = 'F.TRANSACTION'
    F.TRANSACTION  = ''
    CALL OPF(FN.TRANSACTION,F.TRANSACTION)

    FN.STMT.ENTRY = 'F.STMT.ENTRY'
    F.STMT.ENTRY  = ''
    CALL OPF(FN.STMT.ENTRY,F.STMT.ENTRY)

    FN.FUNDS.TRANSFER.HIS = 'F.FUNDS.TRANSFER$HIS'
    F.FUNDS.TRANSFER.HIS = ''
    CALL OPF(FN.FUNDS.TRANSFER.HIS,F.FUNDS.TRANSFER.HIS)

    FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER'
    F.FUNDS.TRANSFER = ''
    CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)
RETURN
EXCHANGE:

    CALL F.READ(FN.STMT.ENTRY,O.DATA,R.STMT.ENTRY,F.STMT.ENTRY,ERR.STMT.ENTRY)
    Y.TRANSACTION.CODE = R.STMT.ENTRY<AC.STE.TRANSACTION.CODE>
    Y.REFERENCE.ID  = R.STMT.ENTRY<AC.STE.TRANS.REFERENCE>

*   CALL F.READ(FN.FUNDS.TRANSFER,Y.REFERENCE.ID,R.FUNDS.TRANSFER,F.FUNDS.TRANSFER,FT.ERR)
*   IF NOT(R.FUNDS.TRANSFER) THEN
*       CALL EB.READ.HISTORY.REC(F.FUNDS.TRANSFER.HIS,Y.REFERENCE.ID,R.FUNDS.TRANSFER,FT.HIS.ERR)
*   END
*   Y.PAYMENT.DETAILS = R.FUNDS.TRANSFER<FT.PAYMENT.DETAILS>
*   IF Y.PAYMENT.DETAILS THEN
*       O.DATA = Y.PAYMENT.DETAILS
*   END ELSE

    CALL CACHE.READ(FN.TRANSACTION, Y.TRANSACTION.CODE, R.TRANSACTION, TRANSACTION.ERR)    ;*R22 Auto Conversion  - F.READ to CACHE.READ
    O.DATA = R.TRANSACTION<AC.TRA.NARRATIVE,LNGG>
    IF NOT(O.DATA) THEN
        O.DATA = R.TRANSACTION<AC.TRA.NARRATIVE,1>
    END

RETURN
END
