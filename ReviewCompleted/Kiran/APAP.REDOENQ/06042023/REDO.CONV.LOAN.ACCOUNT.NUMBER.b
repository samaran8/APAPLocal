$PACKAGE APAP.REDOENQ
SUBROUTINE  REDO.CONV.LOAN.ACCOUNT.NUMBER
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :PRABHU.N
*Program   Name    :REDO.CONV.LOAN.ACCOUNT.NUMBER
*---------------------------------------------------------------------------------

*DESCRIPTION       :This routine is used to get their reference. if not, get the transaction code narrative
*LINKED WITH       :AI.REDO.STMT.LTST.TXN

* ----------------------------------------------------------------------------------
*------------------------------------------------------------------------
* Modification History :
*------------------------------------------------------------------------
*  DATE             WHO                   REFERENCE                  
* 06-APRIL-2023      Conversion Tool       R22 Auto Conversion - Added IF E EQ "EB-UNKNOWN.VARIABLE" THEN
* 06-APRIL-2023      Harsha                R22 Manual Conversion - No changes                             
*------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.TRANSACTION
    $INSERT I_System
    $INSERT I_EB.EXTERNAL.COMMON
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
    Y.VAR.ACCOUNT = System.getVariable('CURRENT.ACCT.NO')
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN    ;*R22 Auto Conversion  - Added IF E EQ "EB-UNKNOWN.VARIABLE" THEN
        Y.VAR.ACCOUNT = ""
    END

    IF NUM(Y.VAR.ACCOUNT) THEN
        IF  O.DATA NE Y.VAR.ACCOUNT THEN
            O.DATA = Y.VAR.ACCOUNT
        END
    END

RETURN
END
