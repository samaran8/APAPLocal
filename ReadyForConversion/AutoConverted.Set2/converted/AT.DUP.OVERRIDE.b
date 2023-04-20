SUBROUTINE AT.DUP.OVERRIDE

* This Routine is Attached to Version FUNDS.TRANSFER,ATM.DUP
* as a Input Routine
* The Purpose of this routine is to raise a Error

*    $INCLUDE T24.BP I_COMMON        ;*/ TUS START
*    $INCLUDE T24.BP I_EQUATE

    $INSERT I_COMMON
    $INSERT I_EQUATE        ;*/ TUS END

    AF='' ; AV=''
    ETEXT = "DUPLICATE ATM TRANSACTION"
    CALL STORE.END.ERROR
    END.ERROR = 1

RETURN
