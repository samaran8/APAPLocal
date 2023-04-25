*-----------------------------------------------------------------------------
* <Rating>-30</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.AC.LOCKED.EVENTS.IN
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.AC.LOCKED.EVENTS
    $INSERT T24.BP I_F.ACCOUNT
    GOSUB MAIN.PROCESS
    RETURN
*------------
MAIN.PROCESS:
*------------
    Y.ACCOUNT.NUMBER = ""
    Y.CUSTOMER = ""
    Y.CATEGORIA = ''
    Y.ACCOUNT.NUMBER =  COMI
    GOSUB LAPAP.GET.AC.CLIENTE
    GOSUB LAPAP.CUENTA.AC.VALIDA
*-------------------
LAPAP.GET.AC.CLIENTE:
*-------------------
    Y.AA.ARR.ID = Y.ACCOUNT.NUMBER
    FN.AC = "F.ACCOUNT"
    FV.AC = ""
    CALL OPF (FN.AC, FV.AC)
    AC.ERROR = ''
    A.AC = ''
    R.AC = ''
    CALL F.READ(FN.AC,Y.AA.ARR.ID,R.AC,FV.AC, AC.ERROR)
    Y.CUSTOMER = R.AC<AC.CUSTOMER>
    Y.CATEGORIA = R.AC<AC.CATEGORY>
    CALL GET.LOC.REF("AC.LOCKED.EVENTS", "L.AC.CUSTOMER",Y.L.AC.CUSTOMER.POS)
    R.NEW(AC.LCK.LOCAL.REF)<1,Y.L.AC.CUSTOMER.POS> = Y.CUSTOMER
    RETURN
*--------------------
LAPAP.CUENTA.AC.VALIDA:
*--------------------
    IF Y.CATEGORIA GE 3000 AND Y.CATEGORIA LE 3999 THEN
        MESSAGE = "NO SE PUEDEN BLOQUEAR PRESTAMO."
        E = MESSAGE
        ETEXT = E
        CALL ERR
    END
END
