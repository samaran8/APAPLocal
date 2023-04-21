* @ValidationCode : MjoxMzkyNjM1MDQ0OkNwMTI1MjoxNjgyMDcyMzAyOTQ1OklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 21 Apr 2023 15:48:22
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.LAPAP
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*21-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                BP REMOVED
*21-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*--------------------------------------------------------------------------------------------------------------
SUBROUTINE LAPAP.AC.LOCKED.EVENTS.IN
    $INSERT I_COMMON ;* AUTO R22 CODE CONVERSION START
    $INSERT I_EQUATE
    $INSERT I_F.AC.LOCKED.EVENTS
    $INSERT I_F.ACCOUNT ;* AUTO R22 CODE CONVERSION END
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
