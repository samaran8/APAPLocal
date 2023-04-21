* @ValidationCode : MjotMTE2NDcwOTE1OTpDcDEyNTI6MTY4MjA3MDkzOTMzMDpJVFNTQk5HOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 21 Apr 2023 15:25:39
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
*-----------------------------------------------------------------------------------------------------------------------
SUBROUTINE L.APAP.VAL.CUST.REST.RT
    $INSERT I_COMMON ;* AUTO R22 CODE CONVERSION START
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER ;* AUTO R22 CODE CONVERSION START
    FN.AC = "F.CUSTOMER"
    F.AC = ""

    P.CUSTOMER.ID = COMI
*CALL F.READ(FN.AC, P.ACCOUNT.ID, R.AC, F.AC, '')

*CALL GET.LOC.REF("ACCOUNT", "L.AC.STATUS",AC.POS)
*Y.ACCOUNT.STATUS = R.AC<AC.LOCAL.REF,AC.POS>
    T.CANTIDAD.CARACTERES = LEN(P.CUSTOMER.ID)
    IF (T.CANTIDAD.CARACTERES LE 3) THEN

        TEXT = "CODIGO CLIENTE INVALIDO "
        E = TEXT
    END
RETURN
END
