* @ValidationCode : Mjo0NTI1MTg0MDc6Q3AxMjUyOjE2ODIwNzE3MTUxMDc6SVRTU0JORzotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 21 Apr 2023 15:38:35
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
*-----------------------------------------------------------------------------------------------------------
SUBROUTINE L.APAP.VAL.TIP.CL
    $INSERT I_COMMON ;* AUTO R22 CODE CONVERSION START
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CUSTOMER ;* AUTO R22 CODE CONVERSION END

    FN.ACC = "F.ACCOUNT"
    F.ACC = ""
    R.ACC = ""
    ACC.ERR = ""
    CALL OPF(FN.ACC,F.ACC)

    FN.CUS = "F.CUSTOMER"
    F.CUS =  ""
    R.CUS = ""
    CUS.ERR = ""
    CALL OPF(FN.CUS,F.CUS)

*CUSTOMER.ID = R.NEW(AC.CUSTOMER)
    CUSTOMER.ID = COMI

    CALL F.READ(FN.CUS,CUSTOMER.ID,R.CUS,F.CUS,CUS.ERR)

    CALL GET.LOC.REF("CUSTOMER", "L.CU.TIPO.CL",AC.POS.1)
    TIPO.CLIENTE =   R.CUS<EB.CUS.LOCAL.REF,AC.POS.1>

    IF TIPO.CLIENTE EQ 'PERSONA JURIDICA' THEN

        MESSAGE = "TIPO DE CLIENTE NO CORRESPONDE CON LA CATEGORIA DE CUENTA"
        E = MESSAGE
        CALL ERR

*  RETURN

    END

* RETURN


END
