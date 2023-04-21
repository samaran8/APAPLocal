* @ValidationCode : MjotMTU4MzgyMzg0NjpDcDEyNTI6MTY4MjA3MTg1ODM0OTpJVFNTQk5HOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 21 Apr 2023 15:40:58
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
*---------------------------------------------------------------------------------------------------------------
SUBROUTINE L.APAP.VAL.TIP.CL.JU

    $INSERT I_COMMON ;* AUTO R22 CODE CONVERSIPON START
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CUSTOMER ;* AUTO R22 CODE CONVERSIPON END

    FN.CUS = "F.CUSTOMER"
    F.CUS =  ""
    R.CUS = ""
    CUS.ERR = ""
    CALL OPF(FN.CUS,F.CUS)

    CUSTOMER.ID = COMI

    CALL F.READ(FN.CUS,CUSTOMER.ID,R.CUS,F.CUS,CUS.ERR)

    CALL GET.LOC.REF("CUSTOMER", "L.CU.TIPO.CL",AC.POS.1)
    TIPO.CLIENTE =   R.CUS<EB.CUS.LOCAL.REF,AC.POS.1>

    IF TIPO.CLIENTE NE 'PERSONA JURIDICA' THEN

        MESSAGE = "TIPO DE CLIENTE NO CORRESPONDE CON LA CATEGORIA DE CUENTA"
        E = MESSAGE
        CALL ERR

        RETURN

    END


END
