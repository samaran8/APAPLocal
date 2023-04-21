* @ValidationCode : Mjo3ODM5MDczMTU6Q3AxMjUyOjE2ODIwNjk0ODc0OTY6SVRTU0JORzotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 21 Apr 2023 15:01:27
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
*21-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*21-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*----------------------------------------------------------------------------------------------------------------
SUBROUTINE L.APAP.V.CUS.ME.INDUSTRY.RT
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_F.DATES


    Y.L.APAP.INDUSTRY = COMI
    Y.CUST.DATE.OF.BIRTH = R.NEW(EB.CUS.DATE.OF.BIRTH)
    Y.TODAY = R.DATES(EB.DAT.TODAY)
    C.DAYS = "C"

    IF Y.CUST.DATE.OF.BIRTH NE '' THEN
        CALL CDD("",Y.CUST.DATE.OF.BIRTH,Y.TODAY,C.DAYS)

        IF (C.DAYS LT 16 AND Y.L.APAP.INDUSTRY NE '930992') THEN
            MESSAGE = "EDAD CLIENTE NO PERMITE ACTIVIDAD ECONOMICA DIFERENTE A 930992"
            E = MESSAGE
            ETEXT = E
            CALL ERR
        END

    END

RETURN

END
