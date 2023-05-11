* @ValidationCode : Mjo2OTQ2ODgxOTY6Q3AxMjUyOjE2ODIwNzAzNzUwMTY6SVRTU0JORzotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 21 Apr 2023 15:16:15
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
*21-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION               > TO GT
*21-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*-----------------------------------------------------------------------------------------------------------
SUBROUTINE L.APAP.VAL.AZ.DATE

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AZ.ACCOUNT

    PAY.DATE = COMI

    IF PAY.DATE NE "" THEN

        IF PAY.DATE GT 31 THEN ;* AUTO R22 CODE CONVERSION > TO GT

            ETEXT = "PARA CAMBIO DE DIA DE PAGO, INGRESAR VALORES ENTRE 1 Y 31"
            CALL STORE.END.ERROR

        END

    END ELSE

        ETEXT = "PARA CAMBIO DE DIA DE PAGO, INGRESAR VALORES ENTRE 1 Y 31"

        CALL STORE.END.ERROR

    END

END
