* @ValidationCode : MjotMjQzNjQ1MDQyOkNwMTI1MjoxNjgyMDcxMDIwNTg5OklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 21 Apr 2023 15:27:00
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
*21-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 BP REMOVED
*21-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*-----------------------------------------------------------------------------------------------------------
SUBROUTINE L.APAP.VAL.CUSTOMER.EMPLEADO.RT
*-----------------------------------------------------------------------------
* Proposito: Identifica si el cliente consultado es o no empleado.
* Parametro de entrada: CUSTOMER.CODE
* Parametro de salida: Indicador S = si es empleado. N = no es empleado.

    $INSERT I_COMMON ;*AUTO R22 CODE CONVERSION START
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER ;*AUTO R22 CODE CONVERSION START

    Y.RETURN = 'N'
    Y.FAX.1 = ''
    ERR.CUS = ''

    Y.CUST.CODE = COMI

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER, F.CUSTOMER)

    CALL F.READ(FN.CUSTOMER, Y.CUST.CODE, R.CUSTOMER, F.CUSTOMER, ERR.CUS)

    IF NOT(ERR.CUS) THEN
        Y.FAX.1 = R.CUSTOMER<EB.CUS.FAX.1>

        IF Y.FAX.1 THEN
            Y.RETURN = 'S'
        END
    END

    COMI = Y.RETURN

RETURN

END
