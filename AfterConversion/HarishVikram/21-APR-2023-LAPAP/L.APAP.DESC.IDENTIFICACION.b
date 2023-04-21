* @ValidationCode : MjotMTc3OTczNTkwNDpDcDEyNTI6MTY4MjA3NTM0NTU3NDpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 21 Apr 2023 16:39:05
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.LAPAP
*Retorna una descripcion del tipo de indentificacion del cliente
*-----------------------------------------------------------------------------
SUBROUTINE L.APAP.DESC.IDENTIFICACION
*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
*
* DATE              WHO                REFERENCE                 DESCRIPTION
* 21-APR-2023     Conversion tool    R22 Auto conversion       BP Removed in insert file
* 21-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------
    $INSERT I_COMMON ;*R22 Auto conversion - START
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_ENQUIRY.COMMON ;*R22 Auto conversion - END

    FN.CUS = "F.CUSTOMER"
    FV.CUS = ""
    Y.CUS.ID = O.DATA
    Y.POS = ""
    R.PASAPORTE = ""
    R.CEDULA = ""
    R.RNC = ""
    R.ACTA = ""
    R.NUMERO.UNICO = ""
    R.IDENTIFICACION = ""

    CALL OPF(FN.CUS,FV.CUS)

    CALL F.READ(FN.CUS,Y.CUS.ID,R.CUS,FV.CUS,CUS.ERROR)


*****PASAPORTE*****

    R.PASAPORTE = R.CUS<EB.CUS.LEGAL.ID>

    IF R.PASAPORTE NE "" THEN
        O.DATA = "PASAPORTE"
        RETURN
    END

*****RNC*****

    CALL GET.LOC.REF("CUSTOMER","L.CU.RNC",Y.POS)
    R.RNC = R.CUS<EB.CUS.LOCAL.REF,Y.POS>

    IF R.RNC NE "" THEN
        O.DATA = "RNC"
        RETURN
    END

*****CEDULA*****

    CALL GET.LOC.REF("CUSTOMER","L.CU.CIDENT",Y.POS)
    R.CEDULA = R.CUS<EB.CUS.LOCAL.REF,Y.POS>

    IF R.CEDULA NE "" THEN
        O.DATA = "CEDULA"
        RETURN
    END

*****ACTA NACIMIENTO*****

    CALL GET.LOC.REF("CUSTOMER","L.CU.ACTANAC",Y.POS)
    R.ACTA = R.CUS<EB.CUS.LOCAL.REF,Y.POS>

    IF R.ACTA NE "" THEN
        O.DATA = "ACTA"
        RETURN
    END

*****NUMERO UNICO*****

    CALL GET.LOC.REF("CUSTOMER","L.CU.NOUNICO",Y.POS)
    R.NUMERO.UNICO = R.CUS<EB.CUS.LOCAL.REF,Y.POS>

    IF R.NUMERO.UNICO NE "" THEN
        O.DATA = "NUMEROUNICO"
        RETURN
    END


RETURN
