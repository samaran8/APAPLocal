* @ValidationCode : Mjo0NDY3ODEyNTg6Q3AxMjUyOjE2ODIwNzM3NTQ5MjM6SGFyaXNodmlrcmFtQzotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 21 Apr 2023 16:12:34
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
SUBROUTINE L.APAP.CUS.PEP.MANDATORY.RT
*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
*
* DATE              WHO                REFERENCE                 DESCRIPTION
* 21-APR-2023     Conversion tool    R22 Auto conversion       BP Removed in insert file
* 21-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------
    $INSERT I_COMMON;*R22 Auto conversion - START
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER ;*R22 Auto conversion - END


    P.L_CU_PEPS = ""
    P.L_CUS_PEP = ""

    CALL GET.LOC.REF("CUSTOMER","L.CU.PEPS",FT.POS.1)
    P.L_CU_PEPS = R.NEW(LOCAL.REF.FIELD)<1,FT.POS.1>
    CALL GET.LOC.REF("CUSTOMER","L.CUS.PEP",FT.POS.2)
    P.L_CUS_PEP = R.NEW(LOCAL.REF.FIELD)<1,FT.POS.2>

    IF P.L_CU_PEPS EQ "SI" THEN
        IF P.L_CUS_PEP NE "D" AND P.L_CUS_PEP NE "V" THEN
            MESSAGE = 'CAMPO: TIPO PEP, REQUERIDO PARA PERSONAS MARCADAS COMO PEP.'
            E = MESSAGE
*CALL STORE.END.ERROR
            ETEXT = E
            CALL ERR
        END
    END

RETURN

END
