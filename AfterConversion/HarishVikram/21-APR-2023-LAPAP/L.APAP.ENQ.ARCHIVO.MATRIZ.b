* @ValidationCode : MjoxMDg0NzUzNDYyOkNwMTI1MjoxNjgyMDc2MDQyNDM1OkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 21 Apr 2023 16:50:42
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
SUBROUTINE L.APAP.ENQ.ARCHIVO.MATRIZ
*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
*
* DATE              WHO                REFERENCE                 DESCRIPTION
* 21-APR-2023     Conversion tool    R22 Auto conversion       BP Removed in insert file
* 21-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------
    $INSERT I_COMMON ;*R22 Auto conversion - START
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON ;*R22 Auto conversion - END

    Y.CATEGORIA = O.DATA

    Y.ARCHIVO.MATRIZ = ""

    Y.ARCHIVO.MATRIZ = "PAGE=CER.FINANCIERO.FINANCIERO.xsl"

    IF Y.CATEGORIA EQ 6612 OR Y.CATEGORIA EQ 6613 THEN
        Y.ARCHIVO.MATRIZ = "PAGE=CER.FINANCIERO.LIBRE.xsl"
    END


    IF Y.CATEGORIA EQ 6614 OR Y.CATEGORIA EQ 6615 THEN
        Y.ARCHIVO.MATRIZ = "PAGE=CER.FINANCIERO.SIN.REDENCION.xsl"
    END

    O.DATA = Y.ARCHIVO.MATRIZ

RETURN

END
