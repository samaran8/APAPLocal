* @ValidationCode : MjotNzQxODU4MTU4OkNwMTI1MjoxNjgyMDc2MDkwNDEwOkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 21 Apr 2023 16:51:30
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
SUBROUTINE L.APAP.ENQ.B186.NOTA5.RT
*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
*
* DATE              WHO                REFERENCE                 DESCRIPTION
* 21-APR-2023     Conversion tool    R22 Auto conversion       = to EQ
* 21-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON

    Y.CLAIM.TYPE = O.DATA

    Y.SELECTOR.NOTA.5 = 0

*1. Lista de tipos de reclamaciones de tarjeta de crédito donde intervenga una marca internacional.
    BEGIN CASE

        CASE Y.CLAIM.TYPE EQ "COMPRA.DUPLICADA.T.DEBITO"
            Y.SELECTOR.NOTA.5 = 1
        CASE Y.CLAIM.TYPE EQ "COMPRA.DUPLICADA.T.CREDITO"
            Y.SELECTOR.NOTA.5 = 1
        CASE Y.CLAIM.TYPE EQ "COMPRA.DUPLICADA.T.FLOTILLA"
            Y.SELECTOR.NOTA.5 = 1
        CASE Y.CLAIM.TYPE EQ "COMPRA.NO.RECONOCIDA.TARJ.DEBITO"
            Y.SELECTOR.NOTA.5 = 1
        CASE Y.CLAIM.TYPE EQ "COMPRA.NO.RECONOCIDA.FLOTILLA"
            Y.SELECTOR.NOTA.5 = 1
        CASE Y.CLAIM.TYPE EQ "COMPRA.NO.RECONOCIDA.T.CREDITO"
            Y.SELECTOR.NOTA.5 = 1
        CASE Y.CLAIM.TYPE EQ "FRAUDE.TARJETA.DE.DEBITO"
            Y.SELECTOR.NOTA.5 = 1
        CASE Y.CLAIM.TYPE EQ "FRAUDE.TARJETA.DE.CREDITO"
            Y.SELECTOR.NOTA.5 = 1
        CASE Y.CLAIM.TYPE EQ "FRAUDE.TARJETA.FLOTILLA"
            Y.SELECTOR.NOTA.5 = 1
        CASE Y.CLAIM.TYPE EQ "PL-COMPRA.DUPLICADA.T.CREDITO"
            Y.SELECTOR.NOTA.5 = 1
*2. Lista de tipos de reclamaciones de buro incorrecta.
        CASE Y.CLAIM.TYPE EQ "PL-COMPRA.NO.RECONOCIDA"
            Y.SELECTOR.NOTA.5 = 2
        CASE Y.CLAIM.TYPE EQ "INFO.DE.BURO.INCORRECTA"
            Y.SELECTOR.NOTA.5 = 2

    END CASE


    O.DATA = Y.SELECTOR.NOTA.5

RETURN

END
