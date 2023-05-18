* @ValidationCode : Mjo5OTI4NDY2OTc6Q3AxMjUyOjE2ODQ0MTAwNDI3NTA6c2FtYXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 18 May 2023 17:10:42
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : samar
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.CHANGE.TX.PY

* Developed By : TAM (Marimuthu S)
* Reference :
* Description : This is conversion routine used to change the payoff bill descriptions to spanish

** 21-04-2023 R22 Auto Conversion no changes
** 21-04-2023 Skanda R22 Manual Conversion - No changes

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON

    BEGIN CASE

        CASE O.DATA EQ 'CURRENT'
            O.DATA = 'SALDOS VIGENTE'
        CASE O.DATA EQ 'DUE'
            O.DATA = 'VENCIDO'
        CASE O.DATA EQ 'OVERDUE'
            O.DATA = 'VENCIDO + 30 DIAS'
        CASE O.DATA EQ 'CHARGE'
            O.DATA = 'CARGOS'
    END CASE

RETURN

END
