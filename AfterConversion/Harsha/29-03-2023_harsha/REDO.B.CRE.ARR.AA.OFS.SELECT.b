* @ValidationCode : MjoxMTUyNzQ1MDM5OkNwMTI1MjoxNjgxOTk0OTQ3MDY5OklUU1M6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 20 Apr 2023 18:19:07
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.AA
* Version 1 13/04/00  GLOBUS Release No. G14.0.00 03/07/03
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
SUBROUTINE REDO.B.CRE.ARR.AA.OFS.SELECT
*-----------------------------------------------------------------------------
* Fabrica de Credito
* This SERVICE has to check if the AA that was queued by REDO.CREATE.ARRANGEMENT
* is created OK or NOT
*
*        AUTHOR                   DATE
*-----------------------------------------------------------------------------
* hpasquel@temenos.com         2011-01-11
*-----------------------------------------------------------------------------
*------------------------------------------------------------------------
* Modification History :
*------------------------------------------------------------------------
*  DATE             WHO                   REFERENCE                  
* 20-APRIL-2023      Conversion Tool       R22 Auto Conversion - No changes
* 20-APRIL-2023      Harsha                R22 Manual Conversion - No changes                             
*------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
*
    $INSERT I_REDO.B.CRE.ARR.AA.OFS.COMMON

    LIST.PARAMETERS = '' ; ID.LIST = ''

    SELECT.STATEMENT = 'SELECT ':FN.REDO.CRE.ARR.AA.OFS.LIST
    NO.SELECTED = ''
    SYSTEM.RETURN.CODE = ''
    CALL EB.READLIST(SELECT.STATEMENT,ID.LIST,'',NO.SELECTED,SYSTEM.RETURN.CODE)

*     LIST.PARAMETERS<2> = 'F.REDO.CRE.ARR.AA.OFS.LIST'
*     LIST.PARAMETERS<3> = 'TRADE.CCY EQ "USD"'

    CALL BATCH.BUILD.LIST(LIST.PARAMETERS,ID.LIST)

RETURN
END
