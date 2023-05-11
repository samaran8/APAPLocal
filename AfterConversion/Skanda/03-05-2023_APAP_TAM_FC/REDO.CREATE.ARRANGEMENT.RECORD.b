* @ValidationCode : MjotNjY2OTQ2NzYyOkNwMTI1MjoxNjgzMTEyMzM1OTA3OklUU1M6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 03 May 2023 16:42:15
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
$PACKAGE APAP.TAM
* Version 9 16/05/01  GLOBUS Release No. 200511 31/10/05
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
SUBROUTINE REDO.CREATE.ARRANGEMENT.RECORD
*----------------------------------------------------------------------------------------------------
*
* Subroutine Type : ROUTINE
* Attached to     : TEMPLATE REDO.CREATE.ARRANGEMENT
* Attached as     : ROUTINE
* Primary Purpose : Set default values
*
* Incoming:
* ---------
*
*
* Outgoing:
* ---------
*
*
* Error Variables:
*
*-----------------------------------------------------------------------------------
* Modification History:
*
* Development for : Asociacion Popular de Ahorros y Prestamos
* Development by  : Juan Pablo Armas - TAM Latin America
* Date            : 08 Junio 2011
*
** 06-04-2023 R22 Auto Conversion
** 06-04-2023 Skanda R22 Manual Conversion - No changes
*-----------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON


* Check if the record is okay to input to
    GOSUB CHECK.RECORD
    IF E EQ '' THEN ;* R22 Auto conversion
        GOSUB SET.ENRICHMENTS
        GOSUB INITIALIZE
        GOSUB POPULATE.COMMONS
    END

RETURN
*-----------------------------------------------------------------------
POPULATE.COMMONS:

    CALL REDO.FC.S.POPVALUES(ID.PARAMS)
RETURN
*-----------------------------------------------------------------------------
INITIALIZE:

    ID.PARAMS = "SYSTEM"

RETURN
*-----------------------------------------------------------------------------
SET.ENRICHMENTS:
*      CALL EB.SET.FIELD.ENRICHMENT(FIELD.NUMBER, FIELD.ENRICHMENT)
RETURN
*--------------------------------------------------------------------
CHECK.RECORD:

    Y.FIRST.TIME = ''
*   IF OFS$BROWSER EQ 1 THEN
    IF OFS$GETRECORD EQ 1 THEN  ;* OFS$GETRECORD is 1, when the application was called for first time
        Y.FIRST.TIME = 1
    END
*   END

RETURN
*-----------------------------------------------------------------------------
END
