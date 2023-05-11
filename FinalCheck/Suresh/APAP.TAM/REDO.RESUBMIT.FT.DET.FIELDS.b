* @ValidationCode : MjotODg5ODQ1MDc3OkNwMTI1MjoxNjgxMzc5MzQwMjMzOjMzM3N1Oi0xOi0xOjA6MDp0cnVlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 13 Apr 2023 15:19:00
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : 333su
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : true
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.RESUBMIT.FT.DET.FIELDS
*-----------------------------------------------------------------------------
*<doc>
* Template for field definitions routine YOURAPPLICATION.FIELDS
*
* @author tcoleman@temenos.com
* @stereotype fields template
* @uses Table
* @public Table Creation
* @package infra.eb
* </doc>
*-----------------------------------------------------------------------------
* Modification History :
*
* 19/10/07 - EN_10003543
*            New Template changes
*
* 14/11/07 - BG_100015736
*            Exclude routines that are not released
*Modification History:
*DATE                 WHO                  REFERENCE                     DESCRIPTION
*13/04/2023      CONVERSION TOOL     AUTO R22 CODE CONVERSION             NOCHANGE
*13/04/2023         SURESH           MANUAL R22 CODE CONVERSION           NOCHANGE
*-----------------------------------------------------------------------------
*** <region name= Header>
*** <desc>Inserts and control logic</desc>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DataTypes
*** </region>
*-----------------------------------------------------------------------------
    CALL Table.defineId("ARRANGEMENT.ID", T24_String)         ;* Define Table id
*-----------------------------------------------------------------------------

    neighbour = ''
    fieldName = 'XX<FT.ID'
    fieldLength = '35'
    fieldType = 'A'
    CALL Table.addField(fieldName, fieldType, args, neighbour)
    fieldName = 'XX-DATE'
    fieldLength = '11'
    fieldType = 'D'
    CALL Table.addField(fieldName, fieldType, args, neighbour)
    fieldName = 'XX-BILL.AMT'
    fieldLength = '35'
    fieldType = 'A'
    CALL Table.addField(fieldName, fieldType, args, neighbour)
    fieldName = 'XX>OFS.MSG.ID'
    fieldLength = '35'
    fieldType = 'A'
    CALL Table.addField(fieldName, fieldType, args, neighbour)
RETURN
*-----------------------------------------------------------------------------
END
