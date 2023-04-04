* @ValidationCode : MjotNzQ3NDY0MjY3OkNwMTI1MjoxNjgwNjAzNzE1ODMzOklUU1NCTkc6LTE6LTE6MDowOnRydWU6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 04 Apr 2023 15:51:55
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : true
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.REDOFCFI
SUBROUTINE REDO.FC.CL.BALANCE.FIELDS
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
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*04-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*04-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*-----------------------------------------------------------------------------
*** <region name= Header>
*** <desc>Inserts and control logic</desc>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DataTypes
    $INSERT I_F.PGM.FILE
    $INSERT I_F.VERSION

*** </region>
*-----------------------------------------------------------------------------
    CALL Table.defineId("ID", T24_String) ;* Define Table id
*-----------------------------------------------------------------------------
    CALL Table.addField("AA.AMOUNT", T24_Numeric, "", "")     ;* Add a new fields
    CALL Table.addField("AA.BALANCE", T24_Numeric, "", "")    ;* Add a new fields

    CALL Table.addFieldDefinition("XX<COLLATERAL.RIGHT", 35, "A", "")   ;* Add a new fields
    CALL Field.setCheckFile("COLLATERAL.RIGHT")     ;* Use DEFAULT.ENRICH from SS or just field 1

    CALL Table.addFieldDefinition("XX-COLLATERAL.ID", 35, "A", "")      ;* Add a new fields
    CALL Field.setCheckFile("COLLATERAL") ;* Use DEFAULT.ENRICH from SS or just field 1

    CALL Table.addField("XX-MG.ACTUAL", T24_Numeric, "", "")  ;* Add a new fields
    CALL Table.addField("XX>MG.ORIGINAL", T24_Numeric, "", "")          ;* Add a new fields

*-----------------------------------------------------------------------------
    CALL Table.setAuditPosition ;* Poputale audit information
*-----------------------------------------------------------------------------
RETURN
*-----------------------------------------------------------------------------
END
