* @ValidationCode : Mjo2MjY4MTY5Mzk6Q3AxMjUyOjE2ODExMjI1MjMzMTE6SGFyaXNodmlrcmFtQzotMTotMTowOjA6dHJ1ZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 10 Apr 2023 15:58:43
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : true
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOCHNLS
SUBROUTINE AI.REDO.TXN.TYPE.FIELDS
*-----------------------------------------------------------------------------
*<doc>
* Template for field definitions routine REDO.BRANCH.STATUS.FIELDS *
* @author riyasbasha@temenos.com
* @stereotype fields template
* Reference : ODR-2010-08-0031
* @uses Table
* @public Table Creation
* @package infra.eb
* </doc>
*-----------------------------------------------------------------------------
* Modification History :
*
* 04/01/10 - EN_10003543
*            New Template changes
*
* 10-APR-2023     Conversion tool    R22 Auto conversion       No changes
* 10-APR-2023     Conversion tool    R22 Auto conversion       No changes
*-----------------------------------------------------------------------------
*** <region name= Header>
*** <desc>Inserts and control logic</desc>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DataTypes
*** </region>
*-----------------------------------------------------------------------------
    ID.F = '@ID'
    ID.N = '35'
    ID.T = 'A'


    fieldName = 'XX.LL.DESCRIPTION'
    fieldLength = '50.1'
    fieldType = 'A'
    neighbour = ''
    CALL Table.addFieldDefinition(fieldName, fieldLength, fieldType, neighbour)

*-----------------------------------------------------------------------------
    CALL Table.setAuditPosition ;* Poputale audit information
*-----------------------------------------------------------------------------
RETURN
*-----------------------------------------------------------------------------
END
