* @ValidationCode : MjoxOTYxODMwNzg5OkNwMTI1MjoxNjgwNzgxNDU1NjQ2OklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 06 Apr 2023 17:14:15
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.REDORETAIL
SUBROUTINE LATAM.CARD.CLASS.CODE.ID
*-----------------------------------------------------------------------------
*<doc>
* ID Validation is done here
* here
* @stereotype Application
* @package TODO define the product group and product, e.g. infra.eb
* </doc>
*-----------------------------------------------------------------------------
* TODO - You MUST write a .FIELDS routine for the field definitions
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------
* 19/10/07 - EN_10003543
*            New Template changes
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*06-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*06-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
* ----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_METHODS.AND.PROPERTIES
    $INSERT I_F.COMPANY

    GOSUB INITALISE
    GOSUB CHECK.ID

RETURN
*------------------------------------------------------------------------------
INITALISE:

    FN.COMPANY = "F.COMPANY"
    F.COMPANY = ""
    CALL OPF(FN.COMPANY,F.COMPANY)
RETURN

*-----------------------------------------------------------------------------
CHECK.ID:

    SELECT.COMP = "SELECT ":FN.COMPANY
    CALL EB.READLIST(SELECT.COMP,SEL.LIST.COMP,'',NO.OF.COMP,SEL.COMP)

    LOCATE ID.NEW IN SEL.LIST.COMP SETTING POS ELSE
        E = 'EB-NOT.VALID.ID'
        CALL ERR
    END
RETURN
*-----------------------------------------------------------------------------
END
