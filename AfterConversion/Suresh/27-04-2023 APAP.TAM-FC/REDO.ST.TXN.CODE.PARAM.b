* @ValidationCode : MjotNzk3ODAzMjgyOkNwMTI1MjoxNjgxMzgxNTE5Njc1OjMzM3N1Oi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 13 Apr 2023 15:55:19
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : 333su
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.ST.TXN.CODE.PARAM
*-----------------------------------------------------------------------------
*<doc>
* This is a local template to hold trade nominal values created for SC002
* @author rshankar@temenos.com
* </doc>
*-----------------------------------------------------------------------------
* Modification History :
*
* 03/12/2010 - ODR-2010-07-0081
*            New Template creation
*
*DATE                 WHO                  REFERENCE                     DESCRIPTION
*13/04/2023      CONVERSION TOOL     AUTO R22 CODE CONVERSION             NOCHANGE
*13/04/2023         SURESH           MANUAL R22 CODE CONVERSION           NOCHANGE
* ----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_Table

*-----------------------------------------------------------------------------
    Table.name = 'REDO.ST.TXN.CODE.PARAM'
    Table.title = 'REDO.ST.TXN.CODE.PARAM'
    Table.stereotype = 'H'
    Table.product = 'EB'
    Table.subProduct = ''
    Table.classification = 'INT'
    Table.systemClearFile = 'Y'
    Table.relatedFiles = ''
    Table.isPostClosingFile = ''
    Table.equatePrefix = 'ST.TXN.CODE'
*-----------------------------------------------------------------------------
    Table.idPrefix = ''
    Table.blockedFunctions = ''
    Table.trigger = ''
*-----------------------------------------------------------------------------

RETURN
END
