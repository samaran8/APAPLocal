* @ValidationCode : MjotMTc0MTQzMzY5NTpDcDEyNTI6MTY4MTIwNTA2Mjc0NjpJVFNTQk5HOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 11 Apr 2023 14:54:22
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
SUBROUTINE REDO.AZ.FUND.PARAM.VALIDATE
*********************************************************************************************************
*Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Developed By      : Temenos Application Management
*Program   Name    : REDO.AZ.FUND.PARAM.VALIDATE
*--------------------------------------------------------------------------------------------------------
*Description       : REDO.AZ.FUND.PARAM.VALIDATE is a validation routine attached to the TEMPLATE
*                    - REDO.AZ.FUND.PARAM , the routine checks if the duplicate value entered in the
*                    CURRENCY field
*Linked With       : Template - REDO.AZ.FUND.PARAM
*In  Parameter     : NA
*Out Parameter     : NA
*Files  Used       : REDO.AZ.FUND.PARAM           As              I               Mode
*--------------------------------------------------------------------------------------------------------
*Modification Details:
*=====================
*    Date               Who                  Reference                  Description
*   ------            ------               -------------               -------------
* 01 Nov 2010       Sudharsanan S               CR.18                 Initial Creation
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*11-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*11-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*********************************************************************************************************
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.AZ.FUND.PARAM
*-------------------------------------------------------------------------------------------------------
    GOSUB CHECK.DUP
RETURN
*--------------------------------------------------------------------------------------------------------
**********
CHECK.DUP:
**********
    AF = REDO.FUND.CURRENCY
    CALL DUP

RETURN
*--------------------------------------------------------------------------------------------------------
END       ;* End of Program
