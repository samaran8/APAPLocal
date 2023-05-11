* @ValidationCode : MjotMTY5ODUzNzM3NTpDcDEyNTI6MTY4MTIwNjI0MjE3OTpJVFNTQk5HOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 11 Apr 2023 15:14:02
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
SUBROUTINE REDO.CARD.BRANCH.ID.DELIVER
*--------------------------------------------------------------------------------------------------------
*Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Developed By      : Temenos Application Management
*Program   Name    : REDO.CARD.BRANCH.ID.DELIVER
*--------------------------------------------------------------------------------------------------------
*Description  : This is a ID routine to avoid "delivered to branch" status if the cards have not been generated yet
*Linked With  : REDO.CARD.REQUEST
*In Parameter : N/A
*Out Parameter: N/A
*--------------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
*    Date            Who                  Reference               Description
*   ------         ------               -------------            -------------
* 1 DEC 2010    SWAMINATHAN       ODR-2010-03-0400        Initial Creation
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*11-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*11-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*--------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_F.REDO.CARD.GENERATION

    FN.REDO.CARD.GENERATION = 'F.REDO.CARD.GENERATION'
    F.REDO.CARD.GENERATION = ''
    CALL OPF(FN.REDO.CARD.GENERATION,F.REDO.CARD.GENERATION)
    R.REDO.CARD.GENERATION = ''
    Y.CARD.REQ.ID = ID.NEW
    CALL F.READ(FN.REDO.CARD.GENERATION,Y.CARD.REQ.ID,R.REDO.CARD.GENERATION,F.REDO.CARD.GENERATION,Y.ERR.GEN)
    IF R.REDO.CARD.GENERATION EQ '' THEN
        E='EB-CARD.DELI.TO.BRANCH'
    END
RETURN
END
