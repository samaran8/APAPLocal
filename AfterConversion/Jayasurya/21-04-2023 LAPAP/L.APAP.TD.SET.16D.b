* @ValidationCode : Mjo4NjMzMjM3MTU6Q3AxMjUyOjE2ODIwNjg3NzQyMDk6SVRTU0JORzotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 21 Apr 2023 14:49:34
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.LAPAP
SUBROUTINE L.APAP.TD.SET.16D
*----------------------------------------------------------------------------------------------------
* DESCRIPTION : This AUTO.CONTENT.ROUTINE  is to set CARD NUMBER in LATAM.CARD.ORDER VERSION from ID.NEW
*-----------------------------------------------------------------------------------------------------
*-----------------------------------------------------------------------------------------------------
* * Input / Output
* --------------
* IN     : -NA-
* OUT    : -NA-
*-----------------------------------------------------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : ARCADIO RUIZ
* PROGRAM NAME : L.APAP.TD.SET.16D
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*21-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 BP NAME REMOVED
*21-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*-----------------------------------------------------------------------------------------------------
    $INSERT I_COMMON ;* AUTO R22 CODE CONVERSION- BP NAME REMOVED
    $INSERT I_EQUATE ;* AUTO R22 CODE CONVERSION- BP NAME REMOVED
    $INSERT I_F.LATAM.CARD.ORDER ;* AUTO R22 CODE CONVERSION- BP NAME REMOVED

    GOSUB PROCESS
RETURN

PROCESS:
*-------

    R.NEW(CARD.IS.CARD.LINKED) = FIELD(ID.NEW,'.',2)
RETURN
*------------------------------------------------------------------------------------------------------------
END
