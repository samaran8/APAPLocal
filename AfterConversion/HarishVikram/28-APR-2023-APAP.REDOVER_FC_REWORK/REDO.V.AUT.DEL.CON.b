* @ValidationCode : MjoxNTQ5NjMzMzA6Q3AxMjUyOjE2ODI0MTIzMzQwOTg6SGFyaXNodmlrcmFtQzotMTotMTowOjE6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 25 Apr 2023 14:15:34
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.V.AUT.DEL.CON
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :PRABHU.N
*Program   Name    :REDO.V.AUT.DEL.CON
*---------------------------------------------------------------------------------

*DESCRIPTION       :It is attached as authorization routine used to delete the contracts
*LINKED WITH       :

* ----------------------------------------------------------------------------------
*Modification Details:
*=====================
*   Date              who           Reference                       Description
* 28-Dec-2010      Prabhu.N         ODR-2010-08-0031              Initial Creation
*06-04-2023       Conversion Tool   R22 Auto Code conversion        IF CONDITION ADDED
*06-04-2023       Samaran T         R22 Manual Code Conversion       No Changes
*-------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.ADD.THIRDPARTY
    $INSERT I_System

    GOSUB OPEN.PARA
    GOSUB PROCESS.PARA

RETURN
*---------
OPEN.PARA:
*---------
    FN.CUS.CON.LIST = 'F.CUS.CON.LIST'
    F.CUS.CON.LIST  = ''
    CALL OPF(FN.CUS.CON.LIST,F.CUS.CON.LIST)

RETURN

*------------
PROCESS.PARA:
*------------
    CUSTOMER.ID = System.getVariable('EXT.SMS.CUSTOMERS')
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN  ;*R22 AUTO CODE CONVERSION
        CUSTOMER.ID = ""  ;*R22 AUTO CODE CONVERSION
    END  ;*R22 AUTO CODE CONVERSION

    GOSUB DEL.EXISTING.CON

RETURN
*------------------
DEL.EXISTING.CON:
*------------------
    CUS.CON.LIST.ID = CUSTOMER.ID
    CALL F.READ(FN.CUS.CON.LIST,CUS.CON.LIST.ID,R.CUS.CON.LIST,F.CUS.CON.LIST,CUS.CON.LIST.ER)
    Y.TP.ID = ID.NEW:'*':R.NEW(ARC.TP.CONTRACT.NO)
    LOCATE Y.TP.ID IN R.CUS.CON.LIST<1> SETTING Y.CON.POS THEN
        DEL R.CUS.CON.LIST<Y.CON.POS>
    END

    CALL F.WRITE(FN.CUS.CON.LIST,CUS.CON.LIST.ID,R.CUS.CON.LIST)

RETURN
END
*---------------------------------------------*END OF SUBROUTINE*-------------------------------------------
