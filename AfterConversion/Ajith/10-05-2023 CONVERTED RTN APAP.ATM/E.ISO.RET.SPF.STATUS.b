* @ValidationCode : MjoxMDk0MzI3MjkzOkNwMTI1MjoxNjgzNjk1ODg4ODQ2OmFqaXRoOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 10 May 2023 10:48:08
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ajith
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.ATM
SUBROUTINE E.ISO.RET.SPF.STATUS(Y.ID.LIST)
*Modification History
*  Date       Who             Reference       Description
* 24 Aug 2011 Balagurunathan ODR-2010-08-0469 added the value to Unique transaction code for issue PACS00084788
* Date                  who                   Reference
* 24-04-2023         CONVERSTION TOOL      R22 AUTO CONVERSTION = TO EQ
* 24-04-2023          ANIL KUMAR B         R22 MANUAL CONVERSTION CALL rtn format can be modified
*-------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.SPF
    $INSERT I_AT.ISO.COMMON
    $INSERT I_ATM.BAL.ENQ.COMMON
    
    Y.ID.LIST = "STATUS:1:1=":R.SPF.SYSTEM<SPF.OP.MODE>
    Y.ID.LIST:=',UNIQUE.TXN.CODE:1:1=1'
    Y.ID.LIST := ',Y.ISO.RESPONSE:1:1='

    IF AT$INCOMING.ISO.REQ(3)[1,2] EQ '94' THEN
        Y.UNIQUE.ID=AT$INCOMING.ISO.REQ(38)
* CALL APAP.ATM.V.FT.UPD.ENQ.ATM.KEY.ID
        CALL APAP.ATM.vFtUpdEnqAtmKeyId() ;*R22 Manual code conversion
        

    END
    IF R.SPF.SYSTEM<SPF.OP.MODE> EQ 'O' THEN
        Y.ID.LIST := '00'
    END ELSE
        Y.ID.LIST := '01'
    END

RETURN
END
