* @ValidationCode : MjotMTI3ODg2NjEyMjpDcDEyNTI6MTY4NTUzNDkzMTk3MDpzYW1hcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 31 May 2023 17:38:51
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : samar
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOAPAP
SUBROUTINE REDO.V.AUTH.NEW.ACCT.STATUS
*-------------------------------------------------------------------------------------------
*This is auth routine to update ACTIVE status in new account creation
*-------------------------------------------------------------------------------------------
*Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By     : Jayasurya H
*Program   Name    : REDO.V.AUTH.NEW.ACCT.STATUS
*---------------------------------------------------------------------------------
* LINKED WITH:
*----------------------------------------------------------------------
*Input param = none
*output param =none
*-----------------------------------------------------------------------
* MODIFICATION DETAILS:
*DATE           WHO                 REFERENCE               DESCRIPTION
*25-05-2023    CONVERSION TOOL     R22 AUTO CONVERSION     NO CHANGE
*25-05-2023    VICTORIA S          R22 MANUAL CONVERSION   call routine modified
*----------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT

    ACCOUNT.ID = ID.NEW

    STATUS.SEQ = 'ACTIVE'

    LREF.APP = 'ACCOUNT'
    LREF.FIELDS = 'L.AC.STATUS'
    LREF.POS=''
    CALL MULTI.GET.LOC.REF(LREF.APP,LREF.FIELDS,LREF.POS)
    Y.L.AC.STATUS.POS = LREF.POS<1,1>
    R.NEW(AC.LOCAL.REF)<1,Y.L.AC.STATUS.POS> = 'AC'

*CALL REDO.UPD.ACCOUNT.STATUS.DATE(ACCOUNT.ID,STATUS.SEQ)          ;*  To update new account active status
    APAP.REDOAPAP.redoUpdAccountStatusDate(ACCOUNT.ID,STATUS.SEQ) ;*R22 MANUAL CONVERSION
RETURN
END
