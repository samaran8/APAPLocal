* @ValidationCode : MjotMTk4NDA2ODI3NjpDcDEyNTI6MTY4MjQxMjM1NjU4NDpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MTpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 25 Apr 2023 14:15:56
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
SUBROUTINE REDO.V.VAL.ARC.STATUS
*-------------------------------------------------------------------------
*DESCRIPTION:
*------------
* This routine is attached as a validation routine to the version CR.CONTACT.LOG,REDO.PWD.AMEND to
* make the field CONTACT.NOTES mandatory if STATUS ne ACEPTA or FIRMADO
*-------------------------------------------------------------------------
* HISTORY:
*---------
*   Date               who           Reference                       Description

* 24-AUG-2011     SHANKAR RAJU     ODR-2011-07-0162              Initial Creation
*13-04-2023       Conversion Tool  R22 Auto Code conversion          No Changes
*13-04-2023       Samaran T         R22 Manual Code Conversion       No Changes
*-------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CR.CONTACT.LOG

    GOSUB CHECK.NOTES

RETURN
*-------------------------------------------------------------------------
CHECK.NOTES:
*~~~~~~~~~~~

    Y.CONTACT.STATUS = R.NEW(CR.CONT.LOG.CONTACT.STATUS)
    Y.CONTACT.NOTES = R.NEW(CR.CONT.LOG.CONTACT.NOTES)

    IF COMI EQ '' THEN
        IF Y.CONTACT.STATUS NE 'FIRMADO' THEN
            AF = CR.CONT.LOG.CONTACT.NOTES
            ETEXT = 'EB-REDO.ENTER.COMMENT'
            CALL STORE.END.ERROR
        END
    END

RETURN
*-------------------------------------------------------------------------
END
