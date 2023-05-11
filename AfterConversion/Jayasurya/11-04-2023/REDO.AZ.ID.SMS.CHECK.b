* @ValidationCode : MjotMjA3MjgwNDAyNzpDcDEyNTI6MTY4MTIwNTA5NTE5MzpJVFNTQk5HOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 11 Apr 2023 14:54:55
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
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*11-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*11-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*------------------------------------------------------------------------------------------------------------------
SUBROUTINE REDO.AZ.ID.SMS.CHECK
*-------------------------------------------------------------------
* Description: This ID routine is to check whether any existing record is getting modified
*              through the creation version then it will throw the error.
*-------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE

    IF V$FUNCTION EQ "I" THEN
        GOSUB PROCESS
    END

RETURN
*-------------------------------------------------------------------
PROCESS:
*-------------------------------------------------------------------

    FN.FILE = "F.":APPLICATION
    F.FILE  = ""
    CALL OPF(FN.FILE,F.FILE)

    Y.ID = COMI

    CALL F.READ(FN.FILE,Y.ID,R.RECORD,F.FILE,FILE.ERR)

    IF R.RECORD THEN
        E = "EB-TRANSACTION.NOT.ALLOWED"
        CALL STORE.END.ERROR
    END

RETURN
END
