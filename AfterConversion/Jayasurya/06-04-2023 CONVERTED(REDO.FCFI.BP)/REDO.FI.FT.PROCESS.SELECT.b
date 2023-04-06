* @ValidationCode : Mjo2MjkyNDI4MDpDcDEyNTI6MTY4MDc1ODY1MTM3MjpJVFNTQk5HOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 06 Apr 2023 10:54:11
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.REDOFCFI
SUBROUTINE REDO.FI.FT.PROCESS.SELECT
*-------------------------------------------------------------------------------------------------------------------------------
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*04-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*04-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*-------------------------------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_REDO.FI.FT.PROCESS.COMMON

    GOSUB PROCESS
RETURN
*-------
PROCESS:
*-------
    Y.SEL.CMD='SELECT ':FN.REDO.TEMP.FI.CONTROL:' WITH STATUS EQ ""'
    CALL EB.READLIST(Y.SEL.CMD,Y.REC.LIST,'',NO.OF.REC,Y.ERR)
    CALL BATCH.BUILD.LIST('',Y.REC.LIST)
RETURN
END
