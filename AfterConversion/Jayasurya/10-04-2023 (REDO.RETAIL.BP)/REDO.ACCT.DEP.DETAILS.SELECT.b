* @ValidationCode : MjoyMTAzMDI1NjA5OkNwMTI1MjoxNjgxMTE5NjQwMDk3OklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 10 Apr 2023 15:10:40
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
*10-04-2023            CONVERSION TOOL                AUTO R22 CODE CONVERSION              NO CHANGES
*10-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*-----------------------------------------------------------------------------------------------------------------------------
SUBROUTINE REDO.ACCT.DEP.DETAILS.SELECT
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CUSTOMER
    $INSERT I_F.STMT.ENTRY
    $INSERT I_F.RELATION
    $INSERT I_F.CATEGORY
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.ACCOUNT.CLASS
    $INSERT I_F.REDO.AML.PARAM
    $INSERT I_REDO.ACCT.DEP.DETAILS.COMMON
    $INSERT I_F.REDO.H.TELLER.TXN.CODES

    GOSUB SELECT.PROCES
*
RETURN
*------------------------------------------------------------------------------------------------------------------------------------------------------------
SELECT.PROCES:
*-------------
    CALL EB.CLEAR.FILE(FN.REDO.APAP.BKP.REP55,F.REDO.APAP.BKP.REP55)

    SEL.CMD = " SELECT ": FN.ACCT.ENT.LWORK.DAY
    SEL.LIST = ''
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,ERR.CODE)
    CALL BATCH.BUILD.LIST('',SEL.LIST)
*
RETURN
*-----------------------------------------------------------------------------------------------------------------
END
