* @ValidationCode : MjoxNTk0MTE1NTc5OkNwMTI1MjoxNjgxMzc3NDg2NDc1OklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 13 Apr 2023 14:48:06
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
SUBROUTINE REDO.DS.ACL.ACCT.TITLE(ACCT.TITLE)
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :S SUDHARSANAN
*Program   Name    :REDO.DS.ACL.ACCT.TITLE
*---------------------------------------------------------------------------------
* DESCRIPTION       :This program is used to get the account title value.
* ----------------------------------------------------------------------------------
* MODIFICATION HISTORY:
* DATE            WHO               REFERENCE              DESCRIPTION
* 07 MAY 2012     Sudharsanan S     CR.20         This program is used to get the account title value.
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*13-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*13-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*------------------------------------------------------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT.CLOSURE
    $INSERT I_F.ACCOUNT

    GOSUB PROCESS
RETURN
*********
PROCESS:
**********
    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.ACCOUNT.HIS = 'F.ACCOUNT$HIS'
    F.ACCOUNT.HIS = ''
    CALL OPF(FN.ACCOUNT.HIS,F.ACCOUNT.HIS)

    VAR.ID = ID.NEW
    CALL F.READ(FN.ACCOUNT,VAR.ID,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
    IF NOT(R.ACCOUNT) THEN
        CALL EB.READ.HISTORY.REC(F.ACCOUNT.HIS,VAR.ID,R.ACCOUNT,ACC.ERR)
    END
    ACCT.TITLE = R.ACCOUNT<AC.ACCOUNT.TITLE.1>:" ":R.ACCOUNT<AC.ACCOUNT.TITLE.2>

RETURN
END
*----------------------------------------------- End Of Record ----------------------------------
