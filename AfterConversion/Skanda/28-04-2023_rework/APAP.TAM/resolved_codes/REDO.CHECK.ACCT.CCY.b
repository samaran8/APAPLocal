* @ValidationCode : MjotOTYxNjE5NTM0OkNwMTI1MjoxNjgwNjg0OTQ3NzA2OklUU1M6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 05 Apr 2023 14:25:47
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
*-----------------------------------------------------------------------------
* <Rating>-10</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE REDO.CHECK.ACCT.CCY
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :GANESH.R
*Program   Name    :REDO.CHECK.ACCT.CCY
*---------------------------------------------------------------------------------

*DESCRIPTION       :This program is used to get the Bank name
*
*LINKED WITH       :

** 05-04-2023 R22 Auto Conversion no changes
** 05-04-2023 Skanda R22 Manual Conversion - No changes
* ----------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.T24.FUND.SERVICES

    GOSUB PROCESS
RETURN

PROCESS:

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    ACCT.ID = COMI
    CALL F.READ(FN.ACCOUNT,ACCT.ID,R.ACCOUNT,F.ACCOUNT,ACCT.ERR)
    VAR.CCY = R.ACCOUNT<AC.CURRENCY>
    IF VAR.CCY NE LCCY THEN
        AF = TFS.SURROGATE.AC
        ETEXT = "EB-INVALID.CCY"
        CALL STORE.END.ERROR
    END

RETURN
END
