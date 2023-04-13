* @ValidationCode : MjoxNjkzNzQ0NTgyOkNwMTI1MjoxNjgxMzY3NzgyNzQ2OkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 13 Apr 2023 12:06:22
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOEB
SUBROUTINE REDO.ANC.REINV.APP

*--------------------------------------------------------------------------------
* Company Name : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Program Name : REDO.ANC.REINV.APP

*--------------------------------------------------------------------------------
* Description: This ANC routine is used to get the value of APP
*
*-----------------------------------------------------------------------------

*MODIFICATION HISTORY:
*
* DATE              WHO                REFERENCE                 DESCRIPTION
* 13-APR-2023     Conversion tool    R22 Auto conversion       No changes
* 13-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.AZ.ACCOUNT
    GOSUB INIT
    GOSUB PROCESS
RETURN
*---------------------------------------------------------------------------------
INIT:
*---------------------------------------------------------------------------------

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    LOC.REF.APPLICATION="ACCOUNT"
    LOC.REF.FIELDS='L.AZ.APP'
    POS.L.AZ.APP=''

    CALL GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,POS.L.AZ.APP)


RETURN
*---------------------------------------------------------------------------------
PROCESS:
*---------------------------------------------------------------------------------
    VAR.ID = ID.NEW
    CALL F.READ(FN.ACCOUNT,VAR.ID,R.ACCOUNT,F.ACCOUNT,ACC.ERR)

    IF R.ACCOUNT THEN

        Y.APP.ID = R.ACCOUNT<AC.LOCAL.REF,POS.L.AZ.APP>

        R.NEW(AZ.ALL.IN.ONE.PRODUCT) = Y.APP.ID

    END

RETURN
*-----------------------------------------------
END
