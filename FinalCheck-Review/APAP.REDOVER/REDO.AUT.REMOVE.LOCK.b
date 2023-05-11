* @ValidationCode : MjotMTk2OTA2ODI4OkNwMTI1MjoxNjgyNDEyMzI2ODM5OkhhcmlzaHZpa3JhbUM6LTE6LTE6MDoxOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 25 Apr 2023 14:15:26
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
SUBROUTINE REDO.AUT.REMOVE.LOCK
****************************************************************
*-------------------------------------------------------------------------
* Company Name  : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By  : Ganesh R
* Program Name  : REDO.AUT.REMOVE.LOCK
*-------------------------------------------------------------------------
* Description: This routine is a Authorisation routine
*
*----------------------------------------------------------
* Linked with:  T24.FUNDS.SERVICES,FCY.COLLECT and T24.FUNDS.SERVICES,LCY.COLLECT
* In parameter :
* out parameter : None
*------------------------------------------------------------------------
* MODIFICATION HISTORY
*--------------------------------------------
*   DATE              ODR                             DESCRIPTION
* 21-09-10          ODR-2010-09-0251              Initial Creation
*------------------------------------------------------------------------
*Modification History
*04-04-2023           Conversion Tool          R22 Auto Code conversion      No Changes
*04-04-2023            Samaran T                Manual R22 Code Conversion    No Changes
*-------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.REDO.CLEARING.OUTWARD

    GOSUB OPEN.FILE
    GOSUB PROCESS
RETURN

OPEN.FILE:
*Opening Files

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.REDO.CLEARING.OUTWARD = 'F.REDO.CLEARING.OUTWARD'
    F.REDO.CLEARING.OUTWARD = ''
    CALL OPF(FN.REDO.CLEARING.OUTWARD,F.REDO.CLEARING.OUTWARD)

RETURN

PROCESS:

*Get the Count of Transaction Field

    VAR.PAY.DETAILS = R.NEW(FT.PAYMENT.DETAILS)
    CALL F.READ(FN.REDO.CLEARING.OUTWARD,VAR.PAY.DETAILS,R.REDO.CLEARING.OUTWARD,F.REDO.CLEARING.OUTWARD,CLEARING.ERR)
    ACCT.ID = R.REDO.CLEARING.OUTWARD<CLEAR.OUT.AC.LOCK.ID>
    R.NEW(FT.PAYMENT.DETAILS) = 'RETURNED'
    CALL F.WRITE(FN.REDO.CLEARING.OUTWARD,VAR.PAY.DETAILS,R.REDO.CLEARING.OUTWARD)

    R.AC.LOCKED.EVENTS = ''
    APP.NAME = 'AC.LOCKED.EVENTS'
    OFSFUNCT = 'R'
    PROCESS  = 'PROCESS'
    OFSVERSION = 'AC.LOCKED.EVENTS,OFS'
    GTSMODE = ''
    NO.OF.AUTH = '0'
    TRANSACTION.ID = ACCT.ID
    OFSRECORD = ''

    OFS.MSG.ID =''
    OFS.SOURCE.ID = 'REDO.CHQ.ISSUE'
    OFS.ERR = ''

    CALL OFS.BUILD.RECORD(APP.NAME,OFSFUNCT,PROCESS,OFSVERSION,GTSMODE,NO.OF.AUTH,TRANSACTION.ID,R.AC.LOCKED.EVENTS,OFSRECORD)
    CALL OFS.POST.MESSAGE(OFSRECORD,OFS.MSG.ID,OFS.SOURCE.ID,OFS.ERR)

RETURN
END
