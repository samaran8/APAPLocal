* @ValidationCode : MjoxNTA5MTc1MjgxOkNwMTI1MjoxNjgyNTk4MDE3MzE4OnNhbWFyOi0xOi0xOjA6MTpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 27 Apr 2023 17:50:17
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : samar
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDORETAIL
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*12-04-2023            CONVERSION TOOL                AUTO R22 CODE CONVERSION           VM TO @VM ,FM TO @FM
*12-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
SUBROUTINE REDO.COL.EXTRACT.CREDIT.READ(AA.ID,R.AA,R.ACT.HIST,R.PRINCIPALINT,R.MORAINT,R.AA.ACCOUNT.DETAILS)
*-----------------------------------------------------------------------------
* Name : REDO.COLLECTOR.EXTRACT.CREDIT.READ
*      : Allows to read files to use in REDO.COLLECTOR.EXTRACT.CREDIT routine
*
* @author hpasquel@temenos.com
* @stereotype subroutine
* @package REDO.COL
*
* @Parameters:
* ----------------------------------------------------------------------------
*                AA.ID                 (in)    Arrangement id
*                R.AA                  (out)   AA.ARRANGEMENT record
*                R.ACT.HIST            (out)   AA.ACTIVITY.HISTORY record
*                R.PRINCIPALINT        (out)   AA.INTEREST.ACCRUAL record for PRINCIPALINT
*                R.PENALTYINT          (out)   AA.INTEREST.ACCRUAL record for PENALTYINT
*                R.AA.ACCOUNT.DETAILS  (out)   AA.ACCOUNT.DETAILS record
* ----------------------------------------------------------------------------
*             E               (out) The message error
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AA.ARRANGEMENT
*

    $INSERT I_REDO.COL.CUSTOMER.COMMON
    $INSERT I_REDO.COL.EXTRACT.CREDIT.COMMON
    $INSERT I_F.REDO.APAP.PROPERTY.PARAM
*
    GOSUB PROCESS
RETURN
* --------------------------------------------------------------------------------------------
PROCESS:
* --------------------------------------------------------------------------------------------
* Read Main Files

    CALL F.READ(FN.AA, AA.ID,R.AA, F.AA, YERR)
    IF YERR THEN
        E = yRecordNotFound : @FM : AA.ID : @VM : "F.AA.ARRANGEMENT"
        RETURN
    END

    REQD.MODE = ''; EFF.DATE =R.AA<AA.ARR.START.DATE>; R.AA.ACTIVITY.HISTORY = ''
    CALL AA.READ.ACTIVITY.HISTORY(AA.ID, REQD.MODE, EFF.DATE, R.ACT.HIST)

    IF NOT(R.ACT.HIST) THEN
        E = yRecordNotFound : @FM : AA.ID : @VM : "F.AA.ACTIVITY.HISTORY"
        RETURN
    END

    Y.PRIN.INT.RATE.ID = AA.ID : "-PRINCIPALINT"
    R.PRINCIPALINT = ""
    CALL F.READ(FN.AA.INTEREST.ACCRUALS, Y.PRIN.INT.RATE.ID ,R.PRINCIPALINT, F.AA.INTEREST.ACCRUALS, YERR)

*    Y.PENALTY.INT.RATE.ID = AA.ID : "-PENALTYINT"
*    R.PENALTYINT = ""
*    CALL F.READ(FN.AA.INTEREST.ACCRUALS, Y.PENALTY.INT.RATE.ID ,R.PENALTYINT, F.AA.INTEREST.ACCRUALS, YERR)

    Y.PRODUCT.GROUP=R.AA<AA.ARR.PRODUCT.GROUP>
******PACS00523653*****
*    CALL F.READ(FN.REDO.APAP.PROPERTY.PARAM,Y.PRODUCT.GROUP,R.REDO.APAP.PROPERTY.PARAM,F.REDO.APAP.PROPERTY.PARAM,ERR)
    CALL CACHE.READ(FN.REDO.APAP.PROPERTY.PARAM,Y.PRODUCT.GROUP,R.REDO.APAP.PROPERTY.PARAM,ERR)
******PACS00523653*****
    PROP.CLASS = ''
    PROP.NAME  = R.REDO.APAP.PROPERTY.PARAM<PROP.PARAM.PENALTY.ARREAR>
    returnConditions = ''
    RET.ERR = ''

    CALL AA.GET.ARRANGEMENT.CONDITIONS(AA.ID,PROP.CLASS,PROP.NAME,'','',R.MORAINT,ERR.COND)

    CALL F.READ(FN.AA.DETAILS, AA.ID,R.AA.ACCOUNT.DETAILS, F.AA.DETAILS, YERR)
    IF YERR THEN
        E = yRecordNotFound : @FM : AA.ID : @VM : "F.AA.ACCOUNT.DETAILS"
    END

RETURN
* ------------------------------------------------------------------------------------
END
