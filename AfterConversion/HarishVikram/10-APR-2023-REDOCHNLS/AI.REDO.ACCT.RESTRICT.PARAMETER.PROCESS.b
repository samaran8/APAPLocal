* @ValidationCode : MjotMjMxOTMwMzQ6Q3AxMjUyOjE2ODExMDc0MjExNTU6SGFyaXNodmlrcmFtQzotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 10 Apr 2023 11:47:01
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
$PACKAGE APAP.REDOCHNLS
SUBROUTINE AI.REDO.ACCT.RESTRICT.PARAMETER.PROCESS
*********************************************************************************************************
*Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Program   Name    : REDO.PREVALANCE.STATUS.PROCESS
*--------------------------------------------------------------------------------------------------------
*Description       : This routine is a authorization routine for attaching the template REDO.PREVALANCE.STATUS.VALIDATE
*In Parameter      :
*Out Parameter     :
*--------------------------------------------------------------------------------------------------------
*Modification Details:
*=====================
*    Date            Who                            Reference                      Description
*   ------         ------                         -------------                    -------------
*  21/09/2011      Riyas                         ODR-2010-08-0490                Initial Creation
*
* 10-APR-2023     Conversion tool   		R22 Auto conversion   		FM TO @FM, VM to @VM
* 10-APR-2023      Harishvikram C             Manual R22 conversion      No changes
*********************************************************************************************************

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.AI.REDO.ACCT.RESTRICT.PARAMETER

    GOSUB OPEN.FILES
    GOSUB PROCESS
    GOSUB GOEND
RETURN

************
OPEN.FILES:
************
    FN.AI.REDO.ACCT.RESTRICT.PARAMETER = 'F.AI.REDO.ACCT.RESTRICT.PARAMETER'
    F.AI.REDO.ACCT.RESTRICT.PARAMETER = ''
    CALL OPF(FN.AI.REDO.ACCT.RESTRICT.PARAMETER,F.AI.REDO.ACCT.RESTRICT.PARAMETER)

RETURN
*--------------------------------------------------------------------------------
PROCESS:
*--------------------------------------------------------------------------------

    Y.ACCT.TYPE = R.NEW(AI.RES.PARAM.RESTRICT.ACCT.TYPE)
    CHANGE @VM TO @FM IN Y.ACCT.TYPE
    Y.DEBIT.CNT   = COUNT(Y.ACCT.TYPE,"DEBIT")
    Y.CREDIT.ACCT = COUNT(Y.ACCT.TYPE,"CREDIT")
    IF Y.DEBIT.CNT GT 1 OR Y.CREDIT.ACCT GT 1 THEN
        AV = 1
        AF    =  AI.RES.PARAM.RESTRICT.ACCT.TYPE
        ETEXT = "EB-DUPCAT.NT.ALLWD"
        CALL STORE.END.ERROR
    END
RETURN
*-----
GOEND:
*-----
END
