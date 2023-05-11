* @ValidationCode : MjoxMTQ2NTE0ODQ2OkNwMTI1MjoxNjgyNTEwMTYwMDA0OkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 26 Apr 2023 17:26:00
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
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.V.INP.AZ.ACC.DEF.TASA.POOL
*-----------------------------------------------------------------------------------------------
*DESCRIPTION:
*  This routine is attached as input routine to the AZ.ACCOUNT versions used for Deposit opening
* ----------------------------------------------------------------------------------------------
* Input/Output:
*--------------
* IN  : -NA-
* OUT : -NA-
*
* Dependencies:
*---------------
* CALLS     : -NA-
* CALLED BY : -NA-
*
* Revision History:
*------------------
*   Date               who                  Reference            Description
*
* 03-MAY-2010  N. Satheesh Kumar         ODR-2009-10-0325            Initial Creation
* 01-APR-2011     H GANESH                 PACS00052350               Modified as per issue
*11-04-2023       Conversion Tool        R22 Auto Code conversion          VM TO @VM
*11-04-2023       Samaran T               R22 Manual Code Conversion       No Changes
 
*---------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.AZ.PRODUCT.PARAMETER
    $INSERT I_F.REDO.POOL.RATE
    $USING APAP.TAM

    IF R.NEW(AZ.CURR.NO) NE '' THEN
        RETURN
    END

    GOSUB OPEN.FILES
    GOSUB GET.LOC.REF.POS
    GOSUB PROCESS
RETURN

*----------
OPEN.FILES:
*----------
*---------------------------------------
* This section opens the necessary files
*---------------------------------------
    Y.YR.DIV=''

    FN.REDO.POOL.RATE = 'F.REDO.POOL.RATE'
    F.REDO.POOL.RATE = ''
    R.REDO.POOL.RATE = ''
    CALL OPF(FN.REDO.POOL.RATE,F.REDO.POOL.RATE)

    FN.AZ.PRODUCT.PARAMETER='F.AZ.PRODUCT.PARAMETER'

RETURN

*---------------
GET.LOC.REF.POS:
*---------------
*-----------------------------------------------------------------------------------------
* This section gets the position of the local reference fields from AZ.ACCOUNT application
*-----------------------------------------------------------------------------------------

    LREF.APPLNS = 'AZ.ACCOUNT'
    LREF.FIELDS = 'L.EB.REVIEW':@VM:'L.EB.PROFITLOSS':@VM:'L.EB.TASA.POOL'
    CALL MULTI.GET.LOC.REF(LREF.APPLNS,LREF.FIELDS,LREF.POS)
    POS.EB.REVIEW = LREF.POS<1,1>
    POS.EB.PROFITLOSS = LREF.POS<1,2>
    POS.EB.TASA.POOL = LREF.POS<1,3>
RETURN

*-------
PROCESS:
*-------
*----------------------------------------------------------------------------------------------------------------------
* This section updates the local reference field PROFITLOSS and the related local reference fields for Deposit products
*----------------------------------------------------------------------------------------------------------------------

    AZ.PRODUCT = R.NEW(AZ.ALL.IN.ONE.PRODUCT)
    R.AZ.PRODUCT.PARAMETER=''
    CALL CACHE.READ(FN.AZ.PRODUCT.PARAMETER,AZ.PRODUCT,R.AZ.PRODUCT.PARAMETER,PARA.ERR)
    IF R.AZ.PRODUCT.PARAMETER<AZ.APP.LOAN.DEPOSIT> NE 'DEPOSIT' THEN
        RETURN
    END
    AZ.CUR = R.NEW(AZ.CURRENCY)
    AZ.CATEG = R.NEW(AZ.CATEGORY)
    GOSUB GET.INTEREST
    R.NEW(AZ.LOCAL.REF)<1,POS.EB.TASA.POOL> = BUY.RATE
    R.NEW(AZ.LOCAL.REF)<1,POS.EB.PROFITLOSS> =  FMT(BUY.RATE-R.NEW(AZ.INTEREST.RATE),"R2#10")
*--------PACS00052350---------------------------
*R.NEW(AZ.LOCAL.REF)<1,POS.EB.REVIEW> = 'YES'
*--------PACS00052350---------------------------
RETURN
*-------
GET.INTEREST:
*-------
    Y.AZ.VALUE.DATE=R.NEW(AZ.VALUE.DATE)
    Y.AZ.MATURITY.DATE=R.NEW(AZ.MATURITY.DATE)
    Y.REGION = ''
    Y.DIFF.DAYS = 'C'
    CALL CDD(Y.REGION,Y.AZ.VALUE.DATE,Y.AZ.MATURITY.DATE,Y.DIFF.DAYS)
    Y.DIFF.DAYS = Y.DIFF.DAYS:'D'
    BUY.RATE = ''
    RATE = ''
    CALL APAP.TAM.redoGetPoolRate(AZ.CUR,Y.DIFF.DAYS,RATE)
    BUY.RATE = RATE<2>
RETURN
END
