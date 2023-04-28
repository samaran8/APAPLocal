* @ValidationCode : MjotNDMzMjEzNDc5OkNwMTI1MjoxNjgyMDY4NzUwNjk2OklUU1M6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 21 Apr 2023 14:49:10
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
SUBROUTINE REDO.CRE.ARR.AUTH.GET.LIM.COLL
*----------------------------------------------------------------------------------------------------
* DESCRIPTION :
*              This is related with REDO.CREATE.ARRANGEMENT.AUTHORISE routine
*              In this code we assign the SEQ values for creating LIMIT and COLLATERAL
*-----------------------------------------------------------------------------------------------------
* Input / Output
* --------------
* IN Parameter    : NA
* OUT Parameter   :
*                     E          is equals to "" then everything OK
*                     R.NEW      information for creating the LIMIT
*-----------------------------------------------------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : hpasquel@temenos.com
* PROGRAM NAME : REDO.CREATE.ARRANGEMENT.VALIDATE
*-----------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
* Date             Author             Reference         Description
* 05-Jan-2011    Paul Pasquel      ODR-2009-11-0199    Initial creation
** 21-04-2023 R22 Auto Conversion
** 21-04-2023 Skanda R22 Manual Conversion - No changes
*------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.REDO.CREATE.ARRANGEMENT

    GOSUB PROCESS

RETURN
*------------------------------------------------------------------------------------------------------
PROCESS:
*------------------------------------------------------------------------------------------------------
    IF R.NEW(REDO.CR.LIMIT) NE '' THEN
        CALL OCOMO("LIMIT WAS ALREADY ASSOCIATED, THEN OMMIT THIS PROCESS")
        RETURN
    END
* Get Limit Product for the given category code
    YPRODUCT = ''
    E = ""
    GOSUB GET.LIMIT.PRODUCT
    IF E NE "" THEN
        RETURN
    END
* Set Limit information
    R.NEW(REDO.CR.LIMIT) = YPRODUCT
* Collateral must be created ?
    IF R.NEW(REDO.CR.COLL.CODE) NE '' THEN
        R.NEW(REDO.CR.COLL.RIGHT.ID) = R.NEW(REDO.CR.CUSTOMER) : "." : P.LAST.COLL.ID
        R.NEW(REDO.CR.SEC.NO) = R.NEW(REDO.CR.COLL.RIGHT.ID) : ".1"
    END ELSE
        R.NEW(REDO.CR.COLL.RIGHT.ID) = ''
        R.NEW(REDO.CR.SEC.NO) = ''
    END
    R.NEW(REDO.CR.APPRVL.DATE) = TODAY
    R.NEW(REDO.CR.OFFRD.UNTIL) = TODAY
    Y.MAT.DATE = R.NEW(REDO.CR.TERM)
    CALL CALENDAR.DAY(R.NEW(REDO.CR.EFFECT.DATE),'+',Y.MAT.DATE)
    R.NEW(REDO.CR.LIM.EXP.DATE) = Y.MAT.DATE
    R.NEW(REDO.CR.NOTES) = "CREADO POR FABRICA DE CREDITO"
    R.NEW(REDO.CR.INTRNL.AMT) = R.NEW(REDO.CR.AMOUNT)
    R.NEW(REDO.CR.MAX.TOTAL) = R.NEW(REDO.CR.INTRNL.AMT)
    R.NEW(REDO.CR.AVAIL.MKR) = "Y"
*    R.NEW(REDO.CR.COLL.CODE) = R.NEW(REDO.CR.TYPE.OF.SEC)<1,1>
RETURN

*------------------------------------------------------------------------------------------------------
GET.LIMIT.PRODUCT:
*------------------------------------------------------------------------------------------------------
* Backup COMMON variables
*
* We could not use LIMIT.GET.PRODUCT because it launchs an error messagen when more than 1 limite is applicable for the contract
*
*    DIM SAVE.R.NEW(C$SYSDIM)
*    Y.OLD.APPLICATION = APPLICATION
*    MAT SAVE.R.NEW = MAT R.NEW
*    Y.DUMMY = ''
*    MATPARSE R.NEW FROM Y.DUMMY         ;* Clean all the dim array
*    APPLICATION = "ACCOUNT"
*    R.NEW(AC.CATEGORY) = SAVE.R.NEW(REDO.CR.CATEGORY)

* Call to get Limit Product
    YPRODUCT = ""
    YR.SYSTEM = ""
    E = ''
*    CALL LIMIT.GET.PRODUCT (YR.SYSTEM, SAVE.R.NEW(REDO.CR.CUSTOMER), SAVE.R.NEW(REDO.CR.LOAN.CURRENCY), YPRODUCT)
    CALL APAP.TAM.REDO.R.GET.LIMIT.PRODUCT("ACCOUNT", R.NEW(REDO.CR.CATEGORY), YPRODUCT) ;*R22 manual conversion
* Restore COMMON variables
*    MAT R.NEW = MAT SAVE.R.NEW
*    APPLICATION =Y.OLD.APPLICATION

* After restoring, check if LIMIT.GET.PRODUCT found an error
    IF E NE '' THEN
        AF = REDO.CR.LIMIT
        ETEXT = E
        CALL STORE.END.ERROR
        CALL OCOMO("ERROR TRYING TO GET PRODUCT.LIMIT FOR THE CURRENT CONTRACT")
        RETURN
    END
* Get the next sequence to use for creating the LIMIT
*    YPRODUCT  = YPRODUCT[".",1,1]
    P.CUSTOMER.ID = R.NEW(REDO.CR.CUSTOMER)
    P.LIMIT.REF = YPRODUCT
    P.ACTION = 'R'
    P.LAST.ID = ''
    P.LAST.COLL.ID = ''
    CALL REDO.R.CRE.ARR.LIMIT.SEQ.UPD(P.CUSTOMER.ID, P.LIMIT.REF, P.ACTION, P.LAST.ID, P.LAST.COLL.ID)
*
    P.LAST.ID += 1 ;* R22 Auto conversion
    P.LAST.ID = FMT(P.LAST.ID,"R%2")
    YPRODUCT  = YPRODUCT : "." : P.LAST.ID
* Calculate the next Collateral.Id
    P.LAST.COLL.ID += 1 ;* R22 Auto conversion

RETURN
*------------------------------------------------------------------------------------------------------
END
