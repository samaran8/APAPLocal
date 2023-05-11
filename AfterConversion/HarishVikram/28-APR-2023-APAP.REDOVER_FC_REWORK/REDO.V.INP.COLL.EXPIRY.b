* @ValidationCode : MjoxNDkyNjI3MzY3OkNwMTI1MjoxNjgyNDEyMzQ5Nzc2OkhhcmlzaHZpa3JhbUM6LTE6LTE6MDoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 25 Apr 2023 14:15:49
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.V.INP.COLL.EXPIRY
*----------------------------------------------------------------------------------------------------
* DESCRIPTION : This routine is an input routine attached to below version,
*                COLLATERAL,REDO.REVALUATION
*-----------------------------------------------------------------------------------------------------
* * Input / Output
* --------------
* IN Parameter    : NA
* OUT Parameter   : NA
*-----------------------------------------------------------------------------------------------------
* COMPANY NAME : APAP
* PROGRAM NAME : REDO.V.INP.COLL.EXPIRY
*-----------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
* Date             Author             Reference                   Description
* 20 JAN 2012      JEEVA T          PACS00136836                  Initial Creation
*Modification history
*Date                Who               Reference                  Description
*17-04-2023      conversion tool     R22 Auto code conversion     No changes
*17-04-2023      Mohanraj R          R22 Manual code conversion   No changes
*------------------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.COLLATERAL

    IF R.NEW(COLL.EXPIRY.DATE) NE "" THEN
        GOSUB INIT
        RETURN
    END
RETURN
*-------------------------------------------------------------------------------------------------------
INIT:
*-------------------------------------------------------------------------------------------------------
    IF R.NEW(COLL.EXPIRY.DATE) LT TODAY THEN
        AF = COLL.EXPIRY.DATE
        ETEXT="AZ-MUST.GE.TODAY"
        CALL STORE.END.ERROR
        RETURN
    END
    Y.STATUS.OLD = R.OLD(COLL.STATUS)
    Y.STATUS.NEW = R.NEW(COLL.STATUS)
    IF R.OLD(COLL.STATUS) NE 'LIQ' THEN
        AF = COLL.EXPIRY.DATE
        ETEXT="AC-INP.NOT.ALLOWED"
        CALL STORE.END.ERROR
        RETURN
    END

RETURN
END
