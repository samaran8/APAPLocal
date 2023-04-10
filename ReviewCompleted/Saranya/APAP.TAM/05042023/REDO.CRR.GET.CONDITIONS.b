* @ValidationCode : MjotMzY1NTgzNTAyOkNwMTI1MjoxNjgwNjg4MDA0MTAxOklUU1M6LTE6LTE6MDoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 05 Apr 2023 15:16:44
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.CRR.GET.CONDITIONS(ARR.ID,EFF.DATE,PROP.CLASS,PROPERTY,R.Condition,ERR.MSG)
*---------------------------------------------------------------------------
*DESCRIPTION:
*------------
* Routine to extract arrangement condition record
*----------------------------------------------------------------------------
** Input/Output:
*      IN : ARR.ID - (Arrangement ID)
*           EFF.DATE - Effective Date
*           PROP.CLASS - Property Class
*           PROPERTY - Property
*     OUT : R.Condition - Arrangement Condition
*           ERR.MSG - Error message if any, else blank
*----------------------------------------------------------------------------
* Dependencies:
*---------------
* CALLS : -NA-
* CALLED BY : -NA-
*
* Revision History:
*------------------
*  Date                who           Reference                      Description
* 23-Sep-2009     Mohammed Aslam        BPCRA2009090099         Initial Creation
* 05.04.2023       Conversion Tool       R22                    Auto Conversion     - = TO EQ
* 05.04.2023       Shanmugapriya M       R22                    Manual Conversion   - No changes
*
*--------------------------------------------------------------------------
*--------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_AA.APP.COMMON
    $INSERT I_F.AA.PAYMENT.SCHEDULE

    GOSUB INITIALISE
    GOSUB GET.ARRANGEMENT.CONDITION

RETURN
*---------(Main)

INITIALISE:
*----------
    ArrangementID = ARR.ID
    idPropertyClass = PROP.CLASS
    idProperty = PROPERTY
    effectiveDate = EFF.DATE
    returnIds = ''
    returnConditions = ''
    returnError = ''
    returnVal = ''
    returnFreq = ''
    EngFreq = ''
    PayType = ''
    PAYMENT.TYPE = ''
    SHORT.TYPE = ''
    RTN.MSG = ''

    R.Arrangement = ''
    R.Err = ''

    IF effectiveDate EQ '' THEN        ;** R22 Auto conversion - = TO EQ
        effectiveDate = TODAY
    END
RETURN
*---------(Initialise)

* Call AA.GET.ARRANGEMENT.CONDITIONS to get the arrangement condition record
GET.ARRANGEMENT.CONDITION:
*-------------------------

    CALL AA.GET.ARRANGEMENT.CONDITIONS(ArrangementID, idPropertyClass, idProperty, effectiveDate, returnIds, returnConditions, returnError)
    IF returnError THEN
        RTN.MSG = returnError
        RETURN
    END
    R.Condition = RAISE(returnConditions)
RETURN
*---------(GetPaymentType)
END
