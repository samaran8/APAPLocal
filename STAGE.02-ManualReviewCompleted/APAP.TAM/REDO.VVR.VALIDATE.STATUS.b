* @ValidationCode : MjoxMTE4NzgzNjMzOkNwMTI1MjoxNjgxMTkzOTM0MzU5OklUU1M6LTE6LTE6MDoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 11 Apr 2023 11:48:54
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
SUBROUTINE REDO.VVR.VALIDATE.STATUS
*-----------------------------------------------------------------------------
*DESCRIPTION:
*------------
*This routine is attached with VERSION.CONTROL of CUSTOMER.It will assign CUSTOMER.STATUS to active
*at the creation of customer record.It will also generate error message if you change the customer
*status to CLOSED when customer has active product.
*------------------------------------------------------------------------------------------
* Input/Output:
*--------------
* IN  : -NA-
* OUT : -NA-
*
* Dependencies:
*---------------
* CALLS : -NA-
* CALLED BY : -NA-
*
* Revision History:
*------------------
*   Date               who           Reference            Description
* 27-DEC-2009        Prabhu.N       ODR-2009-10-0535     Initial Creation
* 11.04.2023       Conversion Tool       R22            Auto Conversion     - FM TO @FM, VM TO @VM
* 11.04.2023       Shanmugapriya M       R22            Manual Conversion   - No changes
*
*------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_F.REDO.CUST.PRD.LIST
    GOSUB INIT
    GOSUB UPD.STATUS
RETURN
*----
INIT:
*----
    FN.CUST.PRD.LIST='F.REDO.CUST.PRD.LIST'
    F.CUST.PRD.LIST=''
    CALL OPF(FN.CUST.PRD.LIST,F.CUST.PRD.LIST)
RETURN
*----------
UPD.STATUS:
*----------
    IF R.NEW(EB.CUS.CURR.NO) EQ '' THEN
        R.NEW(EB.CUS.CUSTOMER.STATUS)='1'
    END
    IF COMI EQ '4' THEN
        CALL F.READ(FN.CUST.PRD.LIST,ID.NEW,R.CUST.PRD.LIST,F.CUST.PRD.LIST,CUS.ERR)
        Y.PRD.STATUS.LIST=R.CUST.PRD.LIST<PRD.PRD.STATUS>
        CHANGE @VM TO @FM IN Y.PRD.STATUS.LIST
        LOCATE 'ACTIVE' IN Y.PRD.STATUS.LIST SETTING POS THEN
            ETEXT="EB-REDO.ACTIVE.PRD.FOUND"
            CALL STORE.END.ERROR
        END
    END
RETURN
END
