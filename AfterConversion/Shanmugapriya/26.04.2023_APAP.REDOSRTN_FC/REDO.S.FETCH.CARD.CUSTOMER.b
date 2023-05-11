* @ValidationCode : MjoxMTEzNjUxMTQzOkNwMTI1MjoxNjgyNDE1MTQzMjc0OklUU1M6LTE6LTE6MDoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 25 Apr 2023 15:02:23
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
$PACKAGE APAP.REDOSRTN
SUBROUTINE REDO.S.FETCH.CARD.CUSTOMER(IN.PARAM)
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :KAVITHA
*Program   Name    :REDO.S.FETCH.CARD.CUSTOMER
*-------------------------------------------------------------------------------

*DESCRIPTION       :This subroutine is used to get the value of system time and will update the deal slip DCARD.RECEIPT
*
* ----------------------------------------------------------------------------------
*------------------------------------------------------------------------------------------
* Revision History
*-------------------------
*    Date             Who               Reference       Description
* 01-JUL-2011        KAVITHA            PACS00062260    Initial creation
*Modification history
*Date                Who               Reference                  Description
*10-04-2023      conversion tool     R22 Auto code conversion     No changes
*10-04-2023      Mohanraj R          R22 Manual code conversion   No changes

*----------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_F.LATAM.CARD.ORDER

    FN.CUST='F.CUSTOMER'
    F.CUST=''
    CALL OPF(FN.CUST,F.CUST)

    Y.CUST.ID = R.NEW(CARD.IS.CUSTOMER.NO)<1,1>
    CALL F.READ(FN.CUST,Y.CUST.ID,R.CUSTOMER,F.CUST,CUS.ERROR)
    IF R.CUSTOMER THEN
        IN.PARAM = R.CUSTOMER<EB.CUS.NAME.1>
    END


RETURN
END
