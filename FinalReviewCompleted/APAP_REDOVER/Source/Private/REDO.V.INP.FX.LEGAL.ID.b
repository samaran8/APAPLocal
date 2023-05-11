* @ValidationCode : MjoxOTU2MDA4MjEwOkNwMTI1MjoxNjgxMjgyNTYyNTQ5OnNhbWFyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 12 Apr 2023 12:26:02
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : samar
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.V.INP.FX.LEGAL.ID
*-------------------------------------------------------------------------------------------------------------------------------------------------------------------
* Company Name  : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By  : Pradeep S
* Program Name  : REDO.V.INP.FX.LEGAL.ID
*--------------------------------------------------------------------------------------------------------------------------------------------------------------------
* Description   : This input routine attached to Versions FOREX,REDO.APAP.SPOTDEAL & FOREX,REDO.APAP.FORWARDDEAL to default the local field L.FX.LEGAL.ID
*                 of the based on CIDENT/RTC/PASSPORT
* In parameter  : None
* out parameter : None
*--------------------------------------------------------------------------------------------------------------------------------------------------------------------
* Modificataion History:
*-------------------------
* Date             Author             Reference                         Description
*12-04-2023      Conversion Tool      R22 Auto Code conversion       FM TO @FM VM TO @VM
*12-04-2023       Samaran T           R22 Manual Code conversion          No Changes

*--------------------------------------------------------------------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FOREX
    $INSERT I_F.CUSTOMER

    GOSUB OPEN.FILES
    GOSUB GET.LOCAL.REFS
    GOSUB PROCESS

RETURN

*-----------
OPEN.FILES:
*-----------

    FN.CUST = 'F.CUSTOMER'
    F.CUST = ''
    CALL OPF(FN.CUST,F.CUST)

    Y.FX.LEGAL.ID = ''

RETURN

*---------------
GET.LOCAL.REFS:
*---------------
    Y.APPL = "CUSTOMER":@FM:"FOREX"
    Y.LOCAL.FLD = 'L.CU.CIDENT':@VM:'L.CU.RNC':@FM:'L.FX.LEGAL.ID'
    CALL MULTI.GET.LOC.REF(Y.APPL,Y.LOCAL.FLD,Y.FLD.POS)

    Y.L.CU.CIDENT.POS = Y.FLD.POS<1,1>
    Y.L.CU.RNC.POS = Y.FLD.POS<1,2>
    Y.L.FX.LEGAL.ID.POS = Y.FLD.POS<2,1>

RETURN

*--------
PROCESS:
*--------

    Y.CUS.ID = R.NEW(FX.COUNTERPARTY)
    R.CUS = ''
    CALL F.READ(FN.CUST,Y.CUS.ID,R.CUS,F.CUST,CUS.ERR)

    IF R.CUS THEN


        Y.L.CU.CIDENT = R.CUS<EB.CUS.LOCAL.REF,Y.L.CU.CIDENT.POS>
        Y.L.CU.RNC = R.CUS<EB.CUS.LOCAL.REF,Y.L.CU.RNC.POS>
        Y.CUS.LEGAL.ID = R.CUS<EB.CUS.LEGAL.ID>
        Y.CUS.NAME = R.CUS<EB.CUS.NAME.1>
        GOSUB SUB.PROCESS

        R.NEW(FX.LOCAL.REF)<1,Y.L.FX.LEGAL.ID.POS>  = Y.FX.LEGAL.ID

    END


RETURN

*-----------
SUB.PROCESS:
*------------

    BEGIN CASE

        CASE Y.L.CU.RNC NE ''

            Y.FX.LEGAL.ID = "RNC.":Y.L.CU.RNC:".":Y.CUS.NAME

        CASE Y.L.CU.CIDENT NE ''

            Y.FX.LEGAL.ID = "CIDENT.":Y.L.CU.CIDENT:".":Y.CUS.NAME

        CASE Y.CUS.LEGAL.ID NE ''

            Y.FX.LEGAL.ID = "PASSPORT.":Y.CUS.LEGAL.ID:".":Y.CUS.NAME

    END CASE

RETURN

END
