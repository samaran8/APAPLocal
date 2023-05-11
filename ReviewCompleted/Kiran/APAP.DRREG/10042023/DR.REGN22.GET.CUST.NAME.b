* @ValidationCode : MjotNDg3MTg2OTUyOkNwMTI1MjoxNjgxMTIyMjU0MTQxOmFqaXRoOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 10 Apr 2023 15:54:14
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ajith
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.DRREG
*
*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*10-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION  FM to @FM
*10-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION NO CHANGE
*----------------------------------------------------------------------------------------




*-----------------------------------------------------------------------------
SUBROUTINE DR.REGN22.GET.CUST.NAME
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_F.SEC.TRADE
*Tus Start
    GOSUB MULTI.GET.LOC.REF.DEFINE
*Tus End
    FN.SEC.TRADE = 'F.SEC.TRADE'
    F.SEC.TRADE = ''
    CALL OPF(FN.SEC.TRADE,F.SEC.TRADE)
    R.SEC.TRADE = ''
*Tus Start
*CALL GET.LOC.REF('SEC.TRADE','L.ST.CPTY',L.ST.CPTY.POS) ;* Tus End
    CALL F.READ(FN.SEC.TRADE,COMI,R.SEC.TRADE,F.SEC.TRADE,SEC.TRADE.ERR)
    BEGIN CASE
        CASE R.SEC.TRADE<SC.SBS.BROKER.TYPE> EQ 'CO'
            CUST.ID = R.SEC.TRADE<SC.SBS.BROKER.NO>
        CASE R.SEC.TRADE<SC.SBS.BROKER.TYPE> EQ 'B'
            CUST.ID = R.SEC.TRADE<SC.SBS.LOCAL.REF,L.ST.CPTY.POS>
    END CASE

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER); *Tus Start
*CALL GET.LOC.REF('CUSTOMER','L.CU.TIPO.CL',TIPO.CL.POS) ;* Tus End
    R.CUSTOMER = ''
    CALL F.READ(FN.CUSTOMER,CUST.ID,R.CUSTOMER,F.CUSTOMER,CUSTOMER.ERR)
    CUSTOMER.NAME = ''

    BEGIN CASE
        CASE R.CUSTOMER<EB.CUS.LOCAL.REF,TIPO.CL.POS> EQ 'PERSONA FISICA' OR R.CUSTOMER<EB.CUS.LOCAL.REF,TIPO.CL.POS> EQ 'CLIENTE MENOR'
            CUSTOMER.NAME = R.CUSTOMER<EB.CUS.GIVEN.NAMES>:' ':R.CUSTOMER<EB.CUS.FAMILY.NAME>
        CASE R.CUSTOMER<EB.CUS.LOCAL.REF,TIPO.CL.POS> EQ 'PERSONA JURIDICA'
            CUSTOMER.NAME = R.CUSTOMER<EB.CUS.NAME.1>:' ':R.CUSTOMER<EB.CUS.NAME.2>
    END CASE

    COMI = CUSTOMER.NAME

RETURN

*Tus Start
*************************
MULTI.GET.LOC.REF.DEFINE:
*************************

    Y.LREF.APP =''
    Y.LREF.FIELD= ''
    Y.LREF.POS = ''
    Y.LREF.APP ='SEC.TRADE':@FM:'CUSTOMER'
    Y.LREF.FIELD ='L.ST.CPTY':@FM:'L.CU.TIPO.CL'
    CALL MULTI.GET.LOC.REF(Y.LREF.APP,Y.LREF.FIELD,Y.LREF.POS)
    L.ST.CPTY.POS=Y.LREF.POS<1,1>
    TIPO.CL.POS=Y.LREF.POS<2,1>
RETURN
*Tus End
END
