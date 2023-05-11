* @ValidationCode : MjotNjcyNjk5MzI6Q3AxMjUyOjE2ODI1MDI2NzU0ODQ6SVRTUzotMTotMTozNzk6MTpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 26 Apr 2023 15:21:15
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 379
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOAPAP
SUBROUTINE REDO.APAP.V.VAL.CUS.DET
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :PRABHU.N
*Program   Name    :REDO.APAP.V.VAL.CUS.DET
*---------------------------------------------------------------------------------

*DESCRIPTION       :It is attached as VALIDATION routine in ADD APP BENEFICIARY
*
*LINKED WITH       :

* ----------------------------------------------------------------------------------
*Modification Details:
*=====================
*   Date               who           Reference            Description
* 16-NOV-2010        Prabhu.N       ODR-2010-08-0031   Initial Creation
*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*19-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION  FM to @FM , VM to @VM
*19-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION NO CHANGE
*----------------------------------------------------------------------------------------
*-------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_F.ACCOUNT
    $INSERT I_F.BENEFICIARY
    IF NOT(COMI) THEN
        RETURN
    END
    IF VAL.TEXT THEN
        RETURN
    END

    GOSUB OPEN.PARA
    GOSUB PROCESS.PARA

RETURN
*---------
OPEN.PARA:
*---------
    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT  = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER  = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

RETURN

*------------
PROCESS.PARA:
*------------
    ACCOUNT.ID = COMI
    R.ACCOUNT  = ''
    ACCOUNT.ER = ''
    CALL F.READ(FN.ACCOUNT,ACCOUNT.ID,R.ACCOUNT,F.ACCOUNT,ACCOUNT.ER)

    CUSTOMER.ID = R.ACCOUNT<AC.CUSTOMER>

    R.CUSTOMER  = ''
    CUSTOMER.ER = ''
    CALL F.READ(FN.CUSTOMER,CUSTOMER.ID,R.CUSTOMER,F.CUSTOMER,CUSTOMER.ER)

    GOSUB FIND.MULTI.GET.LOC.REF

    IF R.CUSTOMER<EB.CUS.LOCAL.REF,LOC.L.CU.TIPO.CL.POS> EQ "PERSONA FISICA" OR R.CUSTOMER<EB.CUS.LOCAL.REF,LOC.L.CU.TIPO.CL.POS> EQ  "CLIENTE MENOR" THEN
        Y.CUS.NAMES = R.CUSTOMER<EB.CUS.GIVEN.NAMES>:" ":R.CUSTOMER<EB.CUS.FAMILY.NAME>
    END

    IF R.CUSTOMER<EB.CUS.LOCAL.REF,LOC.L.CU.TIPO.CL.POS> EQ "PERSONA JURIDICA" THEN
        Y.CUS.NAMES = R.CUSTOMER<EB.CUS.NAME.1,1>:" ":R.CUSTOMER<EB.CUS.NAME.2,1>
    END

    IF NOT(R.CUSTOMER<EB.CUS.LOCAL.REF,LOC.L.CU.TIPO.CL.POS>) THEN
        Y.CUS.NAMES = R.CUSTOMER<EB.CUS.SHORT.NAME>
    END

    R.NEW(ARC.BEN.LOCAL.REF)<1,LOC.L.BEN.CUST.NAME.POS> = Y.CUS.NAMES
    R.NEW(ARC.BEN.NICKNAME)                             = R.CUSTOMER<EB.CUS.MNEMONIC>
    R.NEW(ARC.BEN.LOCAL.REF)<1,LOC.L.BEN.CEDULA.POS>    = R.CUSTOMER<EB.CUS.LOCAL.REF,LOC.L.CU.CIDENT.POS>
    R.NEW(ARC.BEN.LOCAL.REF)<1,LOC.L.BEN.EMAIL.POS>     = R.CUSTOMER<EB.CUS.EMAIL.1,1>

RETURN
*---------------------
FIND.MULTI.GET.LOC.REF:
*---------------------
    APPLN.ARR    = 'BENEFICIARY':@FM:'CUSTOMER'
    APPL.FLD.ARR = 'L.BEN.CUST.NAME':@VM:'L.BEN.CEDULA':@VM:'L.BEN.EMAIL':@VM:'L.BEN.PROD.TYPE':@FM:'L.CU.TIPO.CL':@VM:'L.CU.CIDENT'
    FLD.POS      = ''

    CALL MULTI.GET.LOC.REF(APPLN.ARR,APPL.FLD.ARR,FLD.POS)
    LOC.L.BEN.CUST.NAME.POS = FLD.POS<1,1>
    LOC.L.BEN.CEDULA.POS    = FLD.POS<1,2>
    LOC.L.BEN.EMAIL.POS     = FLD.POS<1,3>
    LOC.L.BEN.PROD.TYPE.POS = FLD.POS<1,4>
    LOC.L.CU.TIPO.CL.POS    = FLD.POS<2,1>
    LOC.L.CU.CIDENT.POS     = FLD.POS<2,2>

RETURN
END
