* @ValidationCode : MjoxMDA1MjExNzU0OkNwMTI1MjoxNjgxMTIwMDgzMzkzOkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 10 Apr 2023 15:18:03
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
SUBROUTINE REDO.CH.V.JURCUS
**
* Subroutine Type : VERSION
* Attached to : AA.ARRANGEMENT.ACTIVITY,REDO.CORP.NEWPROXADM,
* AA.ARRANGEMENT.ACTIVITY,REDO.CORP.NEWPROXAUTH,
* AA.ARRANGEMENT.ACTIVITY,REDO.CORP.NEWPROXINP
* Attached as : Field CUSTOMER as VALIDATION.RTN
* Primary Purpose : Validate if Customer is a juridical person.
*-----------------------------------------------------------------------------
* MODIFICATIONS HISTORY
*
* 29/08/12 - First Version
* ODR Reference: ODR-2010-06-0155
* Project: NCD Asociacion Popular de Ahorros y Prestamos (APAP)
* Roberto Mondragon - TAM Latin America
* rmondragon@temenos.com
*
* 10-APR-2023     Conversion tool   R22 Auto conversion   FM TO @FM
* 10-APR-2023      Harishvikram C   Manual R22 conversion No changes
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_F.CUSTOMER.STATUS
    $INSERT I_F.CUSTOMER

    GOSUB INITIALIZE
    GOSUB PROCESS

RETURN

***********
INITIALIZE:
***********

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    FN.CUSTOMER.STATUS = 'F.CUSTOMER.STATUS'
    F.CUSTOMER.STATUS = ''
    CALL OPF(FN.CUSTOMER.STATUS,F.CUSTOMER.STATUS)

    LREF.APP = 'CUSTOMER'
    LREF.FIELDS = 'L.CU.TIPO.CL'
    LREF.POS = ''
    CALL GET.LOC.REF(LREF.APP,LREF.FIELDS,LREF.POS)

    L.CU.TIPO.CL.POS = LREF.POS<1,1>

RETURN

********
PROCESS:
********

    Y.CUSTOMER = COMI

    R.CUSTOMER = ''; CUS.ERR = ''
    CALL F.READ(FN.CUSTOMER,Y.CUSTOMER,R.CUSTOMER,F.CUSTOMER,CUS.ERR)
    IF R.CUSTOMER THEN
        Y.CUS.STATUS = R.CUSTOMER<EB.CUS.CUSTOMER.STATUS>
        Y.CUS.TYPE = R.CUSTOMER<EB.CUS.LOCAL.REF><1,L.CU.TIPO.CL.POS>
        Y.EMAIL.REG = R.CUSTOMER<EB.CUS.EMAIL.1>
    END

    IF Y.CUS.STATUS NE 1 THEN
        GOSUB GET.DESC.STATUS
        ETEXT = 'EB-REDO.CH.V.EMAIL':@FM:Y.CUS.STATUS.DESC
        CALL STORE.END.ERROR
        RETURN
    END

    IF Y.CUS.TYPE NE 'PERSONA JURIDICA' THEN
        ETEXT = 'EB-REDO.CH.V.EMAIL2':@FM:Y.CUS.TYPE
        CALL STORE.END.ERROR
        RETURN
    END

RETURN

****************
GET.DESC.STATUS:
****************

    R.CUSTOMER.STATUS = '' ; CS.ERR = ''
    CALL F.READ(FN.CUSTOMER.STATUS,Y.CUS.STATUS,R.CUSTOMER.STATUS,F.CUSTOMER.STATUS,CS.ERR)
    IF R.CUSTOMER.STATUS THEN
        Y.CUS.STATUS.DESC = R.CUSTOMER.STATUS<EB.CST.SHORT.NAME>
    END

RETURN

END
