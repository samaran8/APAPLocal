* @ValidationCode : MjoxODkwMzA4MTMzOkNwMTI1MjoxNjgwNzY2MDc2NTY2OmFqaXRoOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 06 Apr 2023 12:57:56
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
*-----------------------------------------------------------------------------
SUBROUTINE DR.REG.RIEN3.EXTRACT.LOAD
*-----------------------------------------------------------------------------
* Company Name   : APAP
* Developed By   : gangadhar@temenos.com
* Program Name   : DR.REG.RIEN3.EXTRACT
* Date           : 3-May-2013
*-----------------------------------------------------------------------------
* Description:
*------------
* This multi-thread job is meant for to extact the MM.
*-----------------------------------------------------------------------------
*
* Modification History :
* ----------------------
*   Date          Author              Modification Description
*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*06-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION   NO CHANGE
*06-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION NO CHANGE
*----------------------------------------------------------------------------------------


*
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_BATCH.FILES
    $INSERT I_F.DATES
    $INSERT I_DR.REG.RIEN3.EXTRACT.COMMON
    $INSERT I_F.DR.REG.RIEN3.PARAM

    GOSUB INIT.PROCESS

RETURN

*-----------------------------------------------------------------------------
INIT.PROCESS:
*-----------*

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    FN.MM.MONEY.MARKET = 'F.MM.MONEY.MARKET'
    F.MM.MONEY.MARKET = ''
    CALL OPF(FN.MM.MONEY.MARKET,F.MM.MONEY.MARKET)

    FN.DR.REG.RIEN3.WORKFILE = 'F.DR.REG.RIEN3.WORKFILE'
    F.DR.REG.RIEN3.WORKFILE = ''
    CALL OPF(FN.DR.REG.RIEN3.WORKFILE,F.DR.REG.RIEN3.WORKFILE)

    FN.LIMIT = 'F.LIMIT'
    F.LIMIT = ''
    CALL OPF(FN.LIMIT,F.LIMIT)

    FN.FOREX = 'F.FOREX'
    F.FOREX = ''
    CALL OPF(FN.FOREX,F.FOREX)

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.SEC.TRADE = 'F.SEC.TRADE'
    F.SEC.TRADE = ''
    CALL OPF(FN.SEC.TRADE,F.SEC.TRADE)

    FN.SC.PARAMETER = 'F.SC.PARAMETER'
    F.SC.PARAMETER = ''
    CALL OPF(FN.SC.PARAMETER,F.SC.PARAMETER)

    FN.SECURITY.MASTER = 'F.SECURITY.MASTER'
    F.SECURITY.MASTER = ''
    CALL OPF(FN.SECURITY.MASTER,F.SECURITY.MASTER)

    FN.DR.REG.RIEN3.CONCAT = 'F.DR.REG.RIEN3.CONCAT'
    F.DR.REG.RIEN3.CONCAT = ''
    CALL OPF(FN.DR.REG.RIEN3.CONCAT,F.DR.REG.RIEN3.CONCAT)

    FN.CUSTOMER.ACCOUNT = 'F.CUSTOMER.ACCOUNT'
    F.CUSTOMER.ACCOUNT = ''
    CALL OPF(FN.CUSTOMER.ACCOUNT,F.CUSTOMER.ACCOUNT)

    FN.DR.REG.RIEN3.PARAM = 'F.DR.REG.RIEN3.PARAM'
    F.DR.REG.RIEN3.PARAM = ''
    CALL OPF(FN.DR.REG.RIEN3.PARAM,F.DR.REG.RIEN3.PARAM)

*  CALL F.READ(FN.DR.REG.RIEN3.PARAM,'SYSTEM',R.DR.REG.RIEN3.PARAM,F.DR.REG.RIEN3.PARAM,DR.REG.RIEN3.PARAM.ERR) ;*Tus Start
    CALL CACHE.READ(FN.DR.REG.RIEN3.PARAM,'SYSTEM',R.DR.REG.RIEN3.PARAM,DR.REG.RIEN3.PARAM.ERR) ; * Tus End
    Y.TODAY = R.DATES(EB.DAT.LAST.WORKING.DAY)

    Y.APP = 'CUSTOMER':@FM:'SECURITY.MASTER'
    Y.FLD = 'L.CU.TIPO.CL':@FM:'L.COLL.ISSUE'
    Y.POS = ''
    CALL MULTI.GET.LOC.REF(Y.APP,Y.FLD,Y.POS)
    L.CU.TIPO.CL.POS = Y.POS<1,1>
    L.COLL.ISSUE.POS = Y.POS<2,1>
*
RETURN
*-----------------------------------------------------------------------------
END
