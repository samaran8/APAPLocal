* @ValidationCode : Mjo1NjY4Mjk3NDk6Q3AxMjUyOjE2ODI1MDI2MTk0OTY6SVRTUzotMTotMTo1Mjc6MTpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 26 Apr 2023 15:20:19
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 527
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOAPAP
SUBROUTINE REDO.APAP.ACCT.NAME
*********************************************************************************************************
*Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Developed By      : Temenos Application Management
*Program   Name    : REDO.APAP.ACCT.NAME
*--------------------------------------------------------------------------------------------------------
*Description       : This is a Conversion routine to get the names of the Customer
*
*Linked With       : Enquiry REDO.PAY.ENQ.RPT
*In  Parameter     : O.DATA
*Out Parameter     : O.DATA
*Files  Used       : ACCOUNT                    As              I               Mode
*
*--------------------------------------------------------------------------------------------------------
*Modification Details:
*=====================
*      Date                 Who                  Reference                 Description
*     ------               -----               -------------              -------------
*     17.06.2011          Dhamu S              ODR-2010-03-0093            Initial Creation
* Date                   who                   Reference              
* 04-04-2023         CONVERSTION TOOL     R22 AUTO CONVERSTION VM TO @VM AND FM TO @FM 
* 04-04-2023          ANIL KUMAR B        R22 MANUAL CONVERSTION -NO CHANGES
*
*********************************************************************************************************
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CUSTOMER
    $INSERT I_F.RELATION
*-------------------------------------------------------------------------------------------------------
**********
MAIN.PARA:
**********
* This is the para from where the execution of the code starts
    GOSUB OPEN.PARA

    GOSUB PROCESS.PARA

RETURN
*--------------------------------------------------------------------------------------------------------
**********
OPEN.PARA:
**********
* In this para of the code, file variables are initialised and opened

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    FN.RELATION = 'F.RELATION'
    F.RELATION = ''
    CALL OPF(FN.RELATION,F.RELATION)

RETURN
*--------------------------------------------------------------------------------------------------------
*************
PROCESS.PARA:
*************

    Y.CUS.NAMES = ''
    GOSUB FIND.MULTI.LOCAL.REF
    Y.AC.ID = O.DATA
    O.DATA = ''
    GOSUB READ.ACCOUNT
    Y.RELATION.COUNT = DCOUNT(R.ACCOUNT<AC.RELATION.CODE>,@VM)
    Y.COUNT = 1
    CUSTOMER.ID = R.ACCOUNT<AC.CUSTOMER>
    IF CUSTOMER.ID EQ '' THEN
        O.DATA = R.ACCOUNT<AC.ACCOUNT.TITLE.1>
        RETURN
    END
    GOSUB READ.CUSTOMER
    GOSUB GET.CUS.NAMES
    MAIN.NAMES = Y.CUS.NAME
    CUSTOMER.ID = ''
    Y.CUS.NAME = ''
    LOOP
    WHILE Y.COUNT LE Y.RELATION.COUNT
        Y.REL.DESC = ''
        Y.CUS.NAME = ''
        RELATION.ID = R.ACCOUNT<AC.RELATION.CODE,Y.COUNT>
        IF RELATION.ID GE 500 AND RELATION.ID LE 529 THEN
            GOSUB GET.VALUES
        END
        Y.COUNT += 1
        Y.CUS.NAMES<-1> = Y.REL.DESC:' ':Y.CUS.NAME
        CHANGE @FM TO ' ' IN Y.CUS.NAMES
    REPEAT
    O.DATA = MAIN.NAMES:' ':Y.CUS.NAMES
RETURN
***********
GET.VALUES:
***********
    GOSUB READ.RELATION
    Y.REL.DESC  = R.RELATION<EB.REL.DESCRIPTION>
    CUSTOMER.ID = R.ACCOUNT<AC.JOINT.HOLDER,Y.COUNT>
    GOSUB READ.CUSTOMER
    GOSUB GET.CUS.NAMES
RETURN

GET.CUS.NAMES:
**************
    IF R.CUSTOMER<EB.CUS.LOCAL.REF,LOC.L.CU.TIPO.CL.POS> EQ 'PERSONA FISICA' OR R.CUSTOMER<EB.CUS.LOCAL.REF,LOC.L.CU.TIPO.CL.POS> EQ 'CLIENTE MENOR' THEN
        Y.CUS.NAME = R.CUSTOMER<EB.CUS.GIVEN.NAMES>:' ':R.CUSTOMER<EB.CUS.FAMILY.NAME>
    END

    IF R.CUSTOMER<EB.CUS.LOCAL.REF,LOC.L.CU.TIPO.CL.POS> EQ 'PERSONA JURIDICA' THEN
        Y.CUS.NAME = R.CUSTOMER<EB.CUS.NAME.1,1>:' ':R.CUSTOMER<EB.CUS.NAME.2,1>
    END

RETURN
*--------------------------------------------------------------------------------------------------------
*************
READ.ACCOUNT:
*************
* In this para of the code, file ACCOUNT is read
    R.ACCOUNT  = ''
    ACCOUNT.ER = ''
    CALL F.READ(FN.ACCOUNT,Y.AC.ID,R.ACCOUNT,F.ACCOUNT,ACCOUNT.ER)

RETURN
*--------------------------------------------------------------------------------------------------------
**************
READ.CUSTOMER:
**************
* In this para of the code, file CUSTOMER is read
    R.CUSTOMER  = ''
    CUSTOMER.ER = ''
    CALL F.READ(FN.CUSTOMER,CUSTOMER.ID,R.CUSTOMER,F.CUSTOMER,CUSTOMER.ER)

RETURN
*--------------------------------------------------------------------------------------------------------
**************
READ.RELATION:
**************
* In this para of the code, file RELATION is read
    R.RELATION  = ''
    RELATION.ER = ''
    CALL F.READ(FN.RELATION,RELATION.ID,R.RELATION,F.RELATION,RELATION.ER)

RETURN
*--------------------------------------------------------------------------------------------------------

*********************
FIND.MULTI.LOCAL.REF:
*********************
* In this para of the code, local reference field positions are obtained
    APPL.ARRAY = 'CUSTOMER'
    FLD.ARRAY  = 'L.CU.TIPO.CL'
    FLD.POS    = ''

    CALL MULTI.GET.LOC.REF(APPL.ARRAY,FLD.ARRAY,FLD.POS)

    LOC.L.CU.TIPO.CL.POS    = FLD.POS<1,1>

RETURN
*--------------------------------------------------------------------------------------------------------
END       ;* End of Prgram
