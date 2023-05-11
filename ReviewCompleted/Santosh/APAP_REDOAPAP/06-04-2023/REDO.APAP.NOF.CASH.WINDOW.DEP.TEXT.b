* @ValidationCode : MjotMTEwNzU3ODMwMjpDcDEyNTI6MTY4MDc1NzAxMjY4MTpJVFNTOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 06 Apr 2023 10:26:52
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOAPAP
SUBROUTINE REDO.APAP.NOF.CASH.WINDOW.DEP.TEXT(Y.CCY.LIST,Y.COMPANY.LIST,Y.FINAL.ARRAY)
*********************************************************************************************************
*Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Developed By      : Temenos Application Management
*Program   Name    : REDO.APAP.NOF.CASH.WINDOW.DEP.TEXT
*--------------------------------------------------------------------------------------------------------
*Description       : REDO.APAP.NOF.CASH.WINDOW.DEP.TEXT is  routine called from another routine
*                    REDO.APAP.NOF.CASH.WINDOW.DEP, this routine is used to amend the report with static text for
*                    display of DEPOSITS
*Linked With       : Enquiry - REDO.APAP.ENQ.CASH.WINDOW.DEP
*In  Parameter     : Y.CCY.LIST    - The variable which holds all the curiences processed
*                    Y.COMP.COUNT  - The variblle holds the umber of third party payment companies
*                    Y.FINAL.ARRAY - The array holding the values to be displayed
*Out Parameter     : Y.FINAL.ARRAY - The array holding the amended values to be displayed
*Files  Used       : NA
*--------------------------------------------------------------------------------------------------------
*Modification Details:
*=====================
*    Date               Who                         Reference                 Description
*   ------             -----                       -------------             -------------
* 11 Mar 2011       Shiva Prasad Y              ODR-2010-03-0086 35         Initial Creation
* 16 Sep 2011       Pradeep S                   PACS00106559                Loan ammendments
* Date                   who                   Reference
* 06-04-2023         CONVERSTION TOOL     R22 AUTO CONVERSTION FM TO @FM VM TO @VM
* 06-04-2023          ANIL KUMAR B        R22 MANUAL CONVERSTION -NO CHANGES
*********************************************************************************************************
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
*-------------------------------------------------------------------------------------------------------
**********
MAIN.PARA:
**********
* This is the para from where the execution of the code starts
    GOSUB PROCESS.PARA

RETURN
*--------------------------------------------------------------------------------------------------------
*************
PROCESS.PARA:
*************
* This is the main processing para

    GOSUB AMEND.ARRAY

RETURN
*--------------------------------------------------------------------------------------------------------
************
AMEND.ARRAY:
************
* In this para of the code, the static text is added in the report for display

    Y.B.LIST    = 3:@FM:4:@FM:5:@FM:6
    Y.B.COUNT   = DCOUNT(Y.B.LIST,@FM)

    Y.REC.COUNT = DCOUNT(Y.FINAL.ARRAY,@FM)
    Y.REC.START = 1

    LOOP
    WHILE Y.REC.START LE Y.REC.COUNT
        GOSUB DEPOSITS
        GOSUB TERM.INST.OPEN
        GOSUB LOAN.PAYMENTS
        GOSUB CREDIT.CARD.PAYMENTS
        GOSUB THIRDPARTY.PAYMENTS
        GOSUB OTHER.INCOMES
        GOSUB CASH.TRANSFERS
        GOSUB BUY.CURRENCY
        GOSUB GET.TOTAL.INC
        GOSUB OVERAGE
        Y.REC.START += 1
    REPEAT

RETURN
*--------------------------------------------------------------------------------------------------------
*********
DEPOSITS:
*********
    Y.FINAL.ARRAY<Y.REC.START,1,1>   = Y.CCY.LIST<Y.REC.START>
* Y.FINAL.ARRAY<Y.REC.START,1,2>   = 'DEPOSITOS'
    Y.FINAL.ARRAY<Y.REC.START,1,3>   = ''
    Y.FINAL.ARRAY<Y.REC.START,1,4>   = ''
    Y.FINAL.ARRAY<Y.REC.START,1,5>   = ''
    Y.FINAL.ARRAY<Y.REC.START,1,6>   = ''
    Y.FINAL.ARRAY<Y.REC.START,1,7>   = ''
    Y.FINAL.ARRAY<Y.REC.START,1,8>   = 'DEPOSITOS'

    Y.FINAL.ARRAY<Y.REC.START,2,1>   = Y.CCY.LIST<Y.REC.START>
    Y.FINAL.ARRAY<Y.REC.START,2,2>   = 'AHORROS'
    Y.FINAL.ARRAY<Y.REC.START,2,7>   = Y.FINAL.ARRAY<Y.REC.START,2,4>  + Y.FINAL.ARRAY<Y.REC.START,2,5>  + Y.FINAL.ARRAY<Y.REC.START,2,6>

    Y.A = 2
    GOSUB CHECK.FOR.VALUE

    Y.FINAL.ARRAY<Y.REC.START,3,1>   = Y.CCY.LIST<Y.REC.START>
    Y.FINAL.ARRAY<Y.REC.START,3,2>   = 'CORRIENTES'
    Y.FINAL.ARRAY<Y.REC.START,3,7>   = Y.FINAL.ARRAY<Y.REC.START,3,4>  + Y.FINAL.ARRAY<Y.REC.START,3,5>  + Y.FINAL.ARRAY<Y.REC.START,3,6>

    Y.A = 3
    GOSUB CHECK.FOR.VALUE

    Y.FINAL.ARRAY<Y.REC.START,4,1>   = Y.CCY.LIST<Y.REC.START>
* Y.FINAL.ARRAY<Y.REC.START,4,2>   = 'TOTAL DEPS. CTAS.'
    Y.FINAL.ARRAY<Y.REC.START,4,9>   = 'TOTAL DEPS. CTAS.'
    Y.FINAL.ARRAY<Y.REC.START,4,3>   = Y.FINAL.ARRAY<Y.REC.START,2,3> + Y.FINAL.ARRAY<Y.REC.START,3,3>
    Y.FINAL.ARRAY<Y.REC.START,4,7>   = Y.FINAL.ARRAY<Y.REC.START,2,7>  + Y.FINAL.ARRAY<Y.REC.START,3,7>

RETURN
*--------------------------------------------------------------------------------------------------------
***************
TERM.INST.OPEN:
***************
    Y.FINAL.ARRAY<Y.REC.START,5,1>   = Y.CCY.LIST<Y.REC.START>
* Y.FINAL.ARRAY<Y.REC.START,5,2>   = 'APERTURA INVERSIONES'
    Y.FINAL.ARRAY<Y.REC.START,5,8>   = 'APERTURA INVERSIONES'
    Y.FINAL.ARRAY<Y.REC.START,5,3>   = ''
    Y.FINAL.ARRAY<Y.REC.START,5,4>   = ''
    Y.FINAL.ARRAY<Y.REC.START,5,5>   = ''
    Y.FINAL.ARRAY<Y.REC.START,5,6>   = ''
    Y.FINAL.ARRAY<Y.REC.START,5,7>   = ''

    Y.FINAL.ARRAY<Y.REC.START,6,1>   = Y.CCY.LIST<Y.REC.START>
    Y.FINAL.ARRAY<Y.REC.START,6,2>   = 'CERTIF. FINANCIEROS'
    Y.FINAL.ARRAY<Y.REC.START,6,7>   = Y.FINAL.ARRAY<Y.REC.START,6,4>  + Y.FINAL.ARRAY<Y.REC.START,6,5>  + Y.FINAL.ARRAY<Y.REC.START,6,6>

    Y.A = 6
    GOSUB CHECK.FOR.VALUE

    Y.FINAL.ARRAY<Y.REC.START,7,1>   = Y.CCY.LIST<Y.REC.START>
    Y.FINAL.ARRAY<Y.REC.START,7,2>   = 'DEP.  PLAZO FIJO'
    Y.FINAL.ARRAY<Y.REC.START,7,7>   = Y.FINAL.ARRAY<Y.REC.START,7,4>  + Y.FINAL.ARRAY<Y.REC.START,7,5>  + Y.FINAL.ARRAY<Y.REC.START,7,6>

    Y.A = 7
    GOSUB CHECK.FOR.VALUE

    Y.FINAL.ARRAY<Y.REC.START,8,1>   = Y.CCY.LIST<Y.REC.START>
    Y.FINAL.ARRAY<Y.REC.START,8,2>   = 'CPH'
    Y.FINAL.ARRAY<Y.REC.START,8,7>   = Y.FINAL.ARRAY<Y.REC.START,8,4>  + Y.FINAL.ARRAY<Y.REC.START,8,5>  + Y.FINAL.ARRAY<Y.REC.START,8,6>

    Y.A = 8
    GOSUB CHECK.FOR.VALUE

    Y.FINAL.ARRAY<Y.REC.START,9,1>  = Y.CCY.LIST<Y.REC.START>
* Y.FINAL.ARRAY<Y.REC.START,9,2>  = 'TOTAL APERT. INV,'
    Y.FINAL.ARRAY<Y.REC.START,9,9>  = 'TOTAL APERT. INV,'
    Y.FINAL.ARRAY<Y.REC.START,9,3>  = Y.FINAL.ARRAY<Y.REC.START,6,3> + Y.FINAL.ARRAY<Y.REC.START,7,3> + Y.FINAL.ARRAY<Y.REC.START,8,3>
    Y.FINAL.ARRAY<Y.REC.START,9,7>  = Y.FINAL.ARRAY<Y.REC.START,6,7>  + Y.FINAL.ARRAY<Y.REC.START,7,7>  + Y.FINAL.ARRAY<Y.REC.START,8,7>

RETURN
*--------------------------------------------------------------------------------------------------------
**************
LOAN.PAYMENTS:
**************
    Y.FINAL.ARRAY<Y.REC.START,10,1>  = Y.CCY.LIST<Y.REC.START>
*    Y.FINAL.ARRAY<Y.REC.START,10,2>  = 'PAGOS PRESTAMOS'
    Y.FINAL.ARRAY<Y.REC.START,10,8>  = 'PAGOS PRESTAMOS'
    Y.FINAL.ARRAY<Y.REC.START,10,3>  = ''
    Y.FINAL.ARRAY<Y.REC.START,10,4>  = ''
    Y.FINAL.ARRAY<Y.REC.START,10,5>  = ''
    Y.FINAL.ARRAY<Y.REC.START,10,6>  = ''
    Y.FINAL.ARRAY<Y.REC.START,10,7>  = ''

    Y.FINAL.ARRAY<Y.REC.START,11,1>  = Y.CCY.LIST<Y.REC.START>
    Y.FINAL.ARRAY<Y.REC.START,11,2>  = 'HIPOTECARIOS'
    Y.FINAL.ARRAY<Y.REC.START,11,7>  = Y.FINAL.ARRAY<Y.REC.START,11,4>  + Y.FINAL.ARRAY<Y.REC.START,11,5>  + Y.FINAL.ARRAY<Y.REC.START,11,6>

    Y.A = 11
    GOSUB CHECK.FOR.VALUE

    Y.FINAL.ARRAY<Y.REC.START,12,1>  = Y.CCY.LIST<Y.REC.START>
    Y.FINAL.ARRAY<Y.REC.START,12,2>  = 'COMERCIALES'
    Y.FINAL.ARRAY<Y.REC.START,12,7>  = Y.FINAL.ARRAY<Y.REC.START,12,4>  + Y.FINAL.ARRAY<Y.REC.START,12,5>  + Y.FINAL.ARRAY<Y.REC.START,12,6>

    Y.A = 12
    GOSUB CHECK.FOR.VALUE

    Y.FINAL.ARRAY<Y.REC.START,13,1>  = Y.CCY.LIST<Y.REC.START>
    Y.FINAL.ARRAY<Y.REC.START,13,2>  = 'DE CONSUMO'
    Y.FINAL.ARRAY<Y.REC.START,13,7>  = Y.FINAL.ARRAY<Y.REC.START,13,4>  + Y.FINAL.ARRAY<Y.REC.START,13,5>  + Y.FINAL.ARRAY<Y.REC.START,13,6>

    Y.A = 13
    GOSUB CHECK.FOR.VALUE

*PACS00106559 - S
    Y.OTR.LOANS = FIELD(Y.FINAL.ARRAY,@FM,1,1)
    Y.OTR.LOANS = FIELD(Y.OTR.LOANS,@VM,31,99)
    Y.OTR.LOANS.CNT = DCOUNT(Y.OTR.LOANS,@VM)
    Y.L.CNT = 1
    Y.SUM.POS.3 = ''
    Y.SUM.POS.7 = ''
    IF Y.OTR.LOANS THEN
        LOOP
        WHILE Y.L.CNT LE Y.OTR.LOANS.CNT

            Y.A = 30 + Y.L.CNT
            Y.FINAL.ARRAY<Y.REC.START,Y.A,1>  = Y.CCY.LIST<Y.REC.START>
            Y.FINAL.ARRAY<Y.REC.START,Y.A,7>  = Y.FINAL.ARRAY<Y.REC.START,Y.A,4>  + Y.FINAL.ARRAY<Y.REC.START,Y.A,5>  + Y.FINAL.ARRAY<Y.REC.START,Y.A,6>
            Y.SUM.POS.3 += Y.FINAL.ARRAY<Y.REC.START,Y.A,3>
            Y.SUM.POS.7 += Y.FINAL.ARRAY<Y.REC.START,Y.A,7>
            GOSUB CHECK.FOR.VALUE
            Y.L.CNT += 1
        REPEAT

    END

    Y.FINAL.ARRAY<Y.REC.START,14,1>  = Y.CCY.LIST<Y.REC.START>
*    Y.FINAL.ARRAY<Y.REC.START,14,2>  = 'TOTAL PAGOS PREST.'
    Y.FINAL.ARRAY<Y.REC.START,14,9>  = 'TOTAL PAGOS PREST.'
    Y.FINAL.ARRAY<Y.REC.START,14,3>  = Y.FINAL.ARRAY<Y.REC.START,11,3> + Y.FINAL.ARRAY<Y.REC.START,12,3> + Y.FINAL.ARRAY<Y.REC.START,13,3> + Y.SUM.POS.3
    Y.FINAL.ARRAY<Y.REC.START,14,7>  = Y.FINAL.ARRAY<Y.REC.START,11,7>  + Y.FINAL.ARRAY<Y.REC.START,12,7>  + Y.FINAL.ARRAY<Y.REC.START,13,7> + Y.SUM.POS.7

*PACS00106559 - E
RETURN
*--------------------------------------------------------------------------------------------------------
*********************
CREDIT.CARD.PAYMENTS:
*********************
    Y.FINAL.ARRAY<Y.REC.START,15,1>  = Y.CCY.LIST<Y.REC.START>
*    Y.FINAL.ARRAY<Y.REC.START,15,2>  = 'PAGOS TARJETA CRED.'
    Y.FINAL.ARRAY<Y.REC.START,15,8>  = 'PAGOS TARJETA CRED.'
    Y.FINAL.ARRAY<Y.REC.START,15,3>  = ''
    Y.FINAL.ARRAY<Y.REC.START,15,4>  = ''
    Y.FINAL.ARRAY<Y.REC.START,15,5>  = ''
    Y.FINAL.ARRAY<Y.REC.START,15,6>  = ''
    Y.FINAL.ARRAY<Y.REC.START,15,7>  = ''

    Y.FINAL.ARRAY<Y.REC.START,16,1>  = Y.CCY.LIST<Y.REC.START>
    Y.FINAL.ARRAY<Y.REC.START,16,2>  = 'VISA'
    Y.FINAL.ARRAY<Y.REC.START,16,7>  = Y.FINAL.ARRAY<Y.REC.START,16,4>  + Y.FINAL.ARRAY<Y.REC.START,16,5> + Y.FINAL.ARRAY<Y.REC.START,16,6>

    Y.A = 16
    GOSUB CHECK.FOR.VALUE

    Y.FINAL.ARRAY<Y.REC.START,17,1>  = Y.CCY.LIST<Y.REC.START>
    Y.FINAL.ARRAY<Y.REC.START,17,2>  = 'MASTERCARD'
    Y.FINAL.ARRAY<Y.REC.START,17,7>  = Y.FINAL.ARRAY<Y.REC.START,17,4>  + Y.FINAL.ARRAY<Y.REC.START,17,5> + Y.FINAL.ARRAY<Y.REC.START,17,6>

    Y.A = 17
    GOSUB CHECK.FOR.VALUE

    Y.FINAL.ARRAY<Y.REC.START,18,1>  = Y.CCY.LIST<Y.REC.START>
    Y.FINAL.ARRAY<Y.REC.START,18,2>  = 'OTHER'
    Y.FINAL.ARRAY<Y.REC.START,18,7>  = Y.FINAL.ARRAY<Y.REC.START,18,4>  + Y.FINAL.ARRAY<Y.REC.START,18,5> + Y.FINAL.ARRAY<Y.REC.START,18,6>

    Y.A = 18
    GOSUB CHECK.FOR.VALUE

    Y.FINAL.ARRAY<Y.REC.START,19,1>  = Y.CCY.LIST<Y.REC.START>
*    Y.FINAL.ARRAY<Y.REC.START,19,2>  = 'TOTAL PAGOS T.CRED.'
    Y.FINAL.ARRAY<Y.REC.START,19,9>  = 'TOTAL PAGOS T.CRED.'
    Y.FINAL.ARRAY<Y.REC.START,19,3>  = Y.FINAL.ARRAY<Y.REC.START,16,3> + Y.FINAL.ARRAY<Y.REC.START,17,3> + Y.FINAL.ARRAY<Y.REC.START,18,3>
    Y.FINAL.ARRAY<Y.REC.START,19,7>  = Y.FINAL.ARRAY<Y.REC.START,16,7> + Y.FINAL.ARRAY<Y.REC.START,17,7> + Y.FINAL.ARRAY<Y.REC.START,18,7>

RETURN
*--------------------------------------------------------------------------------------------------------
********************
THIRDPARTY.PAYMENTS:
********************
    Y.FINAL.ARRAY<Y.REC.START,20,1>  = Y.CCY.LIST<Y.REC.START>
*    Y.FINAL.ARRAY<Y.REC.START,20,2>  = 'PAGOS SERV.TERCEROS'
    Y.FINAL.ARRAY<Y.REC.START,20,8>  = 'PAGOS SERV.TERCEROS'
    Y.FINAL.ARRAY<Y.REC.START,20,3>  = ''
    Y.FINAL.ARRAY<Y.REC.START,20,4>  = ''
    Y.FINAL.ARRAY<Y.REC.START,20,5>  = ''
    Y.FINAL.ARRAY<Y.REC.START,20,6>  = ''
    Y.FINAL.ARRAY<Y.REC.START,20,7>  = ''

    Y.COMP.COUNT = DCOUNT(Y.COMPANY.LIST,@FM)

    Y.TOT.POS = 20 + Y.COMP.COUNT + 1
    Y.FINAL.ARRAY<Y.REC.START,Y.TOT.POS,1>  = Y.CCY.LIST<Y.REC.START>
*  Y.FINAL.ARRAY<Y.REC.START,Y.TOT.POS,2>  = 'TOTAL PAGOS TERCEROS'
    Y.FINAL.ARRAY<Y.REC.START,Y.TOT.POS,9>  = 'TOTAL PAGOS TERCEROS'
    Y.FINAL.ARRAY<Y.REC.START,Y.TOT.POS,7>  = 0

    Y.COMP.START = 1
    LOOP
    WHILE Y.COMP.START LE Y.COMP.COUNT
        Y.VM.POS = 20 + Y.COMP.START
        Y.FINAL.ARRAY<Y.REC.START,Y.VM.POS,1> = Y.CCY.LIST<Y.REC.START>
        Y.FINAL.ARRAY<Y.REC.START,Y.VM.POS,7> = Y.FINAL.ARRAY<Y.REC.START,Y.VM.POS,4> + Y.FINAL.ARRAY<Y.REC.START,Y.VM.POS,5> + Y.FINAL.ARRAY<Y.REC.START,Y.VM.POS,6>
        Y.A = Y.VM.POS
        GOSUB CHECK.FOR.VALUE
        Y.FINAL.ARRAY<Y.REC.START,Y.TOT.POS,7> += Y.FINAL.ARRAY<Y.REC.START,Y.VM.POS,7>
        Y.FINAL.ARRAY<Y.REC.START,Y.TOT.POS,3> += Y.FINAL.ARRAY<Y.REC.START,Y.VM.POS,3>
        Y.COMP.START += 1
    REPEAT

RETURN
*--------------------------------------------------------------------------------------------------------
**************
OTHER.INCOMES:
**************
    Y.OIH.POS = 20 + Y.COMP.COUNT + 2

    Y.FINAL.ARRAY<Y.REC.START,Y.OIH.POS,1>  = Y.CCY.LIST<Y.REC.START>
*    Y.FINAL.ARRAY<Y.REC.START,Y.OIH.POS,2>  = 'OTROS INGRESOS'
    Y.FINAL.ARRAY<Y.REC.START,Y.OIH.POS,8>  = 'OTROS INGRESOS'
    Y.FINAL.ARRAY<Y.REC.START,Y.OIH.POS,3>  = ''
    Y.FINAL.ARRAY<Y.REC.START,Y.OIH.POS,4>  = ''
    Y.FINAL.ARRAY<Y.REC.START,Y.OIH.POS,5>  = ''
    Y.FINAL.ARRAY<Y.REC.START,Y.OIH.POS,6>  = ''
    Y.FINAL.ARRAY<Y.REC.START,Y.OIH.POS,7>  = ''

    Y.OIH.POS += 1

    Y.FINAL.ARRAY<Y.REC.START,Y.OIH.POS,1>  = Y.CCY.LIST<Y.REC.START>
    Y.FINAL.ARRAY<Y.REC.START,Y.OIH.POS,2>  = 'INGRESO TIPO'
    Y.FINAL.ARRAY<Y.REC.START,Y.OIH.POS,7>  = Y.FINAL.ARRAY<Y.REC.START,Y.OIH.POS,4>  + Y.FINAL.ARRAY<Y.REC.START,Y.OIH.POS,5>  + Y.FINAL.ARRAY<Y.REC.START,Y.OIH.POS,6>

    Y.A = Y.OIH.POS
    GOSUB CHECK.FOR.VALUE

RETURN
*--------------------------------------------------------------------------------------------------------
***************
CASH.TRANSFERS:
***************
    Y.CT.POS = 20 + Y.COMP.COUNT + 4

    Y.FINAL.ARRAY<Y.REC.START,Y.CT.POS,1>  = Y.CCY.LIST<Y.REC.START>
*    Y.FINAL.ARRAY<Y.REC.START,Y.CT.POS,2>  = 'TRANSF. RECIBIDAS'
    Y.FINAL.ARRAY<Y.REC.START,Y.CT.POS,8>  = 'TRANSF. RECIBIDAS'
    Y.FINAL.ARRAY<Y.REC.START,Y.CT.POS,3>  = ''
    Y.FINAL.ARRAY<Y.REC.START,Y.CT.POS,4>  = ''
    Y.FINAL.ARRAY<Y.REC.START,Y.CT.POS,5>  = ''
    Y.FINAL.ARRAY<Y.REC.START,Y.CT.POS,6>  = ''
    Y.FINAL.ARRAY<Y.REC.START,Y.CT.POS,7>  = ''

    Y.CT.POS += 1
    Y.A = Y.CT.POS
    GOSUB CHECK.FOR.VALUE

    Y.FINAL.ARRAY<Y.REC.START,Y.CT.POS,1>  = Y.CCY.LIST<Y.REC.START>
    Y.FINAL.ARRAY<Y.REC.START,Y.CT.POS,2>  = 'POR CAJEROS/BOV'
    Y.FINAL.ARRAY<Y.REC.START,Y.CT.POS,7>  = Y.FINAL.ARRAY<Y.REC.START,Y.CT.POS,4> +  Y.FINAL.ARRAY<Y.REC.START,Y.CT.POS,5> +  Y.FINAL.ARRAY<Y.REC.START,Y.CT.POS,6>


    Y.CT.POS += 1
    Y.A = Y.CT.POS
    GOSUB CHECK.FOR.VALUE

    Y.FINAL.ARRAY<Y.REC.START,Y.CT.POS,1>  = Y.CCY.LIST<Y.REC.START>
    Y.FINAL.ARRAY<Y.REC.START,Y.CT.POS,2>  = 'BCENTRAL/OTROS BCOS.'

    Y.FINAL.ARRAY<Y.REC.START,Y.CT.POS,7>   =  Y.FINAL.ARRAY<Y.REC.START,Y.CT.POS,4> +  Y.FINAL.ARRAY<Y.REC.START,Y.CT.POS,5> +  Y.FINAL.ARRAY<Y.REC.START,Y.CT.POS,6>

RETURN
*--------------------------------------------------------------------------------------------------------
*************
BUY.CURRENCY:
*************
    Y.BUY.POS = 20 + Y.COMP.COUNT + 7

    Y.FINAL.ARRAY<Y.REC.START,Y.BUY.POS,1>  = Y.CCY.LIST<Y.REC.START>
*    Y.FINAL.ARRAY<Y.REC.START,Y.BUY.POS,2>  = 'COMPRA DE DIVISAS'
    Y.FINAL.ARRAY<Y.REC.START,Y.BUY.POS,8>  = 'COMPRA DE DIVISAS'
    Y.FINAL.ARRAY<Y.REC.START,Y.BUY.POS,3>  = ''
    Y.FINAL.ARRAY<Y.REC.START,Y.BUY.POS,4>  = ''
    Y.FINAL.ARRAY<Y.REC.START,Y.BUY.POS,5>  = ''
    Y.FINAL.ARRAY<Y.REC.START,Y.BUY.POS,6>  = ''
    Y.FINAL.ARRAY<Y.REC.START,Y.BUY.POS,7>  = ''

    Y.BUY.POS += 1
    Y.FINAL.ARRAY<Y.REC.START,Y.BUY.POS,1>  = Y.CCY.LIST<Y.REC.START>
    Y.FINAL.ARRAY<Y.REC.START,Y.BUY.POS,2>  = 'DIVISA'
    Y.FINAL.ARRAY<Y.REC.START,Y.BUY.POS,7>  = Y.FINAL.ARRAY<Y.REC.START,Y.BUY.POS,4>  + Y.FINAL.ARRAY<Y.REC.START,Y.BUY.POS,5>  + Y.FINAL.ARRAY<Y.REC.START,Y.BUY.POS,6>

    Y.A = Y.BUY.POS
    GOSUB CHECK.FOR.VALUE

*    Y.BUY.POS += 1
*    Y.FINAL.ARRAY<Y.REC.START,Y.BUY.POS,1>  = Y.CCY.LIST<Y.REC.START>
*    Y.FINAL.ARRAY<Y.REC.START,Y.BUY.POS,2>  = 'DIVISA 2'
*    Y.FINAL.ARRAY<Y.REC.START,Y.BUY.POS,7>  = Y.FINAL.ARRAY<Y.REC.START,Y.BUY.POS,4>  + Y.FINAL.ARRAY<Y.REC.START,Y.BUY.POS,5>  + Y.FINAL.ARRAY<Y.REC.START,Y.BUY.POS,6>

*    Y.A = Y.BUY.POS
*    GOSUB CHECK.FOR.VALUE

RETURN
*--------------------------------------------------------------------------------------------------------
**************
GET.TOTAL.INC:
**************
    Y.TI.POS = 20 + Y.COMP.COUNT + 10

    Y.FINAL.ARRAY<Y.REC.START,Y.TI.POS,1>  = Y.CCY.LIST<Y.REC.START>
*    Y.FINAL.ARRAY<Y.REC.START,Y.TI.POS,2>  = 'TOTAL RECIBIDO'
    Y.FINAL.ARRAY<Y.REC.START,Y.TI.POS,9>  = 'TOTAL RECIBIDO'
    Y.FINAL.ARRAY<Y.REC.START,Y.TI.POS,4>  = ''
    Y.FINAL.ARRAY<Y.REC.START,Y.TI.POS,5>  = ''
    Y.FINAL.ARRAY<Y.REC.START,Y.TI.POS,6>  = ''

    Y.FINAL.ARRAY<Y.REC.START,Y.TI.POS,7> = Y.FINAL.ARRAY<Y.REC.START,4,7> + Y.FINAL.ARRAY<Y.REC.START,9,7> + Y.FINAL.ARRAY<Y.REC.START,14,7> + Y.FINAL.ARRAY<Y.REC.START,19,7> + Y.FINAL.ARRAY<Y.REC.START,Y.TOT.POS,7> + Y.FINAL.ARRAY<Y.REC.START,Y.OIH.POS,7> + Y.FINAL.ARRAY<Y.REC.START,Y.CT.POS-1,7> + Y.FINAL.ARRAY<Y.REC.START,Y.CT.POS,7> + Y.FINAL.ARRAY<Y.REC.START,Y.BUY.POS-1,7> + Y.FINAL.ARRAY<Y.REC.START,Y.BUY.POS,7>

RETURN
*--------------------------------------------------------------------------------------------------------
********
OVERAGE:
********
    Y.OVER.POS = 20 + Y.COMP.COUNT + 9

    Y.FINAL.ARRAY<Y.REC.START,Y.OVER.POS,1>  = Y.CCY.LIST<Y.REC.START>
*    Y.FINAL.ARRAY<Y.REC.START,Y.OVER.POS,2>  = 'SOBRANTE'
    Y.FINAL.ARRAY<Y.REC.START,Y.OVER.POS,8>  = 'SOBRANTE'
    Y.FINAL.ARRAY<Y.REC.START,Y.OVER.POS,4>  = ''
    Y.FINAL.ARRAY<Y.REC.START,Y.OVER.POS,5>  = ''
    Y.FINAL.ARRAY<Y.REC.START,Y.OVER.POS,6>  = ''
    Y.FINAL.ARRAY<Y.REC.START,Y.OVER.POS,3> = ''

* IF NOT(Y.FINAL.ARRAY<Y.REC.START,Y.OVER.POS,3>) THEN
*     Y.FINAL.ARRAY<Y.REC.START,Y.OVER.POS,3> = ''
* END

    IF NOT(Y.FINAL.ARRAY<Y.REC.START,Y.OVER.POS,7>) THEN
        Y.FINAL.ARRAY<Y.REC.START,Y.OVER.POS,7> = 0
    END

RETURN
*--------------------------------------------------------------------------------------------------------
****************
CHECK.FOR.VALUE:
****************
* In this para of the code, the variable is checked if it holds any value else its amended with ZERO

    Y.B.START = 1
    LOOP
    WHILE Y.B.START LE Y.B.COUNT
        Y.B = Y.B.LIST<Y.B.START>
        IF NOT(Y.FINAL.ARRAY<Y.REC.START,Y.A,Y.B>) THEN
            Y.FINAL.ARRAY<Y.REC.START,Y.A,Y.B> = 0
        END
        Y.B.START += 1
    REPEAT

RETURN
*--------------------------------------------------------------------------------------------------------
END       ;* End of Program
