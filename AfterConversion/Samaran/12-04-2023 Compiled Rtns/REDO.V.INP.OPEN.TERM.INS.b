* @ValidationCode : MjoxNTQzODAyNDMyOkNwMTI1MjoxNjgxMjg0MTc0MzIxOnNhbWFyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 12 Apr 2023 12:52:54
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
SUBROUTINE REDO.V.INP.OPEN.TERM.INS
*-----------------------------------------------------------------------------
*DESCRIPTION:
*------------
*This routine is an INPUT routine attached to below versions,
*

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
*   Date               who           Reference                            Description
*
* 10-11-2010        JEEVA T           N.45                              INITIAL CREATION
* 21-07-2011        Bharath G        PACS00085750                 Local Fields included in the version
*12-04-2023       Conversion Tool    R22 Auto Code conversion          VM TO @VM
*12-04-2023       Samaran T          R22 Manual Code Conversion        No Changes
 
*------------------------------------------------------------------------------------------
    $INSERT I_EQUATE
    $INSERT I_COMMON
    $INSERT I_F.TELLER
    $INSERT I_F.USER
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.CUSTOMER
    $INSERT I_F.ACCOUNT
    $INSERT I_F.REDO.MTS.DISBURSE


    GOSUB INIT
    GOSUB CHECK.AMT
RETURN

******
INIT:
******
*Initialize all the variables

    FN.REDO.MTS.DISBURSE  ='F.REDO.MTS.DISBURSE'
    FN.CUSTOMER            ='F.CUSTOMER'
    FN.ACCOUNT             ='F.ACCOUNT'
    F.REDO.MTS.DISBURSE   ='' ; REDO.MTS.DISBURSE = ''
    F.CUSTOMER  =''
    F.ACCOUNT  =''

    CALL OPF(FN.REDO.MTS.DISBURSE ,F.REDO.MTS.DISBURSE)
    CALL OPF(FN.CUSTOMER ,F.CUSTOMER)
    CALL OPF(FN.ACCOUNT ,F.ACCOUNT)

RETURN

***********
CHECK.AMT:
***********
* Checking the amount value with total amount overdue value

    IF APPLICATION EQ 'FUNDS.TRANSFER' THEN
        Y.TRNS.TYPE = 'DEPOSIT'
        GOSUB PROCESS1
    END

    IF APPLICATION EQ 'TELLER' THEN

        IF PGM.VERSION EQ ',REDO.AA.CHQ' THEN
            Y.TRNS.TYPE = 'CHEQUE'
        END ELSE
            Y.TRNS.TYPE = 'CASH'
        END
        GOSUB PROCESS2
    END
    GOSUB WRITE.FILE
RETURN

***********
PROCESS1:
***********
* PACS00085750 - S
    LREF.APP = 'FUNDS.TRANSFER'
    LREF.FIELDS = 'L.FT.CLIENT.COD':@VM:'L.FT.CLIENT.NME':@VM:'L.FT.CMPNY.ID':@VM:'L.FT.CMPNY.NAME'
    LREF.POS=''
    CALL MULTI.GET.LOC.REF(LREF.APP,LREF.FIELDS,LREF.POS)
    VAR.L.FT.CLIENT.COD.POS = LREF.POS<1,1>
    VAR.L.FT.CLIENT.NME.POS = LREF.POS<1,2>
    VAR.L.FT.CMPNY.ID.POS   = LREF.POS<1,3>
    VAR.L.FT.CMPNY.NAME.POS = LREF.POS<1,4>
*PACS00085750 - E

    Y.CUS.NO      =   R.NEW(FT.LOCAL.REF)<1,VAR.L.FT.CLIENT.COD.POS>
    Y.ACC.ID      =   R.NEW(FT.DEBIT.ACCT.NO)
    BR.CODE       =   R.NEW(FT.LOCAL.REF)<1,VAR.L.FT.CMPNY.ID.POS>
    CALL F.READ(FN.CUSTOMER,Y.CUS.NO,R.CUSTOMER,F.CUSTOMER,Y.CUS.ERR)
    CALL F.READ(FN.ACCOUNT,Y.ACC.ID,R.ACCOUNT,F.ACCOUNT,Y.ACC.ERR)
    Y.AA.ID = R.ACCOUNT<AC.ARRANGEMENT.ID>
* Y.SHRT.NAME = R.CUSTOMER<EB.CUS.SHORT.NAME>  ; * PACS00085750
    R.REDO.MTS.DISBURSE<MT.CUSTOMER.NO>          = Y.CUS.NO
    R.REDO.MTS.DISBURSE<MT.BENEFICIARY>           = R.NEW(FT.LOCAL.REF)<1,VAR.L.FT.CLIENT.NME.POS>
    R.REDO.MTS.DISBURSE<MT.BR.AC.NUMBER>         = R.NEW(FT.CREDIT.ACCT.NO)
    R.REDO.MTS.DISBURSE<MT.BRANCH.ID>            = BR.CODE
    R.REDO.MTS.DISBURSE<MT.ARRANGEMENT.ID>       = Y.AA.ID
    R.REDO.MTS.DISBURSE<MT.TRAN.TYPE>            = Y.TRNS.TYPE
* R.REDO.MTS.DISBURSE<MT.CURRENCY>             = R.NEW(FT.DEBIT.CURRENCY)   ; * PACS00085750
* R.REDO.MTS.DISBURSE<MT.AMOUNT>               = R.NEW(FT.DEBIT.AMOUNT)     ; * PACS00085750
    R.REDO.MTS.DISBURSE<MT.CURRENCY>             = R.NEW(FT.CREDIT.CURRENCY)      ;* PACS00085750
    R.REDO.MTS.DISBURSE<MT.AMOUNT>               = R.NEW(FT.CREDIT.AMOUNT)        ;* PACS00085750
    R.REDO.MTS.DISBURSE<MT.LN.ACCOUNT.NO>        = Y.ACC.ID
RETURN

***********
PROCESS2:
***********
    Y.INTERNAL = R.NEW(TT.TE.ACCOUNT.1)
    Y.CUS.NO =   R.NEW(TT.TE.CUSTOMER.2)
    Y.ACC.ID = R.NEW(TT.TE.ACCOUNT.2)
    BR.CODE = Y.INTERNAL[13,17]
    CALL F.READ(FN.CUSTOMER,Y.CUS.NO,R.CUSTOMER,F.CUSTOMER,Y.CUS.ERR)
    CALL F.READ(FN.ACCOUNT,Y.ACC.ID,R.ACCOUNT,F.ACCOUNT,Y.ACC.ERR)
    Y.AA.ID = R.ACCOUNT<AC.ARRANGEMENT.ID>
    Y.SHRT.NAME = R.CUSTOMER<EB.CUS.SHORT.NAME>
    R.REDO.MTS.DISBURSE<MT.CUSTOMER.NO>          = Y.CUS.NO
    R.REDO.MTS.DISBURSE<MT.BENEFICIARY>           = Y.SHRT.NAME
    R.REDO.MTS.DISBURSE<MT.ARRANGEMENT.ID>       = Y.AA.ID
    R.REDO.MTS.DISBURSE<MT.TRAN.TYPE>            = Y.TRNS.TYPE
    R.REDO.MTS.DISBURSE<MT.LN.ACCOUNT.NO>        = Y.ACC.ID
    R.REDO.MTS.DISBURSE<MT.CURRENCY>             = R.NEW(TT.TE.CURRENCY.1)
    R.REDO.MTS.DISBURSE<MT.AMOUNT>               = R.NEW(TT.TE.AMOUNT.LOCAL.1)
    R.REDO.MTS.DISBURSE<MT.BR.AC.NUMBER>         = R.NEW(TT.TE.ACCOUNT.1)
    R.REDO.MTS.DISBURSE<MT.BRANCH.ID>            = BR.CODE
    R.REDO.MTS.DISBURSE<MT.REMARKS>              = R.NEW(TT.TE.NARRATIVE.2)


RETURN
**************
WRITE.FILE:
***************
*    R.REDO.MTS.DISBURSE<MT.INPUTTER>=TNO:'_':OPERATOR
*    R.REDO.MTS.DISBURSE<MT.AUDITOR.CODE>=TNO:'_':OPERATOR
*    R.REDO.MTS.DISBURSE<MT.CO.CODE>=ID.COMPANY
*    R.REDO.MTS.DISBURSE<MT.DEPT.CODE>=R.USER<EB.USE.DEPARTMENT.CODE>
*    R.REDO.MTS.DISBURSE<MT.RECORD.STATUS> = ''

    CALL F.WRITE(FN.REDO.MTS.DISBURSE,ID.NEW,R.REDO.MTS.DISBURSE)
RETURN
******************************************************************************************************

END
