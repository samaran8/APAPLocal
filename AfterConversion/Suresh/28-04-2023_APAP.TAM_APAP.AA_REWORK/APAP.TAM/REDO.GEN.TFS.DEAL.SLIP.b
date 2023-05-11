* @ValidationCode : MjotNzE1NzA1MTc0OkNwMTI1MjoxNjgyNjc5MDQ0MDc2OjMzM3N1Oi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 28 Apr 2023 16:20:44
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : 333su
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
*-----------------------------------------------------------------------------
* <Rating>-39</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE REDO.GEN.TFS.DEAL.SLIP
*--------------------------------------------------
* Description: This routine is Auth routine for TFS version
* to produce the deal slip
*--------------------------------------------------
* Date           Who            Dev Ref                  Modification
* 08 Jan 2013   H Ganesh      PACS00241481 - CLEARING   Initial Draft
*-----------------------------------------------------------------------------------
*Modification History:
*DATE                 WHO                  REFERENCE                     DESCRIPTION
*25/04/2023      CONVERSION TOOL     AUTO R22 CODE CONVERSION             NOCHANGE
*25/04/2023         SURESH           MANUAL R22 CODE CONVERSION           NOCHANGE
*-----------------------------------------------------------------------------------

*--------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.COMPANY
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CUSTOMER
    $INSERT I_F.CURRENCY
    $INSERT I_TT.COMMON
    $INSERT I_T24.FS.COMMON
    $INSERT I_F.T24.FUND.SERVICES
    $INSERT I_F.TFS.TRANSACTION
    $INSERT I_REDO.TFS.DEAL.SLIP.COMMON


    GOSUB PROCESS
RETURN
*--------------------------------------------------
PROCESS:
*--------------------------------------------------

    FN.TELLER.USER = 'F.TELLER.USER'
    F.TELLER.USER = ''
    CALL OPF(FN.TELLER.USER,F.TELLER.USER)

    FN.TFS.TRANSACTION = 'F.TFS.TRANSACTION'
    F.TFS.TRANSACTION  = ''
    CALL OPF(FN.TFS.TRANSACTION,F.TFS.TRANSACTION)

    FN.CURRENCY = 'F.CURRENCY'
    F.CURRENCY  = ''
    CALL OPF(FN.CURRENCY,F.CURRENCY)

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

*FN.CUSTOMER = 'F.CUSTOMER'
*F.CUSTOMER = ''
*CALL OPF(FN.CUSTOMER,F.CUSTOMER)


    LOC.REF.APPLICATION = "ACCOUNT":@FM:"T24.FUND.SERVICES"
    LOC.REF.FIELDS      = 'L.AC.ALPH.AC.NO':@FM:'L.TT.NO.OF.CHQ':@VM:'L.NCF.NUMBER'
    LOC.REF.POS         = ''
    CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LOC.REF.POS)
    POS.L.AC.ALPH.AC.NO = LOC.REF.POS<1,1>
    POS.L.TT.NO.OF.CHQ  = LOC.REF.POS<2,1>
    POS.L.NCF.NUMBER    = LOC.REF.POS<2,2>

    Y.TRANSACTION.CODE = R.NEW(TFS.TRANSACTION)

    GOSUB CHECK.TRANS.TYPE
    GOSUB PRODUCE.DEAL.SLIP

RETURN
*--------------------------------------------------
CHECK.TRANS.TYPE:
*--------------------------------------------------


    Y.CASH.TRANS   = ''
    Y.CHEQUE.TRANS = ''
    Y.CASH.AMT     = 0
    Y.CHEQUE.AMT   = 0
    Y.VAR1 = 1
    Y.TRANSACTION.CNT = DCOUNT(Y.TRANSACTION.CODE,@VM)
    LOOP
    WHILE Y.VAR1 LE Y.TRANSACTION.CNT
        Y.TRANS = Y.TRANSACTION.CODE<1,Y.VAR1>
        IF Y.TRANS EQ 'CASHDEP' OR Y.TRANS EQ 'FCASHDEP' OR Y.TRANS EQ 'CASHDEPD' THEN
            Y.CASH.TRANS = 'YES'
            Y.CASH.AMT += R.NEW(TFS.AMOUNT)<1,Y.VAR1>
        END
        IF Y.TRANS EQ 'CHQDEP' OR Y.TRANS EQ 'FCHQDEP' THEN
            Y.CHEQUE.TRANS = 'YES'
            Y.CHEQUE.AMT += R.NEW(TFS.AMOUNT)<1,Y.VAR1>
        END

        Y.VAR1++
    REPEAT
    BEGIN CASE
        CASE Y.CASH.TRANS EQ 'YES' AND Y.CHEQUE.TRANS EQ 'YES'
            Y.TRANS.DESC = 'DEPOSITO EN CHEQUE - MIXTO'
        CASE Y.CASH.TRANS EQ '' AND Y.CHEQUE.TRANS EQ 'YES'
            Y.TRANS.DESC = 'DEPOSITO EN CHEQUE'
        CASE Y.CASH.TRANS EQ 'YES' AND Y.CHEQUE.TRANS EQ ''
            Y.TRANS.DESC = 'DEPOSITO EN EFFECTIVO'
    END CASE


    Y.CUR.TIME = TIMEDATE()
    IF R.NEW(TFS.RECORD.STATUS) EQ 'INAO' THEN
        Y.VALUE = R.NEW(TFS.INPUTTER)
        Y.OPERATOR = FIELD(Y.VALUE,"_",2)
    END ELSE
        Y.OPERATOR = OPERATOR
    END
    CALL F.READ(FN.TELLER.USER,Y.OPERATOR,R.TELLER.USER,F.TELLER.USER,TT.US.ERR)
    Y.TELL.ID = R.TELLER.USER
    IF R.COMPANY(EB.COM.COMPANY.NAME)<1,LNGG> EQ '' THEN
        Y.COMP.DESB = R.COMPANY(EB.COM.COMPANY.NAME)<1,1>
    END ELSE
        Y.COMP.DESB = R.COMPANY(EB.COM.COMPANY.NAME)<1,LNGG>
    END

*    Y.TELLER.ID = Y.COMP.DESB :' - ': TT$TID
    Y.TELLER.ID = Y.COMP.DESB :' - ': Y.TELL.ID
    Y.TOTAL.AMT = Y.CASH.AMT + Y.CHEQUE.AMT

*CALL CACHE.READ(FN.TFS.TRANSACTION,'NET.ENTRY',R.TFS.TRANSACTION,TFS.ERR)
*IF R.TFS.TRANSACTION<TFS.TXN.DESCRIPTION,LNGG> EQ '' THEN
*Y.TRANS.DESC = R.TFS.TRANSACTION<TFS.TXN.DESCRIPTION,1>
*END ELSE
*Y.TRANS.DESC = R.TFS.TRANSACTION<TFS.TXN.DESCRIPTION,LNGG>
*END

    Y.CURRENCY = R.NEW(TFS.CURRENCY)<1,1>
    CALL CACHE.READ(FN.CURRENCY,Y.CURRENCY,R.CURRENCY,CUR.ERR)
    IF R.CURRENCY<EB.CUR.CCY.NAME,LNGG> EQ '' THEN
        Y.CURRENCY = R.CURRENCY<EB.CUR.CCY.NAME,1>
    END ELSE
        Y.CURRENCY = R.CURRENCY<EB.CUR.CCY.NAME,LNGG>
    END

    Y.TRANS.REF = ID.NEW
    Y.PRIMARY.ACCOUNT = R.NEW(TFS.PRIMARY.ACCOUNT)
    CALL F.READ(FN.ACCOUNT,Y.PRIMARY.ACCOUNT,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
    Y.L.AC.ALPH.AC.NO = R.ACCOUNT<AC.LOCAL.REF,POS.L.AC.ALPH.AC.NO>
    Y.CUS.ID          = R.ACCOUNT<AC.CUSTOMER>
*CALL F.READ(FN.CUSTOMER,Y.CUS.ID,R.CUSTOMER,F.CUSTOMER,CUS.ERR)
*Y.NAME.1         = R.CUSTOMER<EB.CUS.NAME.1>
*Y.NAME.2         = R.CUSTOMER<EB.CUS.NAME.2>
    CALL APAP.REDORETAIL.REDO.CUST.IDENTITY.REF(Y.CUS.ID,Y.ALT.ID,Y.CUS.NAME) ;*MANUAL R22 CODE CONVERSION
    Y.NAME.1         = Y.CUS.NAME[1,35]
    Y.NAME.2         = Y.CUS.NAME[36,LEN(Y.CUS.NAME)]
    Y.L.TT.NO.OF.CHQ = R.NEW(TFS.LOCAL.REF)<1,POS.L.TT.NO.OF.CHQ>
    Y.L.NCF.NUMBER   = R.NEW(TFS.LOCAL.REF)<1,POS.L.NCF.NUMBER>

RETURN
*--------------------------------------------------
PRODUCE.DEAL.SLIP:
*--------------------------------------------------

* Fix for TFS DEALSLIP ALIGNMENT

    Y.MONTO.NCF = 0
    R.DEAL.TFS.ARRAY    = ''
    R.DEAL.TFS.ARRAY<1> = FMT(Y.CUR.TIME,'R#30')
    R.DEAL.TFS.ARRAY<2> = FMT(Y.TELLER.ID,'R#30')
*R.DEAL.TFS.ARRAY<3> = FMT(Y.MONTO.NCF,"L2,#17")
    R.DEAL.TFS.ARRAY<4> = FMT(Y.TOTAL.AMT,"R2,#24")
    R.DEAL.TFS.ARRAY<5> = FMT(Y.CASH.AMT,"R2,#24")
    R.DEAL.TFS.ARRAY<6> = FMT(Y.CHEQUE.AMT,"R2,#24")
    R.DEAL.TFS.ARRAY<7> = FMT(Y.TRANS.DESC,'R#30')
    R.DEAL.TFS.ARRAY<8> = FMT(Y.CURRENCY,'R#30')
    R.DEAL.TFS.ARRAY<9> = FMT(Y.TRANS.REF,'R#24')
    R.DEAL.TFS.ARRAY<10>= FMT(Y.PRIMARY.ACCOUNT,'R#24')
    R.DEAL.TFS.ARRAY<11>= FMT(Y.L.AC.ALPH.AC.NO,'R#30')
    R.DEAL.TFS.ARRAY<12>= FMT(Y.NAME.1,'R#30')
    R.DEAL.TFS.ARRAY<13>= FMT(Y.L.TT.NO.OF.CHQ,'R#24')
*R.DEAL.TFS.ARRAY<14>= Y.L.NCF.NUMBER
    R.DEAL.TFS.ARRAY<15>= FMT(Y.NAME.2,'R#30')

* End of Fix

RETURN
END
