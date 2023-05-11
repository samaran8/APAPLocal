* @ValidationCode : MjotMjAyNTIyODQ4MTpDcDEyNTI6MTY4MTM2NDkwMDIzNTpJVFNTOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 13 Apr 2023 11:18:20
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
$PACKAGE APAP.TAM
SUBROUTINE REDO.REINV.ANC.FT
*-------------------------------------------------------

* DESCRIPTION: This routine is default ANC fields for reversal of FT


*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
*
*  DATE             WHO         REFERENCE           DESCRIPTION
* 14-Jul-2011     H Ganesh    PACS00072695 - N.11   Initial Draft
** 13-04-2023 R22 Auto Conversion - FM TO @FM, VM to @VM, SM to @SM
** 13-04-2023 Skanda R22 Manual Conversion - added APAP.TAM


    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_System
    $INSERT I_F.FUNDS.TRANSFER


    GOSUB INIT
    GOSUB PROCESS
RETURN

*-----------------------------------------------------------------------------
INIT:
*-----------------------------------------------------------------------------

    FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER'
    F.FUNDS.TRANSFER = ''
    CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)

    FN.FT.HIS = 'F.FUNDS.TRANSFER$HIS'
    F.FT.HIS = ''
    CALL OPF(FN.FT.HIS,F.FT.HIS)

    LOC.REF.APPLICATION="FUNDS.TRANSFER"
    LOC.REF.FIELDS  = 'BENEFIC.NAME':@VM:'L.FT.CONCEPT':@VM:'L.FT.COMM.CODE':@VM:'L.TT.TRANS.AMT':@VM:'L.FT.ADD.INFO':@VM:'L.TT.COMM.CODE'
    LOC.REF.FIELDS := @VM:'L.TT.WV.COMM':@VM:'L.TT.COMM.AMT':@VM:'L.TT.TAX.CODE':@VM:'L.TT.WV.TAX':@VM:'L.TT.TAX.AMT':@VM:'L.TT.WV.TAX.AMT'
    LOC.REF.FIELDS := @VM:'L.NCF.REQUIRED':@VM:'L.NCF.NUMBER':@VM:'L.NCF.TAX.NUM':@VM:'TRANSACTION.REF'
    LOC.REF.POS=''
    CALL APAP.TAM.MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LOC.REF.POS) ;* R22 Manual conversion
    POS.BENEFIC.NAME    = LOC.REF.POS<1,1>
    POS.L.FT.CONCEPT    = LOC.REF.POS<1,2>
    POS.L.FT.COMM.CODE  = LOC.REF.POS<1,3>
    POS.L.TT.TRANS.AMT  = LOC.REF.POS<1,4>
    POS.L.FT.ADD.INFO   = LOC.REF.POS<1,5>
    POS.L.TT.COMM.CODE  = LOC.REF.POS<1,6>
    POS.L.TT.WV.COMM    = LOC.REF.POS<1,7>
    POS.L.TT.COMM.AMT   = LOC.REF.POS<1,8>
    POS.L.TT.TAX.CODE   = LOC.REF.POS<1,9>
    POS.L.TT.WV.TAX     = LOC.REF.POS<1,10>
    POS.L.TT.TAX.AMT    = LOC.REF.POS<1,11>
    POS.L.TT.WV.TAX.AMT = LOC.REF.POS<1,12>
    POS.L.NCF.REQUIRED  = LOC.REF.POS<1,13>
    POS.L.NCF.NUMBER    = LOC.REF.POS<1,14>
    POS.L.NCF.TAX.NUM   = LOC.REF.POS<1,15>
    POS.TXN.REF         = LOC.REF.POS<1,16>


RETURN
*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------

    Y.DATA = ""
    CALL BUILD.USER.VARIABLES(Y.DATA)

* Fix for PACS00293633 [Field values not getting defaulted from FT REVERSE]
*Y.FT.ID = FIELD(Y.DATA,"*",2)
    LOOP
        REMOVE Y.DATA.VAL FROM Y.DATA SETTING Y.DATA.POS
    WHILE Y.DATA.VAL:Y.DATA.POS
        CHECK.FT.ID.CONDITION = FIELD(Y.DATA.VAL,'*',1)
        IF CHECK.FT.ID.CONDITION EQ 'CURRENT.ID' THEN
            Y.FT.ID=FIELD(Y.DATA.VAL,"*",2)
            EXIT
        END
    REPEAT

* End of Fix

    CALL F.READ(FN.FUNDS.TRANSFER,Y.FT.ID,R.FT,F.FUNDS.TRANSFER,FT.ERR)
    IF NOT(R.FT) THEN
        VAR.FT.ID = Y.FT.ID
        CALL EB.READ.HISTORY.REC(F.FT.HIS,VAR.FT.ID,R.FT,FT.ERR)
    END
* This fields are updated manually by user to reinstate cheques.
    IF PGM.VERSION NE ",REDO.REINSTATE" THEN
        R.NEW(FT.DEBIT.ACCT.NO)     = R.FT<FT.CREDIT.ACCT.NO>
        R.NEW(FT.CREDIT.ACCT.NO)    = R.FT<FT.DEBIT.ACCT.NO>
        R.NEW(FT.CREDIT.CURRENCY)   = R.FT<FT.DEBIT.CURRENCY>
        R.NEW(FT.CREDIT.CUSTOMER)   = R.FT<FT.DEBIT.CUSTOMER>
    END
    R.NEW(FT.DEBIT.CURRENCY)    = R.FT<FT.CREDIT.CURRENCY>
    R.NEW(FT.ORDERING.CUST)     = R.FT<FT.DEBIT.CUSTOMER>
    VAR.AMOUNT.CR               = R.FT<FT.AMOUNT.CREDITED>
    AMT.LEN = LEN(VAR.AMOUNT.CR)
    R.NEW(FT.DEBIT.AMOUNT)      = VAR.AMOUNT.CR[4,AMT.LEN]
    R.NEW(FT.CREDIT.THEIR.REF)  = R.FT<FT.CREDIT.THEIR.REF>
    R.NEW(FT.LOCAL.REF)<1,POS.BENEFIC.NAME>    = R.FT<FT.LOCAL.REF,POS.BENEFIC.NAME>
    R.NEW(FT.LOCAL.REF)<1,POS.L.FT.CONCEPT>    = R.FT<FT.LOCAL.REF,POS.L.FT.CONCEPT>
    R.NEW(FT.LOCAL.REF)<1,POS.L.FT.COMM.CODE>  = R.FT<FT.LOCAL.REF,POS.L.FT.COMM.CODE>
    R.NEW(FT.LOCAL.REF)<1,POS.L.TT.TRANS.AMT>  = R.FT<FT.LOCAL.REF,POS.L.TT.TRANS.AMT>
    R.NEW(FT.LOCAL.REF)<1,POS.L.TT.COMM.CODE>  = R.FT<FT.LOCAL.REF,POS.L.TT.COMM.CODE>
    R.NEW(FT.LOCAL.REF)<1,POS.L.TT.WV.COMM>    = R.FT<FT.LOCAL.REF,POS.L.TT.WV.COMM>
    R.NEW(FT.LOCAL.REF)<1,POS.L.TT.COMM.AMT>   = R.FT<FT.LOCAL.REF,POS.L.TT.COMM.AMT>
    R.NEW(FT.LOCAL.REF)<1,POS.L.TT.TAX.CODE>   = R.FT<FT.LOCAL.REF,POS.L.TT.TAX.CODE>
    R.NEW(FT.LOCAL.REF)<1,POS.L.TT.WV.TAX>     = R.FT<FT.LOCAL.REF,POS.L.TT.WV.TAX>
    R.NEW(FT.LOCAL.REF)<1,POS.L.TT.TAX.AMT>    = R.FT<FT.LOCAL.REF,POS.L.TT.TAX.AMT>
    R.NEW(FT.LOCAL.REF)<1,POS.L.TT.WV.TAX.AMT> = R.FT<FT.LOCAL.REF,POS.L.TT.WV.TAX.AMT>
    R.NEW(FT.LOCAL.REF)<1,POS.L.NCF.REQUIRED>  = R.FT<FT.LOCAL.REF,POS.L.NCF.REQUIRED>
    R.NEW(FT.LOCAL.REF)<1,POS.L.NCF.NUMBER>    = R.FT<FT.LOCAL.REF,POS.L.NCF.NUMBER>
    R.NEW(FT.LOCAL.REF)<1,POS.L.NCF.TAX.NUM>   = R.FT<FT.LOCAL.REF,POS.L.NCF.TAX.NUM>
    R.NEW(FT.LOCAL.REF)<1,POS.TXN.REF>         = Y.FT.ID

    IF PGM.VERSION EQ ",GERENCIA.REVERSO.CHQ" OR PGM.VERSION EQ ",MGR.REVERSE.CHQ" THEN
        R.NEW(FT.PROFIT.CENTRE.CUST) = R.FT<FT.DEBIT.CUSTOMER>
    END

RETURN
END
