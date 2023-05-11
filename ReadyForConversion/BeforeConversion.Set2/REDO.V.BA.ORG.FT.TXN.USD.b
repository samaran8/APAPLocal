*-----------------------------------------------------------------------------
* <Rating>185</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE REDO.V.BA.ORG.FT.TXN.USD

*-------------------------------------------------------------------------
* Company Name  : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By  : Juan Garcia
* Program Name  : REDO.V.BA.ORG.FT.TXN.USD
*-------------------------------------------------------------------------
* Description: Esta rutina es una copia de REDO.V.BA.ORG.FT.TXN creada para
* trabajar solos desembolsos en Dolares
*-------------------------------------------------------------------------

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.USER
    $INSERT T24.BP I_F.ACCOUNT
    $INSERT TAM.BP I_F.REDO.FT.TT.TRANSACTION
    $INSERT T24.BP I_F.FUNDS.TRANSFER

    GOSUB INIT
    GOSUB PROCESS

    RETURN

*====
INIT:
*====
    Y.VERSION.NAME = APPLICATION:PGM.VERSION
    Y.APPLICATION = APPLICATION
    Y.OFS.BDY     = ''
    RETURN

*=======
PROCESS:
*=======
    IF APPLICATION EQ 'REDO.FT.TT.TRANSACTION' THEN

        OFSVERSION = ''
        Y.TRANSACTION.TYPE      = R.NEW(FT.TN.TRANSACTION.TYPE)
        Y.DEBIT.ACCT.NO         = R.NEW(FT.TN.DEBIT.ACCT.NO)
        Y.DEBIT.CURRENCY        = R.NEW(FT.TN.DEBIT.CURRENCY)
        Y.DEBIT.VALUE.DATE      = R.NEW(FT.TN.DEBIT.VALUE.DATE)
        Y.DEBIT.THEIR.REF       = R.NEW(FT.TN.DEBIT.THEIR.REF)
        Y.CREDIT.ACCT.NO        = R.NEW(FT.TN.CREDIT.ACCT.NO)
        Y.CREDIT.AMOUNT         = R.NEW(FT.TN.CREDIT.AMOUNT)
        Y.DEBIT.AMOUNT          = R.NEW(FT.TN.DEBIT.AMOUNT)
        Y.CREDIT.CURRENCY       = R.NEW(FT.TN.CREDIT.CURRENCY)
        Y.CREDIT.VALUE.DATE     = R.NEW(FT.TN.CREDIT.VALUE.DATE)
        Y.L.LOAN.COND           = R.NEW(FT.TN.L.LOAN.COND)
        Y.L.LOAN.STATUS.1       = R.NEW(FT.TN.L.LOAN.STATUS.1)
        Y.L.INITIAL.ID          = R.NEW(FT.TN.L.INITIAL.ID)
        Y.CREDIT.THEIR.REF      = ID.NEW

        Y.ACTUAL.VERSION = '' ; Y.NEXT.VERSION = ''; Y.VERSION = '' ; Y.N.VERSION = ''
        Y.ACTUAL.VERSION = R.NEW(FT.TN.L.ACTUAL.VERSIO)
        Y.VERSION = FIELD(Y.ACTUAL.VERSION,',',2)
        Y.NEXT.VERSION   = R.NEW(FT.TN.L.NEXT.VERSION)
        Y.N.VERSION   = FIELD(Y.NEXT.VERSION,',',2)
        REP.CHAR = '?'
        IF Y.VERSION THEN
            Y.L.ACTUAL.VERSIO = 'FUNDS.TRANSFER':REP.CHAR:Y.VERSION
        END
        IF Y.N.VERSION THEN
            Y.L.NEXT.VERSION  = 'FUNDS.TRANSFER':REP.CHAR:Y.N.VERSION
        END

        Y.OFS.BDY = 'TRANSACTION.TYPE::=':Y.TRANSACTION.TYPE:',DEBIT.ACCT.NO::=':Y.DEBIT.ACCT.NO:',DEBIT.AMOUNT::=':Y.DEBIT.AMOUNT:',DEBIT.CURRENCY::=':Y.DEBIT.CURRENCY:',DEBIT.VALUE.DATE::=':Y.DEBIT.VALUE.DATE:',DEBIT.THEIR.REF::=':Y.DEBIT.THEIR.REF:',CREDIT.ACCT.NO::=':Y.CREDIT.ACCT.NO:',CREDIT.AMOUNT::=':Y.CREDIT.AMOUNT:',CREDIT.CURRENCY::=':Y.CREDIT.CURRENCY:',CREDIT.VALUE.DATE::=':Y.CREDIT.VALUE.DATE:',CREDIT.THEIR.REF::=':Y.CREDIT.THEIR.REF
        Y.OFS.BDY :=',L.ACTUAL.VERSIO::=':Y.L.ACTUAL.VERSIO:',L.NEXT.VERSION::=':Y.L.NEXT.VERSION:',L.LOAN.COND::=':Y.L.LOAN.COND:',L.LOAN.STATUS.1::=':Y.L.LOAN.STATUS.1:',L.INITIAL.ID::=':Y.L.INITIAL.ID

        BEGIN CASE
        CASE Y.VERSION.NAME EQ 'REDO.FT.TT.TRANSACTION,L.APAP.TRANS.DOLARES' OR Y.VERSION.NAME EQ 'REDO.FT.TT.TRANSACTION,L.APAP.PART.TRANS.ACH.DOLARES'
            Y.L.FT.COMPANY        = R.NEW(FT.TN.L.FT.COMPANY)
            OFSVERSION = "FUNDS.TRANSFER,":Y.VERSION
            Y.OFS.BDY :=',L.FT.COMPANY::=':Y.L.FT.COMPANY
        CASE 1

        END CASE

        IF OFSVERSION THEN
            GOSUB POST.FT.TXN
        END
    END

    RETURN
*===========
POST.FT.TXN:
*===========
    FN.USER = 'F.USER'
    F.USER = ''
    CALL OPF(FN.USER,F.USER)

    Y.USR = OPERATOR
    CALL F.READ(FN.USER,Y.USR,R.USR,F.USER,ERR.US)
    OFS.USERNAME = R.USR<EB.USE.SIGN.ON.NAME>
    OFS.PASSWORD = R.USR<EB.USE.PASSWORD>
    Y.OFS.MSG = ''
    Y.OFS.MSG = OFSVERSION:'/I/PROCESS,':OFS.USERNAME:'/':OFS.PASSWORD:'/':ID.COMPANY:',,':Y.OFS.BDY

    ofsRequest = Y.OFS.MSG
    CALL ofs.addLocalRequest(ofsRequest,'add',error)
    MSG.OUT = ofsRequest

    RETURN

END
