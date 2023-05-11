* @ValidationCode : MjoxMDczNDQ4NTA2OkNwMTI1MjoxNjgxODEzMjEwNDA4OjMzM3N1Oi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 18 Apr 2023 15:50:10
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
SUBROUTINE REDO.VAU.HIS.ID.UPDATE
*------------------------------------------------------------------------------------------------------
*DESCRIPTION
* routine to update the FT and TT record id and the selection fields in the live template
*REDO.FT.HIS and REDO.TT.HIS

*------------------------------------------------------------------------------------------------------
*APPLICATION
* VERSION.CONTROL auth routine to be attached in FT and TT
*-------------------------------------------------------------------------------------------------------

*
* Input / Output
* --------------
* IN     : -NA-
* OUT    : -NA-
*
*----------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : Temenos Application Management
* PROGRAM NAME : REDO.VAU.HIS.ID.UPDATE
*----------------------------------------------------------
* Modification History :
*-----------------------
*DATE             WHO         REFERENCE         DESCRIPTION
*23.08.2010      Janani     ODR-2011-03-0113  INITIAL CREATION
*
*Modification History:
*DATE                 WHO                  REFERENCE                     DESCRIPTION
*18/04/2023      CONVERSION TOOL     AUTO R22 CODE CONVERSION        F.READ TO CACHE.READ, FM TO @FM
*18/04/2023         SURESH           MANUAL R22 CODE CONVERSION           NOCHANGE
* ----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.TT.HIS
    $INSERT I_F.REDO.FT.HIS
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.TELLER
    $INSERT I_F.FT.TXN.TYPE.CONDITION
    $INSERT I_F.TELLER.TRANSACTION
    GOSUB INIT
    GOSUB PROCESS
RETURN

*------------------------------------------------------------
INIT:
*------------------------------------------------------------
    FN.REDO.FT.HIS = 'F.REDO.FT.HIS'
    F.REDO.FT.HIS = ''
    CALL OPF(FN.REDO.FT.HIS,F.REDO.FT.HIS)

    FN.REDO.TT.HIS = 'F.REDO.TT.HIS'
    F.REDO.TT.HIS = ''
    CALL OPF(FN.REDO.TT.HIS,F.REDO.TT.HIS)

    LF.APP = 'FUNDS.TRANSFER':@FM:'TELLER':@FM:'FT.TXN.TYPE.CONDITION':@FM:'TELLER.TRANSACTION'
    LF.FLD = 'L.FT.CMPNY.NAME':@FM:'L.TT.CMPNY.NAME':@FM:'L.FTTC.CHANNELS':@FM:'L.TT.PAY.TYPE'
    LF.POS = ''

    FN.TELLER.ID = 'F.TELLER.ID'
    F.TELLER.ID = ''
    CALL OPF(FN.TELLER.ID,F.TELLER.ID)

    FN.TXN.TYPE.CONDITION = 'F.FT.TXN.TYPE.CONDITION'
    F.TXN.TYPE.CONDITION = ''
    CALL OPF(FN.TXN.TYPE.CONDITION,F.TXN.TYPE.CONDITION)

    FN.TELLER.TRANSACTION = 'F.TELLER.TRANSACTION'
    F.TELLER.TRANSACTION  = ''
    CALL OPF(FN.TELLER.TRANSACTION,F.TELLER.TRANSACTION)

    CALL MULTI.GET.LOC.REF(LF.APP,LF.FLD,LF.POS)
    FT.POS = LF.POS<1,1>
    TT.POS = LF.POS<2,1>
    LOC.L.FTTC.CHANNELS.POS = LF.POS<3,1>
    LOC.L.TT.PAY.TYPE.POS = LF.POS<4,1>

RETURN

*------------------------------------------------------------
PROCESS:
*------------------------------------------------------------
*
    BEGIN CASE

        CASE APPLICATION EQ 'FUNDS.TRANSFER'
            GOSUB PROCESS.FT

        CASE APPLICATION EQ 'TELLER'
            GOSUB PROCESS.TT

    END CASE

RETURN

*-----------
PROCESS.FT:
*-----------
*
    Y.INWARD.PAY.TYPE  = R.NEW(FT.INWARD.PAY.TYPE)
    Y.PAY.TYPE  = FIELD(Y.INWARD.PAY.TYPE,"-",1)
    Y.DATE = R.NEW(FT.DATE.TIME)
    Y.DATE = Y.DATE[1,6]
    Y.DATE.TIME = TODAY[1,2]:Y.DATE

    IF Y.PAY.TYPE EQ "STO" THEN
        R.REDO.FT.HIS<REDO.FT.HIS.TRANSACTION.TYPE> = R.NEW(FT.TRANSACTION.TYPE)
        R.REDO.FT.HIS<REDO.FT.HIS.DATE.TIME> = R.NEW(FT.DATE.TIME)
        R.REDO.FT.HIS<REDO.FT.HIS.DATE.OF.TXN> = Y.DATE.TIME
        R.REDO.FT.HIS<REDO.FT.HIS.CO.CODE> = R.NEW(FT.CO.CODE)
        R.REDO.FT.HIS<REDO.FT.HIS.INPUTTER> = R.NEW(FT.INPUTTER)
        R.REDO.FT.HIS<REDO.FT.HIS.L.FT.CMPNY.NAME> = R.NEW(FT.LOCAL.REF)<1,FT.POS>
        R.REDO.FT.HIS<REDO.FT.HIS.DEBIT.CURRENCY> = R.NEW(FT.DEBIT.CURRENCY)
        CALL F.WRITE(FN.REDO.FT.HIS,ID.NEW,R.REDO.FT.HIS)
    END ELSE
        VAL.TRANS.TYPE = R.NEW(FT.TRANSACTION.TYPE)
        CALL CACHE.READ(FN.TXN.TYPE.CONDITION, VAL.TRANS.TYPE, R.TXN.TYPE.CONDITION, CONDITION.ERR) ;*AUTO R22 CODE CONVERSION
        Y.FTTC.CHANNELS = R.TXN.TYPE.CONDITION<FT6.LOCAL.REF,LOC.L.FTTC.CHANNELS.POS>
        IF Y.FTTC.CHANNELS EQ 'PMT' THEN
            Y.USER = FIELD(R.NEW(FT.INPUTTER),"_",2)
            SEL.TT.ID = "SELECT ":FN.TELLER.ID:" WITH USER EQ ":Y.USER
            CALL EB.READLIST(SEL.TT.ID,SEL.TT.ID.LIST,'',NO.TT.ID,TT.ID.ERR)
            IF SEL.TT.ID.LIST EQ '' THEN
                R.REDO.FT.HIS<REDO.FT.HIS.TRANSACTION.TYPE> = R.NEW(FT.TRANSACTION.TYPE)
                R.REDO.FT.HIS<REDO.FT.HIS.DATE.TIME> = R.NEW(FT.DATE.TIME)
                R.REDO.FT.HIS<REDO.FT.HIS.DATE.OF.TXN> = Y.DATE.TIME
                R.REDO.FT.HIS<REDO.FT.HIS.CO.CODE> = R.NEW(FT.CO.CODE)
                R.REDO.FT.HIS<REDO.FT.HIS.INPUTTER> = R.NEW(FT.INPUTTER)
                R.REDO.FT.HIS<REDO.FT.HIS.L.FT.CMPNY.NAME> = R.NEW(FT.LOCAL.REF)<1,FT.POS>
                R.REDO.FT.HIS<REDO.FT.HIS.DEBIT.CURRENCY> = R.NEW(FT.DEBIT.CURRENCY)
                CALL F.WRITE(FN.REDO.FT.HIS,ID.NEW,R.REDO.FT.HIS)
            END
        END
    END


RETURN

*----------
PROCESS.TT:
*----------
*

    Y.TRANSACTION.TYPE = R.NEW(TT.TE.TRANSACTION.CODE)
    CALL CACHE.READ(FN.TELLER.TRANSACTION, Y.TRANSACTION.TYPE, R.TELLER.TRANSACTION, TELL.TRANS.ERR) ;*AUTO R22 CODE CONVERSION
    Y.TT.PAY.TYPE = R.TELLER.TRANSACTION<TT.TR.LOCAL.REF,LOC.L.TT.PAY.TYPE.POS>
    Y.DATE = R.NEW(TT.TE.DATE.TIME)
    Y.DATE = Y.DATE[1,6]
    Y.DATE.TIME = TODAY[1,2]:Y.DATE
    IF Y.TT.PAY.TYPE EQ '7' THEN
        R.REDO.TT.HIS<REDO.TT.HIS.TRANSACTION.CODE> = R.NEW(TT.TE.TRANSACTION.CODE)
        R.REDO.TT.HIS<REDO.TT.HIS.DATE.TIME> = R.NEW(TT.TE.DATE.TIME)
        R.REDO.TT.HIS<REDO.TT.HIS.DATE.OF.TXN> = Y.DATE.TIME
        R.REDO.TT.HIS<REDO.TT.HIS.CO.CODE> = R.NEW(TT.TE.CO.CODE)
        R.REDO.TT.HIS<REDO.TT.HIS.INPUTTER> = R.NEW(TT.TE.INPUTTER)
        R.REDO.TT.HIS<REDO.TT.HIS.L.TT.CMPNY.NAME> = R.NEW(TT.TE.LOCAL.REF)<1,TT.POS>
        R.REDO.TT.HIS<REDO.TT.HIS.CURRENCY.1> = R.NEW(TT.TE.CURRENCY.1)
        CALL F.WRITE(FN.REDO.TT.HIS,ID.NEW,R.REDO.TT.HIS)
    END


RETURN
*------------------------------------------------------------
END
