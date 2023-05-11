* @ValidationCode : MjotOTQxNjg5OTE2OkNwMTI1MjoxNjgwNzU1ODgzODEwOklUU1M6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 06 Apr 2023 10:08:03
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
SUBROUTINE REDO.FI.EXT.DEBIT.PROCES(R.PARAMS, OUT.RESP,OUT.ERR)
******************************************************************************
*     Routine to exec a FT transaccion, from Client Account to Internal Account
*     Copy file from one directory to another
* =============================================================================
*
*    First Release : R09
*    Developed for : APAP
*    Developed by  : MGUDINO
*    Date          : 2011/Apr/06
*=======================================================================
* Modifications:
* 27/01/2012 - avelasco@temenos.com
*              APAP C18 :
*              Modifications
** 06-04-2023 R22 Auto Conversion - FM TO @FM, VM to @VM, SM to @SM
** 06-04-2023 Skanda R22 Manual Conversion - line no 384
*=======================================================================
*
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_F.TELLER
    $INSERT I_F.REDO.INTERFACE.PARAM
    $INSERT I_F.REDO.FI.CONTROL
    $INSERT I_F.COMPANY
    $INSERT I_F.AC.ENTRY.PARAM
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.ACCOUNT
    $INSERT I_REDO.FI.VARIABLES.COMMON

    $INSERT I_RAPID.APP.DEV.COMMON ;* R22 Auto conversion
    $INSERT I_RAPID.APP.DEV.EQUATE ;* R22 Auto conversion
* Tus Start
    $INSERT I_F.EB.CONTRACT.BALANCES
* Tus End

*
*************************************************************************
*

    GOSUB INITIALISE
    GOSUB OPEN.FILES

    GOSUB CHECK.PRELIM.CONDITIONS

    IF PROCESS.GOAHEAD THEN
        GOSUB PROCESS.FT
    END
*

    IF WERROR.MSG THEN
        PROCESS.GOAHEAD = 0
        OUT.ERR         = WERROR.MSG
    END ELSE
        CALL F.READ(FN.FUNDS.TRANSFER,OUT.RESP,R.FUNDS.TRANSFER,F.FUNDS.TRANSFER,FUNDS.TRANSFER.ERR)
        IF NOT(R.FUNDS.TRANSFER) THEN
            OUT.ERR = 'TRANSACTION FAILED'
            CALL F.READ(FN.FUNDS.TRANSFER.NAU,OUT.RESP,R.FUNDS.TRANSFER.NAU,F.FUNDS.TRANSFER.NAU,FT.NAU.ERR)
            IF R.FUNDS.TRANSFER.NAU<FT.RECORD.STATUS> EQ 'INAO' THEN
                OUT.ERR = 'MOVED TO INAO STATUS'
            END
        END
    END

*
RETURN
*
* ==============
PROCESS.FT:
* ---------
*


    COMM.INTERNAL.ACCT = W.CUENTA.CREDITO
* Validate, Amount in Accounts

    GOSUB ANALYZE.ACC.INFO
    GOSUB ACCT.STATUS.CHECK
    GOSUB MOVE.FUNDS
RETURN

* ================
ACCT.STATUS.CHECK:
* ================

    IF R.PARAMS<12> EQ 'NOMINA' AND NOT(WERROR.MSG) THEN
        IN.ACCT.TYPE = 'CREDIT'
        IN.ACCT.ID   = W.CUENTA.CREDITO
        CALL REDO.NOMINA.ACCT.STATUS.CHECK(IN.ACCT.ID,IN.ACCT.TYPE,OUT.ACCT.STATUS)
        WERROR.MSG = OUT.ACCT.STATUS
    END
    IF R.PARAMS<12> EQ 'NOMINAEXT' AND NOT(WERROR.MSG) THEN
        IN.ACCT.TYPE = 'DEBIT'
        IN.ACCT.ID   = COMM.CUST.ACCT
        CALL REDO.NOMINA.ACCT.STATUS.CHECK(IN.ACCT.ID,IN.ACCT.TYPE,OUT.ACCT.STATUS)
        WERROR.MSG = OUT.ACCT.STATUS
    END



RETURN


* ====================
ANALYZE.ACC.INFO:
* ====================
* --------------
*   Analyze Customer Debit Account
*
    COMM.CUST.ACCT =  R.PARAMS<6>
*
    GOSUB  VALID.DEBIT.ACCT
*
*
RETURN
*

* ====================
VALID.DEBIT.ACCT:
* ====================
*
*   Get Currency and balance from account
*


    CALL CACHE.READ(FN.ACCOUNT,COMM.CUST.ACCT,R.ACCOUNT, YER.CTA)
* Tus Start
    R.ECB = ''
    ECB.ERR = ''
    CALL EB.READ.HVT("EB.CONTRACT.BALANCES",COMM.CUST.ACCT,R.ECB,ECB.ERR)       ;* Tus End
    IF YER.CTA THEN
        WERROR.MSG = "AC-AC.REC.MISS":@FM:COMM.CUST.ACCT
    END ELSE
        COMM.CURRENCY = R.ACCOUNT<AC.CURRENCY>
        Y.NOM.TYPE    = R.PARAMS<12>

        IF Y.NOM.TYPE EQ 'NOMINAEXTNOTAX' THEN
*      YWORK.BALANCE = R.ACCOUNT<AC.WORKING.BALANCE>
*      Tus Start
            YWORK.BALANCE = R.ECB<ECB.WORKING.BALANCE>
*      Tus End
        END ELSE
            YWORK.BALANCE = R.ACCOUNT<Y.POS.LF.ACC,Y.POS.AV.BAL>
        END


        IF Y.NOM.TYPE EQ 'NOMINAEXT' OR Y.NOM.TYPE EQ 'NOMINAEXTNOTAX' THEN
            IF NOT(FI.PAYMENT.REF) THEN
                IF F.TOT.AMOUNT GT YWORK.BALANCE THEN
*                WERROR.MSG = "EB-Not.enough.BALANCE.in.&.account":FM:COMM.CUST.ACCT
                    WERROR.MSG = 'INSUFFICIENT BALANCE'
                END
            END
        END

    END
*
RETURN
*

* ===============
MOVE.FUNDS:
* ===============
*
    R.FUNDS.TRANSFER                      = ""
    ADDNL.INFO                            = ""
*
    R.FUNDS.TRANSFER<FT.TRANSACTION.TYPE> = R.PARAMS<13>
    Y.ACC.VALUE = R.PARAMS<6>
    R.FUNDS.TRANSFER<FT.DEBIT.ACCT.NO>   = Y.ACC.VALUE

    IF COMM.CURRENCY EQ '' THEN
        COMM.CURRENCY = R.COMPANY(EB.COM.LOCAL.CURRENCY)
    END
* Debit will always use a internal account to debit
    R.FUNDS.TRANSFER<FT.CREDIT.ACCT.NO>    = W.CUENTA.CREDITO
    COMM.CURRENCY =R.PARAMS<8>

    F.CREDIT.AMOUNT = R.PARAMS<7>


    IF WERROR.MSG THEN
        COMM.VERSION = 'NOMINAERR'
        R.FUNDS.TRANSFER<FT.DEBIT.AMOUNT> = F.CREDIT.AMOUNT
    END ELSE
        COMM.VERSION = R.PARAMS<12>
    END

    IF R.PARAMS<12> EQ 'NOMINAEXT' AND  R.ACCOUNT<AC.CUSTOMER> EQ '' THEN
        COMM.VERSION = 'NOMINAEXTNOTAX'
        R.FUNDS.TRANSFER<FT.COMMISSION.CODE> = 'WAIVE'
        R.FUNDS.TRANSFER<FT.CHARGE.CODE> = 'WAIVE'
    END

    IF COMM.VERSION EQ 'NOMINAINT' OR COMM.VERSION EQ 'NOMINAEXT' OR COMM.VERSION EQ 'NOMINAEXTNOTAX' THEN
        R.FUNDS.TRANSFER<FT.DEBIT.AMOUNT> = F.CREDIT.AMOUNT
        R.FUNDS.TRANSFER<FT.DEBIT.CURRENCY>  = COMM.CURRENCY
    END ELSE
        R.FUNDS.TRANSFER<FT.CREDIT.AMOUNT>    = F.CREDIT.AMOUNT
        R.FUNDS.TRANSFER<FT.CREDIT.CURRENCY>  = COMM.CURRENCY
    END
    W.DEBIT.THEIR.REF = R.PARAMS<9>
    R.FUNDS.TRANSFER<FT.PAYMENT.DETAILS>  = R.PARAMS<14>
    R.FUNDS.TRANSFER<FT.LOCAL.REF,Y.L.COMMENTS.POS> = W.DEBIT.THEIR.REF
    R.FUNDS.TRANSFER<FT.ORDERING.BANK>    = 'NOMINA'

*

    ADDNL.INFO<1,1> = COMM.VERSION
    ADDNL.INFO<1,2> = "I"
*
    ADDNL.INFO<2,1>  = "PROCESS"
    ADDNL.INFO<2,2>  = COMM.USER : "/" : COMM.PW
    ADDNL.INFO<2,3>  = ID.COMPANY
    ADDNL.INFO<2,4>  = ""     ;*    Transaction ID
    ADDNL.INFO<2,5>  = 1
    ADDNL.INFO<2,6>  = "0"    ;*   Authorization Number
*
    R.FUNDS.TRANSFER<FT.ORDERING.BANK>="NOMINA"
    Y.OFS.STR = DYN.TO.OFS(R.FUNDS.TRANSFER,'FUNDS.TRANSFER',ADDNL.INFO)
    YWORK.CH  = COMM.USER : "//"
    YWORK.NEW = COMM.USER : "/" : COMM.PW : "/"
    CHANGE YWORK.CH TO YWORK.NEW IN Y.OFS.STR
*
    OFS.RESP   = ""
    TXN.COMMIT = ""
    YERROR.POS = 0
    OFS.SOURCE.ID = "TAM.OFS.SRC"
    OFS.MSG.ER = ""
*

    CALL OFS.CALL.BULK.MANAGER(OFS.SOURCE.ID, Y.OFS.STR, OFS.RESP, TXN.COMMIT)

    M.VALIDA = FIELD(OFS.RESP,"/",1)
    OUT.ERR = ""
    OUT.ERR<1> = OFS.RESP
    OUT.RESP<1> = M.VALIDA
    YERROR.POS = INDEX(OFS.RESP,"-1",1)

    IF YERROR.POS GT 0 THEN
        GOSUB FORMAT.MSG.ERR
    END



*
RETURN

* =============
FORMAT.MSG.ERR:
* =============
*
*   Formated msg error
*
    OUT.ERR<2> = "ERROR"
    Y.VAR = FIELD(OFS.RESP,'-1/NO,',2) ;* R22 Auto conversion
    Z = INDEX(Y.VAR,",",1) - 1 ;* R22 Auto conversion
    IF Z EQ '-1' THEN ;* R22 Auto conversion
        Z = 50
    END

    OUT.ERR<3> ="ERROR"
    IF Y.VAR NE '' THEN ;* R22 Auto conversion
        OUT.ERR<4>=Y.VAR ;* R22 Auto conversion
    END
*
RETURN
*

*-----------------------
CHECK.PRELIM.CONDITIONS:
*-----------------------
*
    LOOP.CNT  = 1   ;   MAX.LOOPS = 3
*

    LOOP
    WHILE LOOP.CNT LE MAX.LOOPS AND PROCESS.GOAHEAD DO
        BEGIN CASE
            CASE LOOP.CNT EQ 1
* Get THE FIRST DOT
                Y.POS = INDEX(WCONTROL.ID, '.', 1)
                Y.POS -= 1 ;* R22 Auto conversion
* Get the Company ID

                Y.INTERFACE = R.PARAMS<5>
                Y.POS += 2 ;* R22 Auto conversion
* Get the Company ID
                Y.COMPANY.ID = SUBSTRINGS (WCONTROL.ID, Y.POS, 4)

                IF Y.INTERFACE[4,6] NE 'NOMINA' THEN
                    PROCESS.GOAHEAD = 0
                    E = "EB-NOT.NOMINA"
                    OUT.ERR = E
                    RETURN
                END

            CASE LOOP.CNT EQ 2

                CALL F.READ(FN.REDO.INTERFACE.PARAM, Y.INTERFACE, R.REDO.INTERFACE.PARAM, F.REDO.INTERFACE.PARAM, Y.ERR)
                IF Y.ERR THEN
                    PROCESS.GOAHEAD = 0
                    E = "EB-PARAMETER.MISSING"
                    CALL ERR
                END

            CASE LOOP.CNT EQ 3
                RIP.USER     = R.REDO.INTERFACE.PARAM<REDO.INT.PARAM.FI.USER.SIGN.ON>
                RIP.PASSWORD = R.REDO.INTERFACE.PARAM<REDO.INT.PARAM.FI.USER.PWD>
                COMM.USER = RIP.USER
                COMM.PW   = RIP.PASSWORD


*
        END CASE
        LOOP.CNT +=1
    REPEAT
*
RETURN

* ---------
INITIALISE:
* ---------
*
    PROCESS.GOAHEAD        = 1          ;*No problems
    F.TRANSACTION.ID = R.PARAMS<1>
    F.VAL.TRX.ENT = R.PARAMS<2>
    F.TOT.AMOUNT = R.PARAMS<3>
    WCONTROL.ID = R.PARAMS<4>

    W.INDICADOR.PROCESO = R.PARAMS<10>
    W.CUENTA.CREDITO = R.PARAMS<11>


    Y.ACC.VALUE = ''
    WPARAM.POS  = 1
    Y.COMPANY.ID = ""
    Y.INTERFACE = ""

    FN.TELLER = "F.TELLER"
    F.TELLER = ""

    FN.REDO.INTERFACE.PARAM = "F.REDO.INTERFACE.PARAM"
    F.REDO.INTERFACE.PARAM = ""

    FN.AC.ENTRY.PARAM = "F.AC.ENTRY.PARAM"
    F.AC.ENTRY.PARAM = ""

    FN.ACCOUNT = "F.ACCOUNT"
    F.ACCOUNT = ""

    FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER'
    F.FUNDS.TRANSFER  = ''
    R.FUNDS.TRANSFER = ''

    FN.FUNDS.TRANSFER.NAU = 'F.FUNDS.TRANSFER$NAU'
    F.FUNDS.TRANSFER.NAU = ''
    R.FUNDS.TRANSFER.NAU = ''

    WERROR.MSG = ""
    OUT.ERR = ""

    Y.POS.LF.ACC           = AC.LOCAL.REF
    Y.POS.CCY.ACC          = AC.CURRENCY
    Y.POS.AV.BAL           = ""

    LREF.POS = ''
    LREF.APP = 'FUNDS.TRANSFER':@FM:'ACCOUNT'
    LREF.FIELD = 'L.COMMENTS':@FM:'L.AC.AV.BAL'
    CALL APAP.TAM.MULTI.GET.LOC.REF(LREF.APP,LREF.FIELD,LREF.POS) ;* R22 Manual conversion
    Y.L.COMMENTS.POS = LREF.POS<1,1>
    Y.POS.AV.BAL = LREF.POS<2,1>

RETURN
*-------------------
* ---------
OPEN.FILES:
* ---------
*
*   Paragraph that open files
*
*   OPEN  TELLER
    CALL OPF(FN.TELLER,F.TELLER)
*
*   OPEN  ACCOUNT
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
*
*   OPEN  REDO.IN.PARAM
    CALL OPF(FN.REDO.INTERFACE.PARAM,F.REDO.INTERFACE.PARAM)
*
*   OPEN  AC.ENTRY.PARAM
    CALL OPF(FN.AC.ENTRY.PARAM,F.AC.ENTRY.PARAM)

    CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)

    CALL OPF(FN.FUNDS.TRANSFER.NAU,F.FUNDS.TRANSFER.NAU)
*
RETURN
*

*-------- END -------

END
