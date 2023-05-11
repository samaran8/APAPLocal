* @ValidationCode : Mjo2MDE1ODQ3Mjc6Q3AxMjUyOjE2ODMwODE3MDIzOTg6SVRTUzotMTotMTowOjE6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 03 May 2023 08:11:42
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM


SUBROUTINE REDO.NV.DELETE.PROC
*
* =============================================================================
*
*  Used when a TT transaction part of a transaction chain is DELETED
*
*  Updates TELLER.ID balances
*  Updates REDO.TRANSACTION.CHAIN balances and status
*
*=======================================================================
*
*    First Release : Joaquin Costa
*    Developed for : APAP
*    Developed by  : Joaquin Costa
*    Date          : 2011 / MAY / 04
*
*Modification History:
*DATE                 WHO                  REFERENCE                     DESCRIPTION
*12/04/2023      CONVERSION TOOL     AUTO R22 CODE CONVERSION        FM TO @FM, VM TO @VM
*12/04/2023         SURESH           MANUAL R22 CODE CONVERSION           NOCHANGE
*=======================================================================
*
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_System
*
    $INSERT I_F.VERSION
    $INSERT I_F.TELLER
    $INSERT I_F.TELLER.ID
    $INSERT I_F.TELLER.TRANSACTION
*
    $INSERT I_F.REDO.MULTITXN.PARAMETER
    $INSERT I_F.REDO.TRANSACTION.CHAIN
    $INSERT I_REDO.NV.COMMON
*
*
* DEBUG
*
    GOSUB INITIALISE
    GOSUB OPEN.FILES
    GOSUB CHECK.PRELIM.CONDITIONS
    IF PROCESS.GOAHEAD THEN
        GOSUB PROCESS
    END
*
RETURN
*
* ---------------
PROCESS:
*  --------------
*
    GOSUB CHECK.IF.CHAIN.DELETED
*
    WVCCY     = RAISE(RAISE(R.TELLER.ID<TT.TID.LOCAL.REF,WCCY>))
    WVALCASH  = RAISE(RAISE(R.TELLER.ID<TT.TID.LOCAL.REF,WCASH>))
    WVALCHECK = RAISE(RAISE(R.TELLER.ID<TT.TID.LOCAL.REF,WCHECK>))

    LOCATE WTM.MONEDA IN WVCCY<1> SETTING YPOS THEN
        WEFECTIVO = WVALCASH<YPOS>
        WCHEQUE   = WVALCHECK<YPOS>
    END
*
    GOSUB UPDATE.TELLER.ID
*
    GOSUB UPDATE.REDO.TRANSACTION.CHAIN
*
RETURN
*
* =====================
CHECK.IF.CHAIN.DELETED:
* =====================
*
    LOCATE ID.NEW IN R.REDO.TRANSACTION.CHAIN<RTC.TRANS.ID,1> SETTING Y.POS THEN
        R.REDO.TRANSACTION.CHAIN<RTC.TRANS.STATUS,Y.POS>   = "DEL"
    END
*
    WDEL.FLAG = 1
    WNUM.TR = DCOUNT(R.REDO.TRANSACTION.CHAIN<RTC.TRANS.ID>,@VM)
    FOR IXX = 1 TO WNUM.TR
        IF R.REDO.TRANSACTION.CHAIN<RTC.TRANS.STATUS,IXX> NE "DEL" THEN
            WDEL.FLAG = ""
            IXX = WNUM.TR + 1
        END
    NEXT IXX
*
RETURN
*
* ---------------
UPDATE.TELLER.ID:
*  --------------
*
    RTR= ""
*
    CALL F.READU(FN.TELLER.ID,WTT.ID,R.TELLER.ID,F.TELLER.ID,ERR.MSJ,RTR)
    R.TELLER.ID<TT.TID.LOCAL.REF,WPOSLI>    = WINITIAL.ID
    LOCATE WTM.MONEDA IN WVCCY<1> SETTING YPOS THEN
        IF WTM.TYPE EQ "CHECK" THEN
            WVALCHECK<YPOS> += WTRAN.AMOUNT
        END ELSE
            WVALCASH<YPOS> += WTRAN.AMOUNT
        END
    END ELSE
        WVCCY<-1> = WTM.MONEDA
        IF WTM.TYPE EQ "CHECK" THEN
            WVALCHECK<-1> = WTRAN.AMOUNT
        END ELSE
            WVALCASH<-1> = WTRAN.AMOUNT
        END
    END
    R.TELLER.ID<TT.TID.LOCAL.REF,WCCY>   = LOWER(LOWER(WVCCY))
    R.TELLER.ID<TT.TID.LOCAL.REF,WCASH>  = LOWER(LOWER(WVALCASH))
    R.TELLER.ID<TT.TID.LOCAL.REF,WCHECK> = LOWER(LOWER(WVALCHECK))
*
    IF WDEL.FLAG THEN
        R.TELLER.ID<TT.TID.LOCAL.REF,WPOSLI> = ""
        R.TELLER.ID<TT.TID.LOCAL.REF,WCCY>   = ""
    END
*
    CALL F.WRITE(FN.TELLER.ID,WTT.ID,R.TELLER.ID)
*
RETURN
*
* ----------------------------
UPDATE.REDO.TRANSACTION.CHAIN:
* ----------------------------
    RTR         = ""
    CALL F.READU(FN.REDO.TRANSACTION.CHAIN,WINITIAL.ID,R.REDO.TRANSACTION.CHAIN,F.REDO.TRANSACTION.CHAIN,ERR.MSJ,RTR)
    LOCATE ID.NEW IN R.REDO.TRANSACTION.CHAIN<RTC.TRANS.ID,1> SETTING Y.POS THEN
        R.REDO.TRANSACTION.CHAIN<RTC.TRANS.STATUS,Y.POS>   = "DEL"
    END
    R.REDO.TRANSACTION.CHAIN<RTC.TRANS.AUTH>   = "P"
*
    IF WDEL.FLAG THEN
        R.REDO.TRANSACTION.CHAIN<RTC.TRANS.AUTH>   = "D"
    END
*
    GOSUB UPDATE.CHANGE.BALANCE

    GOSUB UPDATE.CASHIER.BALANCES
*
    CALL F.WRITE(FN.REDO.TRANSACTION.CHAIN,WINITIAL.ID,R.REDO.TRANSACTION.CHAIN)
*
RETURN
*
*--------------------
UPDATE.CHANGE.BALANCE:
* -------------------
*
    IF WTRAN.AMOUNT LT 0 THEN
        IF WTM.TYPE EQ "CASH" THEN
            R.REDO.TRANSACTION.CHAIN<RTC.TOTAL.CASH>   += WTRAN.AMOUNT
        END ELSE
            R.REDO.TRANSACTION.CHAIN<RTC.TOTAL.CHECK>  += WTRAN.AMOUNT
        END
    END
*
    IF WTM.TYPE EQ "CASH" THEN
        R.REDO.TRANSACTION.CHAIN<RTC.CASH.BALANCE>  += WTRAN.AMOUNT
    END ELSE
        R.REDO.TRANSACTION.CHAIN<RTC.CHECK.BALANCE> += WTRAN.AMOUNT
    END
*
RETURN
*
* ======================
UPDATE.CASHIER.BALANCES:
* ======================
*
    WVCCY     = RAISE(R.TELLER.ID<TT.TID.LOCAL.REF,WCCY>)
    WVALCASH  = RAISE(R.TELLER.ID<TT.TID.LOCAL.REF,WCASH>)
    WVALCHECK = RAISE(R.TELLER.ID<TT.TID.LOCAL.REF,WCHECK>)
*
    R.REDO.TRANSACTION.CHAIN<RTC.CCY.INFO>   = WVCCY
    R.REDO.TRANSACTION.CHAIN<RTC.CASH.INFO>  = WVALCASH
    R.REDO.TRANSACTION.CHAIN<RTC.CHECK.INFO> = WVALCHECK
*
    CCY.BALANCE   = WVCCY
    CASH.BALANCE  = WVALCASH
    CHECK.BALANCE = WVALCHECK
*
RETURN
*
* ================
CONTROL.MSG.ERROR:
* ================
*
*   Paragraph that control the error in the subroutine
*
    IF Y.ERR.MSG THEN
        ETEXT           = Y.ERR.MSG
        CALL STORE.END.ERROR
        ETEXT           = ""
    END
*
RETURN
*
* =========
INITIALISE:
* =========
*
    PROCESS.GOAHEAD    = 1
*
    Y.ERR.MSG          = ""
*
    FN.TELLER.TRANSACTION = "F.TELLER.TRANSACTION"
    F.TELLER.TRANSACTION  = ""

    FN.REDO.TRANSACTION.CHAIN = "F.REDO.TRANSACTION.CHAIN"
    F.REDO.TRANSACTION.CHAIN  = ""


    FN.TELLER.ID = "F.TELLER.ID"
    F.TELLER.ID  = ""

    FN.REDO.MULTITXN.PARAMETER = "F.REDO.MULTITXN.PARAMETER"
    F.REDO.MULTITXN.PARAMETER  = ""

    FN.TELLER = "F.TELLER"
    F.TELLER  = ""
*
    WAPP.LST = "TELLER.ID" : @FM : "TELLER"
    WCAMPO    = "L.INITIAL.ID"
    WCAMPO<2> = "L.CH.CASH"
    WCAMPO<3> = "L.CH.CHECK"
    WCAMPO<4> = "L.CH.CCY"
    WCAMPO    = CHANGE(WCAMPO,@FM,@VM)
    WFLD.LST  = WCAMPO
    WCAMPO    = "L.NEXT.VERSION"
    WCAMPO<2> = "L.ACTUAL.VERSIO"
    WCAMPO<3> = "L.TRAN.AMOUNT"
    WCAMPO<4> = "L.INITIAL.ID"
    WCAMPO    = CHANGE(WCAMPO,@FM,@VM)
    WFLD.LST := @FM : WCAMPO
    CALL MULTI.GET.LOC.REF(WAPP.LST,WFLD.LST,YPOS)
    WPOSLI    = YPOS<1,1>
    WCASH     = YPOS<1,2>
    WCHECK    = YPOS<1,3>
    WCCY      = YPOS<1,4>
*
    WPOSNV    = YPOS<2,1>
    WPOSACV   = YPOS<2,2>
    WPOSAMT   = YPOS<2,3>
    WPOS.LI   = YPOS<2,4>
*
    WTT.TRANS    = R.NEW(TT.TE.TRANSACTION.CODE)
    WTT.ID       = R.NEW(TT.TE.TELLER.ID.1)
*
    NEXT.VERSION = R.NEW(TT.TE.LOCAL.REF)<1,WPOSNV>
    WINITIAL.ID  = R.NEW(TT.TE.LOCAL.REF)<1,WPOS.LI>
    WTRAN.AMOUNT = R.NEW(TT.TE.LOCAL.REF)<1,WPOSAMT> * -1
*
    WEFECTIVO    = 0
    WCHEQUE      = 0
*
RETURN
*
* ---------------
OPEN.FILES:
* ---------------
*
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
                IF WINITIAL.ID EQ "" THEN
                    PROCESS.GOAHEAD = ""
                END

            CASE LOOP.CNT EQ 2
                CALL F.READ(FN.TELLER.ID,WTT.ID,R.TELLER.ID,F.TELLER.ID,ERR.MSJ)
                IF NOT(R.TELLER.ID) THEN
                    PROCESS.GOAHEAD = ""
                END

            CASE LOOP.CNT EQ 3
                CALL F.READ(FN.REDO.TRANSACTION.CHAIN,WINITIAL.ID,R.REDO.TRANSACTION.CHAIN,F.REDO.TRANSACTION.CHAIN,ERR.MSJ)
                IF ERR.MSJ THEN
                    Y.ERR.MSG       = ERR.MSJ
                    AF              = TT.TE.AMOUNT.LOCAL.1
                    PROCESS.GOAHEAD = ""
                END ELSE
                    LOCATE ID.NEW IN R.REDO.TRANSACTION.CHAIN<RTC.TRANS.ID,1> SETTING Y.POS THEN
                        WTM.MONEDA  = R.REDO.TRANSACTION.CHAIN<RTC.TRANS.CCY,Y.POS>
                        WTM.TYPE    = R.REDO.TRANSACTION.CHAIN<RTC.TRANS.TYPE,Y.POS>
                    END ELSE
                        PROCESS.GOAHEAD = ""
                    END
                END

        END CASE
*       Message Error
        GOSUB CONTROL.MSG.ERROR
*       Increase
        LOOP.CNT += 1
*
    REPEAT
*
RETURN
*
END
