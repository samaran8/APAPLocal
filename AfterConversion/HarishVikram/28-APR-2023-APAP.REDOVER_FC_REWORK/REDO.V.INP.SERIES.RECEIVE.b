* @ValidationCode : Mjo1NDc1MjcwNzc6Q3AxMjUyOjE2ODI1MTE3NjAxMjE6SGFyaXNodmlrcmFtQzotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 26 Apr 2023 17:52:40
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.V.INP.SERIES.RECEIVE
*----------------------------------------------------------------------------
* Description:
* This routine will be attached to the version REDO.ORDER.DETAIL,ORDER.DELEVIRY as
* a input routine
*------------------------------------------------------------------------------------------
* * Input / Output
*
* --------------
* IN     : -NA-
* OUT    : -NA-
*------------------------------------------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : MARIMUTHU S
* PROGRAM NAME : REDO.V.INP.SERIES.CHECK
*------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
* DATE             WHO            REFERENCE         DESCRIPTION
* 12.04.2010  MARIMUTHU S     ODR-2009-11-0200  INITIAL CREATION
*Modification history
*Date                Who               Reference                  Description
*17-04-2023      conversion tool     R22 Auto code conversion    TNO:'_':OPERATOR TO C$T24.SESSION.NO:'_':OPERATOR,VM TO @VM,FM TO @FM
*17-04-2023      Mohanraj R          R22 Manual code conversion  CALL method format modified
* -----------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.COMPANY
    $INSERT I_F.USER
    $INSERT I_F.REDO.H.ORDER.DETAILS
    $INSERT I_GTS.COMMON
    $INSERT I_F.REDO.H.BANK.DRAFTS
    $INSERT I_F.REDO.H.ADMIN.CHEQUES
    $INSERT I_F.REDO.ITEM.STOCK.BY.DATE
    $INSERT I_F.REDO.H.PASSBOOK.INVENTORY
    $INSERT I_F.REDO.H.DEPOSIT.RECEIPTS
    $INSERT I_F.REDO.ITEM.STOCK
    $INSERT I_F.REDO.H.PIGGY.BANKS
    $INSERT I_F.REDO.H.MAIN.COMPANY
    $USING APAP.TAM
*-----------------------------------------------------------------------------------------
MAIN:
*-----------------------------------------------------------------------------------------

    GOSUB INIT
    GOSUB OPENFILES


    GOSUB PROCESS1


    GOSUB PROGRAM.END
*-----------------------------------------------------------------------------------------
INIT:
*-----------------------------------------------------------------------------------------
    Y.TOTAL.VAL = 0
    Y.DIFF = 0

RETURN
*-----------------------------------------------------------------------------------------
OPENFILES:
*-----------------------------------------------------------------------------------------

    FN.REDO.H.ORDER.DETAILS = 'F.REDO.H.ORDER.DETAILS'
    F.REDO.H.ORDER.DETAILS = ''
    CALL OPF(FN.REDO.H.ORDER.DETAILS,F.REDO.H.ORDER.DETAILS)
    FN.REDO.H.DEPOSIT.RECEIPTS = 'F.REDO.H.DEPOSIT.RECEIPTS'
    F.REDO.H.DEPOSIT.RECEIPTS = ''
    FN.REDO.H.ADMIN.CHEQUES = 'F.REDO.H.ADMIN.CHEQUES'
    F.REDO.H.ADMIN.CHEQUES = ''
    FN.REDO.H.BANK.DRAFTS = 'F.REDO.H.BANK.DRAFTS'
    F.REDO.H.BANK.DRAFTS = ''
    FN.REDO.H.PASSBOOK.INVENTORY = 'F.REDO.H.PASSBOOK.INVENTORY'
    F.REDO.H.PASSBOOK.INVENTORY = ''
    FN.REDO.ITEM.STOCK = 'F.REDO.ITEM.STOCK'
    F.REDO.ITEM.STOCK = ''
    FN.REDO.ITEM.STOCK.BY.DATE = 'F.REDO.ITEM.STOCK.BY.DATE'
    F.REDO.ITEM.STOCK.BY.DATE = ''
    FN.REDO.H.PIGGY.BANKS = 'F.REDO.H.PIGGY.BANKS'
    F.REDO.H.PIGGY.BANKS = ''
    CALL OPF(FN.REDO.H.PIGGY.BANKS,F.REDO.H.PIGGY.BANKS)

    FN.REDO.H.MAIN.COMPANY = 'F.REDO.H.MAIN.COMPANY'
    F.REDO.H.MAIN.COMPANY = ''
    CALL OPF(FN.REDO.H.MAIN.COMPANY,F.REDO.H.MAIN.COMPANY)

    CALL OPF(FN.REDO.ITEM.STOCK,F.REDO.ITEM.STOCK)
    CALL OPF(FN.REDO.ITEM.STOCK.BY.DATE,F.REDO.ITEM.STOCK.BY.DATE)

    Y.INV.MNT.ID = R.NEW(RE.ORD.ITEM.CODE)
    Y.SEQ.FROM = R.NEW(RE.ORD.SERIES.FROM)
    Y.SEQ.TO = R.NEW(RE.ORD.SERIES.TO)
    Y.REQ.COMP = R.NEW(RE.ORD.REQUEST.COMPANY)
    Y.DEL.QUL =  R.NEW(RE.ORD.RETURN.QUANTITY)
    INPUTTER = R.NEW(RE.ORD.INPUTTER)
    IF NOT(INPUTTER) THEN
        INPUTTER = C$T24.SESSION.NO:'_':OPERATOR ;*R22 Auto code conversion
    END
    AUTHORISER = C$T24.SESSION.NO:'_':OPERATOR ;*R22 Auto code conversion

    FN.REDO.ITEM.SERIES = 'F.REDO.ITEM.SERIES'
    F.REDO.ITEM.SERIES  = ''
    CALL OPF(FN.REDO.ITEM.SERIES,F.REDO.ITEM.SERIES)
    CALL APAP.TAM.redoCheckApplication(Y.INV.MNT.ID,APPL.NAME,APPL.PATH) ;* R22 Manual Conversion - CALL method format modified
    DUP.APPL.NAME = APPL.NAME
    DUP.APPL.PATH = APPL.PATH

    CALL OPF(APPL.NAME,APPL.PATH)
RETURN
*-------------------------------
REC.PIGGY:
*-------------------------------

    Y.UPD.FLAG = ''
    Y.SYSTEM = 'SYSTEM'
    Y.DES.DEPT = R.NEW(RE.ORD.BRANCH.CODE)
    IF Y.DES.DEPT THEN
        Y.BRANCH = ID.COMPANY:"-":Y.DES.DEPT
    END ELSE
        Y.BRANCH = ID.COMPANY
    END
    CALL CACHE.READ(FN.REDO.H.PIGGY.BANKS,Y.SYSTEM,R.REDO.H.PIGGY.BANKS,Y.ERRR)
    IF R.REDO.H.PIGGY.BANKS THEN
        Y.BRANCH = R.REDO.H.PIGGY.BANKS<REDO.PIG.BRANCH.DEPT>
        Y.COUNT.BRAN = DCOUNT(Y.BRANCH,@VM)
        Y.CNT.BR = 1
        LOOP
        WHILE Y.CNT.BR LE Y.COUNT.BRAN
            IF ID.COMPANY EQ R.REDO.H.PIGGY.BANKS<REDO.PIG.BRANCH.DEPT,Y.CNT.BR> THEN
                Y.CODE.VAL.1 = R.REDO.H.PIGGY.BANKS<REDO.PIG.BRANCH.DEPT,Y.CNT.BR>
                IF Y.DES.DEPT AND Y.DES.DEPT EQ R.REDO.H.PIGGY.BANKS<REDO.PIG.DEPT,Y.CNT.BR> THEN
                    R.REDO.H.PIGGY.BANKS<REDO.PIG.NO.AVBL,Y.CNT.BR> = R.REDO.H.PIGGY.BANKS<REDO.PIG.NO.AVBL,Y.CNT.BR> + R.NEW(RE.ORD.RETURN.QUANTITY)
                    Y.UPD.FLAG = '1'
                END
            END
            Y.CNT.BR += 1 ;*R22 Auto code conversion
        REPEAT
    END

    CURR.NO.VALUE = R.REDO.H.PIGGY.BANKS<REDO.PIG.CURR.NO>
    R.REDO.H.PIGGY.BANKS<REDO.PIG.CURR.NO> = CURR.NO.VALUE + 1
    INPUTTER = C$T24.SESSION.NO:'_':OPERATOR ;*R22 Auto code conversion
    AUTHORISER = C$T24.SESSION.NO:'_':OPERATOR ;*R22 Auto code conversion
    TEMPTIME = OCONV(TIME(),"MTS")
    TEMPTIME = TEMPTIME[1,5]
    CHANGE ':' TO '' IN TEMPTIME
    CHECK.DATE = DATE()
    DATE.TIME = OCONV(CHECK.DATE,"DY2"):OCONV(CHECK.DATE,"DM"):OCONV(CHECK.DATE,"DD"):TEMPTIME
    R.REDO.H.PIGGY.BANKS<REDO.PIG.INPUTTER> = INPUTTER
    R.REDO.H.PIGGY.BANKS<REDO.PIG.DATE.TIME> = DATE.TIME
    R.REDO.H.PIGGY.BANKS<REDO.PIG.AUTHORISER> = AUTHORISER

    CALL F.WRITE(FN.REDO.H.PIGGY.BANKS,Y.SYSTEM,R.REDO.H.PIGGY.BANKS)
RETURN
*-----------------------------------------------------------------------------------------
PROCESS1:
*-----------------------------------------------------------------------------------------
    IF DUP.APPL.NAME EQ 'F.REDO.H.PIGGY.BANKS' AND R.NEW(RE.ORD.REJECTED.ORDER) EQ 'YES' THEN
        GOSUB REC.PIGGY
        RETURN
    END
    IF R.NEW(RE.ORD.BRANCH.CODE) THEN
        Y.ITEM.STOCK.ID = ID.COMPANY:'-':R.NEW(RE.ORD.BRANCH.CODE)
    END ELSE
        Y.ITEM.STOCK.ID = ID.COMPANY
    END

    IF R.NEW(RE.ORD.BRANCH.CODE) THEN
        Y.ITEM.STOCK.RPT.ID = ID.COMPANY:'-':R.NEW(RE.ORD.BRANCH.CODE):".":R.NEW(RE.ORD.ITEM.CODE)
    END ELSE
        Y.ITEM.STOCK.RPT.ID = ID.COMPANY:".":R.NEW(RE.ORD.ITEM.CODE)
    END
    CALL F.READ(FN.REDO.ITEM.STOCK,Y.ITEM.STOCK.ID,R.REDO.ITEM.STOCK,F.REDO.ITEM.STOCK,Y.ERR.READ)
    CALL F.READ(FN.REDO.ITEM.STOCK.BY.DATE,Y.ITEM.STOCK.RPT.ID,R.REDO.ITEM.STOCK.BY.DATE,F.REDO.ITEM.STOCK.BY.DATE,Y.ERR.DATE)
    CALL F.READ(FN.REDO.ITEM.SERIES,Y.ITEM.STOCK.RPT.ID,R.REDO.ITEM.SERIES,F.REDO.ITEM.SERIES,Y.ERR.R)
    Y.CNT.TOT = DCOUNT(R.REDO.ITEM.SERIES,@FM)
    Y.SERIES.LIST.DUP = FIELDS(R.REDO.ITEM.SERIES,'*',1,1)
    Y.STATUS.LIST.DUP = FIELDS(R.REDO.ITEM.SERIES,'*',2,1)
    Y.BATCH.LIST.DUP = FIELDS(R.REDO.ITEM.SERIES,'*',3,1)

    Y.REJ.VAL = R.NEW(RE.ORD.REJECTED.ORDER)

    SEL.LIST.VAL = R.NEW(RE.ORD.RESERVED.5)


    IF Y.REJ.VAL EQ 'YES' THEN
        CHANGE ' ' TO @VM IN SEL.LIST.VAL
        Y.COUNT.SEL.LIST2 = DCOUNT(SEL.LIST.VAL,@VM)
        Y.CNT.VAL.CHECK = 1
        LOOP
        WHILE Y.CNT.VAL.CHECK LE Y.COUNT.SEL.LIST2
            SEL.LIST.2 = SEL.LIST.VAL<1,Y.CNT.VAL.CHECK>
            GOSUB CASE.FILE.SEL1
            Y.CNT.VAL.CHECK += 1
        REPEAT
        Y.ITEM.LIST = R.REDO.ITEM.STOCK<ITEM.REG.ITEM.CODE>
        LOCATE Y.INV.MNT.ID IN Y.ITEM.LIST<1,1> SETTING POS THEN
            Y.BAL = R.REDO.ITEM.STOCK<ITEM.REG.BAL,POS>
            Y.BAL.NEW = Y.BAL - Y.DEL.QUL
            R.REDO.ITEM.STOCK<ITEM.REG.BAL,POS> = Y.BAL.NEW
        END
        Y.OFS.VAL = OFS$OPERATION
        IF Y.OFS.VAL EQ 'PROCESS' THEN
            CALL F.WRITE(FN.REDO.ITEM.STOCK,Y.ITEM.STOCK.ID,R.REDO.ITEM.STOCK)
            CALL F.WRITE(FN.REDO.ITEM.SERIES,Y.ITEM.STOCK.RPT.ID,R.REDO.ITEM.SERIES)
            GOSUB UPD.ITEM.STOCK.RPT
        END
    END ELSE
        CHANGE ' ' TO @VM IN SEL.LIST.VAL
        Y.COUNT.SEL.LIST2 = DCOUNT(SEL.LIST.VAL,@VM)
        Y.CNT.VAL.CHECK = 1
        LOOP
        WHILE Y.CNT.VAL.CHECK LE Y.COUNT.SEL.LIST2
            SEL.LIST.2 = SEL.LIST.VAL<1,Y.CNT.VAL.CHECK>
            LOCATE SEL.LIST.2 IN Y.BATCH.LIST.DUP SETTING POS.SER THEN
                R.REDO.ITEM.SERIES<POS.SER> = ''
            END
            GOSUB CASE.FILE.SEL1
            Y.CNT.VAL.CHECK += 1 ;*R22 Auto code conversion
        REPEAT
        R.NEW(RE.ORD.ORDER.STATUS) = 'Orden Devuelta Recibida'
        Y.OFS.VAL = OFS$OPERATION
        IF Y.OFS.VAL EQ 'PROCESS' THEN
            CALL F.WRITE(FN.REDO.ITEM.SERIES,Y.ITEM.STOCK.RPT.ID,R.REDO.ITEM.SERIES)
        END
    END
    R.NEW(RE.ORD.RESERVED.5) = ''
RETURN


*-------------------------------
UPD.ITEM.STOCK.RPT:
*-------------------------------

    Y.DATE.RPT = R.NEW(RE.ORD.DATE)
    IF R.REDO.ITEM.STOCK.BY.DATE THEN
        LOCATE Y.DATE.RPT IN R.REDO.ITEM.STOCK.BY.DATE<ITEM.RPT.DATE,1> SETTING POS.RPT THEN
            R.REDO.ITEM.STOCK.BY.DATE<ITEM.RPT.RECEIVED,POS.RPT>              =   R.REDO.ITEM.STOCK.BY.DATE<ITEM.RPT.RECEIVED,POS.RPT> + R.NEW(RE.ORD.RETURN.QUANTITY)
            R.REDO.ITEM.STOCK.BY.DATE<ITEM.RPT.AVALIABLE,POS.RPT>           =   R.REDO.ITEM.STOCK.BY.DATE<ITEM.RPT.AVALIABLE,POS.RPT> + R.NEW(RE.ORD.RETURN.QUANTITY)
        END ELSE
            Y.DATE.COUNT1 = DCOUNT(R.REDO.ITEM.STOCK.BY.DATE<ITEM.RPT.DATE>,@VM)
            Y.DATE.COUNT = Y.DATE.COUNT1 + 1

            R.REDO.ITEM.STOCK.BY.DATE<ITEM.RPT.DATE,Y.DATE.COUNT>                =   Y.DATE.RPT
            R.REDO.ITEM.STOCK.BY.DATE<ITEM.RPT.ITEM.CODE,Y.DATE.COUNT>           =   R.NEW(RE.ORD.ITEM.CODE)
            R.REDO.ITEM.STOCK.BY.DATE<ITEM.RPT.INITIAL.STOCK,Y.DATE.COUNT>       =   R.REDO.ITEM.STOCK.BY.DATE<ITEM.RPT.AVALIABLE,Y.DATE.COUNT1>
            R.REDO.ITEM.STOCK.BY.DATE<ITEM.RPT.RECEIVED,Y.DATE.COUNT>            =   R.NEW(RE.ORD.RETURN.QUANTITY)
            R.REDO.ITEM.STOCK.BY.DATE<ITEM.RPT.AVALIABLE,Y.DATE.COUNT>           =   R.REDO.ITEM.STOCK.BY.DATE<ITEM.RPT.AVALIABLE,Y.DATE.COUNT1> + R.NEW(RE.ORD.RETURN.QUANTITY)
        END
    END
    CALL F.WRITE(FN.REDO.ITEM.STOCK.BY.DATE,Y.ITEM.STOCK.RPT.ID,R.REDO.ITEM.STOCK.BY.DATE)

RETURN
*-----------------------------------------------------------------------------------------
CASE.FILE.SEL1:
*-----------------------------------------------------------------------------------------

    BEGIN CASE
        CASE DUP.APPL.NAME EQ 'F.REDO.H.DEPOSIT.RECEIPTS'
            GOSUB REDO.DEPOSIT.RECP

        CASE DUP.APPL.NAME EQ 'F.REDO.H.ADMIN.CHEQUES'

            GOSUB REDO.ADMIN.CHEQ

        CASE DUP.APPL.NAME EQ 'F.REDO.H.PASSBOOK.INVENTORY'

            GOSUB REDO.PASSBOOK.INV

        CASE DUP.APPL.NAME EQ 'F.REDO.H.BANK.DRAFTS'
            GOSUB REDO.BANK.DRFT

    END CASE

RETURN

*-------------------------------
REDO.BANK.DRFT:
*-------------------------------
    IF SEL.LIST.2 THEN
        CALL F.READ(FN.REDO.H.BANK.DRAFTS,SEL.LIST.2,R.REDO.H.BANK.DRAFTS,F.REDO.H.BANK.DRAFTS,Y.ERR.APP)
        IF Y.REJ.VAL EQ 'YES' THEN
            R.REDO.H.BANK.DRAFTS<REDO.BANK.STATUS> = 'AVAILABLE'
            R.REDO.H.BANK.DRAFTS<REDO.BANK.INPUTTER> = INPUTTER
            R.REDO.H.BANK.DRAFTS<REDO.BANK.AUTHORISER> = AUTHORISER
            LOCATE SEL.LIST.2 IN Y.BATCH.LIST.DUP SETTING POS.SER THEN
                R.REDO.ITEM.SERIES<POS.SER> = R.REDO.H.BANK.DRAFTS<REDO.BANK.SERIAL.NO>:"*":'AVAILABLE':"*":SEL.LIST.2:"*":R.REDO.H.BANK.DRAFTS<REDO.BANK.DATE.UPDATED>
            END
        END ELSE
            R.REDO.H.BANK.DRAFTS<REDO.BANK.STATUS> = 'ORDEN DEVUELTA RECIBIDA'
            R.REDO.H.BANK.DRAFTS<REDO.BANK.INPUTTER> = INPUTTER
            R.REDO.H.BANK.DRAFTS<REDO.BANK.AUTHORISER> = AUTHORISER
            LOCATE SEL.LIST.2 IN Y.BATCH.LIST.DUP SETTING POS.SER THEN
                R.REDO.ITEM.SERIES<POS.SER> = ''
            END
        END
        R.REDO.H.BANK.DRAFTS<REDO.BANK.DATE.UPDATED> = TODAY
        CALL F.WRITE(FN.REDO.H.BANK.DRAFTS,SEL.LIST.2,R.REDO.H.BANK.DRAFTS)

    END
RETURN
*-------------------------------
REDO.DEPOSIT.RECP:
*-------------------------------
    IF SEL.LIST.2 THEN
        CALL F.READ(FN.REDO.H.DEPOSIT.RECEIPTS,SEL.LIST.2,R.REDO.H.DEPOSIT.RECEIPTS,F.REDO.H.DEPOSIT.RECEIPTS,Y.ERR.APP)
        IF Y.REJ.VAL EQ 'YES' THEN
            R.REDO.H.DEPOSIT.RECEIPTS<REDO.DEP.STATUS> = 'AVAILABLE'
            R.REDO.H.DEPOSIT.RECEIPTS<REDO.DEP.INPUTTER> = INPUTTER
            R.REDO.H.DEPOSIT.RECEIPTS<REDO.DEP.AUTHORISER> = AUTHORISER
            LOCATE SEL.LIST.2 IN Y.BATCH.LIST.DUP SETTING POS.SER THEN
                R.REDO.ITEM.SERIES<POS.SER> = R.REDO.H.DEPOSIT.RECEIPTS<REDO.DEP.SERIAL.NO>:"*":'AVAILABLE':"*":SEL.LIST.2:"*":R.REDO.H.DEPOSIT.RECEIPTS<REDO.DEP.DATE.UPDATED>
            END
        END ELSE
            R.REDO.H.DEPOSIT.RECEIPTS<REDO.DEP.STATUS> = 'ORDEN DEVUELTA RECIBIDA'
            R.REDO.H.DEPOSIT.RECEIPTS<REDO.DEP.INPUTTER> = INPUTTER
            R.REDO.H.DEPOSIT.RECEIPTS<REDO.DEP.AUTHORISER> = AUTHORISER
            LOCATE SEL.LIST.2 IN Y.BATCH.LIST.DUP SETTING POS.SER THEN
                R.REDO.ITEM.SERIES<POS.SER> = ''
            END
        END
        R.REDO.H.DEPOSIT.RECEIPTS<REDO.DEP.DATE.UPDATED> = TODAY
        CALL F.WRITE(FN.REDO.H.DEPOSIT.RECEIPTS,SEL.LIST.2,R.REDO.H.DEPOSIT.RECEIPTS)

    END
RETURN
*-------------------------------
REDO.ADMIN.CHEQ:
*-------------------------------
    IF SEL.LIST.2 THEN
        CALL F.READ(FN.REDO.H.ADMIN.CHEQUES,SEL.LIST.2,R.REDO.H.ADMIN.CHEQUES,F.REDO.H.ADMIN.CHEQUES,Y.ERR.APP)
        IF Y.REJ.VAL EQ 'YES' THEN
            R.REDO.H.ADMIN.CHEQUES<REDO.ADMIN.STATUS> = 'AVAILABLE'
            R.REDO.H.ADMIN.CHEQUES<REDO.ADMIN.INPUTTER> = INPUTTER
            R.REDO.H.ADMIN.CHEQUES<REDO.ADMIN.AUTHORISER> = AUTHORISER
            LOCATE SEL.LIST.2 IN Y.BATCH.LIST.DUP SETTING POS.SER THEN
                R.REDO.ITEM.SERIES<POS.SER> = R.REDO.H.ADMIN.CHEQUES<REDO.ADMIN.SERIAL.NO>:"*":'AVAILABLE':"*":SEL.LIST.2:"*":R.REDO.H.ADMIN.CHEQUES<REDO.ADMIN.DATE.UPDATED>
            END
        END ELSE
            R.REDO.H.ADMIN.CHEQUES<REDO.ADMIN.STATUS> = 'ORDEN DEVUELTA RECIBIDA'
            R.REDO.H.ADMIN.CHEQUES<REDO.ADMIN.INPUTTER> = INPUTTER
            R.REDO.H.ADMIN.CHEQUES<REDO.ADMIN.AUTHORISER> = AUTHORISER
            LOCATE SEL.LIST.2 IN Y.BATCH.LIST.DUP SETTING POS.SER THEN
                R.REDO.ITEM.SERIES<POS.SER> = ''
            END
        END
        R.REDO.H.ADMIN.CHEQUES<REDO.ADMIN.DATE.UPDATED> = TODAY
        CALL F.WRITE(FN.REDO.H.ADMIN.CHEQUES,SEL.LIST.2,R.REDO.H.ADMIN.CHEQUES)
    END
RETURN
*-------------------------------
REDO.PASSBOOK.INV:
*-------------------------------
    IF SEL.LIST.2 THEN
        CALL F.READ(FN.REDO.H.PASSBOOK.INVENTORY,SEL.LIST.2,R.REDO.H.PASSBOOK.INVENTORY,F.REDO.H.PASSBOOK.INVENTORY,Y.ERR.APP)
        IF Y.REJ.VAL EQ 'YES' THEN
            R.REDO.H.PASSBOOK.INVENTORY<REDO.PASS.STATUS> = 'AVAILABLE'
            R.REDO.H.PASSBOOK.INVENTORY<REDO.PASS.INPUTTER> = INPUTTER
            R.REDO.H.PASSBOOK.INVENTORY<REDO.PASS.AUTHORISER> = AUTHORISER
            LOCATE SEL.LIST.2 IN Y.BATCH.LIST.DUP SETTING POS.SER THEN
                R.REDO.ITEM.SERIES<POS.SER> = R.REDO.H.PASSBOOK.INVENTORY<REDO.PASS.SERIAL.NO>:"*":'AVAILABLE':"*":SEL.LIST.2:"*":R.REDO.H.PASSBOOK.INVENTORY<REDO.PASS.DATE.UPDATED>
            END
        END ELSE
            R.REDO.H.PASSBOOK.INVENTORY<REDO.PASS.STATUS> = 'ORDEN DEVUELTA RECIBIDA'
            R.REDO.H.PASSBOOK.INVENTORY<REDO.PASS.INPUTTER> = INPUTTER
            R.REDO.H.PASSBOOK.INVENTORY<REDO.PASS.AUTHORISER> = AUTHORISER
            LOCATE SEL.LIST.2 IN Y.BATCH.LIST.DUP SETTING POS.SER THEN
                R.REDO.ITEM.SERIES<POS.SER> = ''
            END
        END
        R.REDO.H.PASSBOOK.INVENTORY<REDO.PASS.DATE.UPDATED> = TODAY
        CALL F.WRITE(FN.REDO.H.PASSBOOK.INVENTORY,SEL.LIST.2,R.REDO.H.PASSBOOK.INVENTORY)

    END
RETURN
*-----------------------------------------------------------------------------------------
PROGRAM.END:
*-----------------------------------------------------------------------------------------
END
