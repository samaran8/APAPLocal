* @ValidationCode : Mjo4MTQ5ODU0NTpDcDEyNTI6MTY4MTgwMDY3NjQxMjpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 18 Apr 2023 12:21:16
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
$PACKAGE APAP.REDOENQ
SUBROUTINE E.BEA.SDB.STAT.RTN(Y.PROCESSED.ARR)
*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
*
* DATE              WHO                REFERENCE                 DESCRIPTION
* 18-APR-2023     Conversion tool    R22 Auto conversion       = to EQ, ++ to +=, VM to @VM
* 18-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.CUSTOMER
    $INSERT I_F.MB.SDB.PARAM
    $INSERT I_F.MB.SDB.TYPE
    $INSERT I_F.MB.SDB.POST
    $INSERT I_F.MB.SDB.STATUS
    $INSERT I_F.MB.SDB.CLOSED
*-----------------------------------------------------------------------------
    GOSUB INITIALISE
    GOSUB OPEN.FILES
    GOSUB CHECK.PRELIM.CONDITION
    GOSUB PROCESS.RECORDS

RETURN
*-----------------------------------------------------------------------------
PROCESS.RECORDS:
*-----------------------------------------------------------------------------
* Main process to fetch actual datas from MB.SDB.STATUS and MB.SDB.CLOSED file.
*-----------------------------------------------------------------------------

    LOCATE "SDB.ID" IN D.FIELDS<1> SETTING COMP.CODE.POS THEN
        COMP.CODE = D.RANGE.AND.VALUE<COMP.CODE.POS>
    END
    SEL.CMD = 'SELECT ':FN.MB.SDB.TYPE
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.RECS,SEL.ERR)

    LOOP
        REMOVE BOX.TYPE FROM SEL.LIST SETTING POS.BOX.TYPE
*PACS00177126 - S
        NO.BOX.TYPE = BOX.TYPE
        IF NUM(NO.BOX.TYPE) ELSE
            BOX.TYPE1 = FIELD(NO.BOX.TYPE,"X",1)
            DELIM.COUNT = LEN(BOX.TYPE1)
            AFTER.COUNT = DELIM.COUNT + 2
            BOX.TYPE.1 = NO.BOX.TYPE[DELIM.COUNT,1]
            BOX.TYPE.2 = NO.BOX.TYPE[AFTER.COUNT,1]
            IF NUM(BOX.TYPE.1) THEN
                IF NUM(BOX.TYPE.2) THEN
                    NO.BOX.TYPE = NO.BOX.TYPE[1,DELIM.COUNT]
                END
            END
        END
*PACS00177126 - E
*
* Initialise necessary Variable Before select MB.SDB.STATUS and MB.SDB.CLOSED
*
        GOSUB INITIALISE.VARIABLE
    WHILE BOX.TYPE:POS.BOX.TYPE
*
* Select records from MB.SDB.STATUS file based on condition
*
        GOSUB SELECT.MB.SDB.STATUS
*
* Select records from MB.SDB.CLOSED file based on condition
*
        GOSUB SELECT.MB.SDB.CLOSED
        TOTAL.RENTED = BOX.RENTED.STAFF + BOX.RENTED.CUSTOMER
        UTIL.PERCENT.TEMP = TOTAL.RENTED/TOTAL.IN.BRANCH
        UTIL.PERCENT = UTIL.PERCENT.TEMP * 100
        Y.PROCESSED.ARR<-1>=COMP.CODE:"*":BOX.TYPE:"*":BOX.RENTED.CUSTOMER:"*":BOX.RENTED.STAFF:"*":TOTAL.RENTED:"*":BOX.RESERVED:"*":BOX.AVAILABLE:"*":TOTAL.IN.BRANCH:"*":UTIL.PERCENT:"*":TOD.BOX.CANCELLED:"*":TOD.BOX.RENTED:"*":TOT.RENT.AMOUNT.REFUNDED:"*":TOT.VAT.AMOUNT.REFUNDED:"*":TOT.INCOME.RECEIVED:"*":TOT.VAT.RECEIVED:"*":TOT.INCOME.RECEIVABLE:"*":TOT.VAT.RECEIVABLE:"*":BOX.REPAIR

    REPEAT

RETURN
*-----------------------------------------------------------------------------
SELECT.MB.SDB.STATUS:

    SEL.SDB.STATUS.CMD = 'SELECT ':FN.MB.SDB.STATUS:' WITH @ID LIKE ':COMP.CODE:'.':NO.BOX.TYPE:'... BY @ID'
    CALL EB.READLIST(SEL.SDB.STATUS.CMD,SDB.STATUS.LIST,'',NO.STATUS.REC,STATUS.ERR)
    TOTAL.IN.BRANCH = NO.STATUS.REC

    LOOP
        REMOVE SDB.STATUS.ID FROM SDB.STATUS.LIST SETTING POS.STATUS.ID
    WHILE SDB.STATUS.ID:POS.STATUS.ID
        R.MB.SDB.STATUS.REC=''
        CALL F.READ(FN.MB.SDB.STATUS,SDB.STATUS.ID,R.MB.SDB.STATUS.REC,F.MB.SDB.STATUS,SDB.STATUS.ERR)
        BOX.STATUS = R.MB.SDB.STATUS.REC<SDB.STA.STATUS>
        CUSTOMER.ID = R.MB.SDB.STATUS.REC<SDB.STA.CUSTOMER.NO>
        NOTES = R.MB.SDB.STATUS.REC<SDB.STA.NOTES>

        IF BOX.STATUS EQ 'AVAILABLE' THEN
            BOX.AVAILABLE += 1
        END
        IF BOX.STATUS EQ 'RESERVED' THEN
            BOX.RESERVED += 1
        END
        IF BOX.STATUS EQ 'RENTED' THEN
            TOD.BOX.RENTED += 1
            IF STAFF.FLAG EQ '1' THEN
                BOX.RENTED.STAFF += 1
            END ELSE
                BOX.RENTED.CUSTOMER += 1
            END
        END
        IF BOX.STATUS EQ 'REPAIR' THEN
            BOX.REPAIR += 1
        END
        GOSUB RECORD.PROCESS
    REPEAT

RETURN
*-----------------------------------------------------------------------------
SELECT.MB.SDB.CLOSED:

    THIS.MONTH = TODAY[1,6]
    SEL.SDB.CLOSED.CMD = 'SELECT ':FN.MB.SDB.CLOSED:' WITH @ID LIKE ':COMP.CODE:'.':NO.BOX.TYPE:'... AND CLOSED.ON LIKE ':THIS.MONTH:'...'
    CALL EB.READLIST(SEL.SDB.CLOSED.CMD,SDB.CLOSED.LIST,'',NO.CLOSED.REC,CLOSED.ERR)

    LOOP
        REMOVE SDB.CLOSED.ID FROM SDB.CLOSED.LIST SETTING POS.CLOSED.ID
    WHILE SDB.CLOSED.ID:POS.CLOSED.ID
        R.MB.SDB.CLOSED.REC=''
        CALL F.READ(FN.MB.SDB.CLOSED,SDB.CLOSED.ID,R.MB.SDB.CLOSED.REC,F.MB.SDB.CLOSED,SDB.CLOSED.ERR)
        RENT.AMOUNT.REFUNDED = R.MB.SDB.CLOSED.REC<SDB.CLO.CHG.RENT.AMT>
        VAT.AMOUNT.REFUNDED = R.MB.SDB.CLOSED.REC<SDB.CLO.CHG.RENT.VAT>

        REFUND.AMT = '' ; VAT.REFUND.AMT = ''
        RENT.AMOUNT.DC = DCOUNT(RENT.AMOUNT.REFUNDED,@VM)
        FOR RENT.AMOUNT.CT = 1 TO RENT.AMOUNT.DC
            IF RENT.AMOUNT.REFUNDED<1,RENT.AMOUNT.CT> LT 0 THEN
                REFUND.AMT += RENT.AMOUNT.REFUNDED<1,RENT.AMOUNT.CT>
            END
            IF VAT.AMOUNT.REFUNDED<1,RENT.AMOUNT.CT> LT 0 THEN
                VAT.REFUND.AMT += VAT.AMOUNT.REFUNDED<1,RENT.AMOUNT.CT>
            END
        NEXT RENT.AMOUNT.CT
        TOT.RENT.AMOUNT.REFUNDED += REFUND.AMT
        TOT.VAT.AMOUNT.REFUNDED += VAT.REFUND.AMT

    REPEAT

RETURN
*-----------------------------------------------------------------------------
RECORD.PROCESS:

    CUST.GROUP = '' ; STAFF.FLAG = ''
    MB.SDB.PARAM.ID = FIELD(SDB.STATUS.ID,'.',1,1) ; YERR = ''
    CALL F.READ(FN.MB.SDB.PARAM,MB.SDB.PARAM.ID,R.MB.SDB.PARAM,F.MB.SDB.PARAM,YERR)
    STAFF.GROUP = R.MB.SDB.PARAM<SDB.PAR.STAFF.GROUP>

    APPL.R = '' ; APPL.ID = SDB.STATUS.ID
    APPL.R<SDB.STA.CUSTOMER.NO> = R.MB.SDB.STATUS.REC<SDB.STA.CUSTOMER.NO>
    APPL.R<SDB.STA.CUSTOMER.ACCT> = R.MB.SDB.STATUS.REC<SDB.STA.CUSTOMER.ACCT>

    CALL APPL.GRP.CONDITION('MB.SDB.STATUS', APPL.ID, APPL.R, CUST.GROUP)
    LOCATE CUST.GROUP IN STAFF.GROUP<1,1> SETTING POS THEN
        STAFF.FLAG = '1'
    END

    START.DATE = R.MB.SDB.STATUS.REC<SDB.STA.LAST.RENEWAL.DATE>
    IF NOT(START.DATE) THEN
        START.DATE = R.MB.SDB.STATUS.REC<SDB.STA.OPENING.DATE>
    END
    THIS.MONTH = TODAY[1,6]
    START.THIS.MONTH = THIS.MONTH:'01'

    IF START.DATE AND START.DATE GE START.THIS.MONTH THEN
        TOT.INCOME.RECEIVED += R.MB.SDB.STATUS.REC<SDB.STA.RENT.AMT>
        TOT.VAT.RECEIVED += R.MB.SDB.STATUS.REC<SDB.STA.RENT.VAT>
    END
    RENEW.DATE = R.MB.SDB.STATUS.REC<SDB.STA.RENEWAL.DUE.ON>
    IF RENEW.DATE AND RENEW.DATE LT START.THIS.MONTH THEN
        TOT.INCOME.RECEIVABLE += R.MB.SDB.STATUS.REC<SDB.STA.PERIODIC.RENT>
        TOT.VAT.RECEIVABLE += R.MB.SDB.STATUS.REC<SDB.STA.VAT.AMOUNT>
    END

RETURN
*-----------------------------------------------------------------------------
INITIALISE.VARIABLE:

    BOX.REPAIR = 0
    BOX.RENTED.CUSTOMER = 0
    BOX.RENTED.STAFF = 0
    BOX.AVAILABLE = 0
    BOX.RESERVED = 0
    TOD.BOX.RENTED = 0
    TOD.BOX.CANCELLED = 0
    TOT.RENT.AMOUNT.REFUNDED = 0
    TOT.VAT.AMOUNT.REFUNDED = 0
    TOT.INCOME.RECEIVED = 0
    TOT.VAT.RECEIVED = 0
    TOT.INCOME.RECEIVABLE = 0
    TOT.VAT.RECEIVABLE = 0
    SEL.SDB.STATUS.CMD=''
    SDB.STATUS.LIST=''
    NO.STATUS.REC=''

RETURN
*-----------------------------------------------------------------------------
*/////////////////////////////////////////////////////////////////////////////
*////////////////P R E  P R O C E S S  S U B R O U T I N E S /////////////////
*/////////////////////////////////////////////////////////////////////////////
*-----------------------------------------------------------------------------
INITIALISE:

    Y.PROCESSED.ARR=''
    COMP.CODE=''
    SEL.CMD=''
    SEL.LIST=''
    POS.BOX.TYPE=''

RETURN
*-----------------------------------------------------------------------------
OPEN.FILES:

    FN.MB.SDB.TYPE = 'F.MB.SDB.TYPE'
    F.MB.SDB.TYPE = ''
    CALL OPF(FN.MB.SDB.TYPE,F.MB.SDB.TYPE)

    FN.MB.SDB.PARAM = 'F.MB.SDB.PARAM'
    F.MB.SDB.PARAM = ''
    CALL OPF(FN.MB.SDB.PARAM,F.MB.SDB.PARAM)

    FN.MB.SDB.POST = 'F.MB.SDB.POST'
    F.MB.SDB.POST  = ''
    CALL OPF(FN.MB.SDB.POST,F.MB.SDB.POST)

    FN.MB.SDB.STATUS = 'F.MB.SDB.STATUS'
    F.MB.SDB.STATUS = ''
    CALL OPF(FN.MB.SDB.STATUS,F.MB.SDB.STATUS)

    FN.MB.SDB.CLOSED = 'F.MB.SDB.CLOSED'
    F.MB.SDB.CLOSED = ''
    CALL OPF(FN.MB.SDB.CLOSED,F.MB.SDB.CLOSED)

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

RETURN
*-----------------------------------------------------------------------------
CHECK.PRELIM.CONDITION:

RETURN
*-----------------------------------------------------------------------------
END
