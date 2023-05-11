* @ValidationCode : Mjo5NjQzNDEwMDU6Q3AxMjUyOjE2ODI2ODE5MTkzNTk6SVRTUzotMTotMTowOjE6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 28 Apr 2023 17:08:39
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

*-----------------------------------------------------------------------------
* <Rating>-85</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE REDO.NOF.CHQ.NT.PRESENT(Y.FINAL.ARRAY)
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :Akthar Rasool S
*Program   Name    :REDO.NOF.CHQ.NT.PRESENT
*---------------------------------------------------------------------------------

*DESCRIPTION       :This routine is No file routine for the enquiry REDO.PENDING.TFS
*
*LINKED WITH       :
*21 Nov 2011    Kavitha       PACS00164149 fix
* ----------------------------------------------------------------------------------
*-----------------------------------------------------------------------------------------------------
* Modification History:
*
* Date             Who                   Reference      Description
* 28.04.2023       Conversion Tool       R22            Auto Conversion     - No changes
* 28.04.2023       Shanmugapriya M       R22            Manual Conversion   - FM TO @FM, VM TO @VM
*
*------------------------------------------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.T24.FUND.SERVICES
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CUSTOMER
    $INSERT I_F.REDO.CLEARING.OUTWARD
    $INSERT I_F.REDO.OUT.CLEAR.FILE
    $INSERT I_F.REDO.APAP.CLEAR.PARAM
    $INSERT I_F.REDO.COLLECT.PARAM
    $INSERT I_F.TELLER

    GOSUB INIT
    GOSUB OPEN.FILE
    GOSUB PROCESS

RETURN

*---------------------------------------------------------------------------------
INIT:
*---------------------------------------------------------------------------------
*Initialisation

    BOOKING.DATE = ''; PRIMARY.ACCOUNT =''; AUTHORISER =''; NO.OF.CHQ='';CO.CODE='';CURRENCY='';AMOUNT='';TFS.ID='';Y.FINAL.ARRAY=''
    TXN.ID=''; TXN.VAL=''

RETURN

*---------------------------------------------------------------------------------
OPEN.FILE:
*---------------------------------------------------------------------------------
*Opening Files

    FN.FUND.SERVICES ='F.T24.FUND.SERVICES'
    F.FUND.SERVICES =''
    CALL OPF(FN.FUND.SERVICES,F.FUND.SERVICES)

    FN.REDO.APAP.CLEAR.PARAM ='F.REDO.APAP.CLEAR.PARAM'
    F.REDO.APAP.CLEAR.PARAM=''
    CALL OPF(FN.REDO.APAP.CLEAR.PARAM,F.REDO.APAP.CLEAR.PARAM)

    FN.REDO.COLLECT.PARAM='F.REDO.COLLECT.PARAM'
    F.REDO.COLLECT.PARAM=''
    CALL OPF(FN.REDO.COLLECT.PARAM,F.REDO.COLLECT.PARAM)

    FN.TELLER.USER='F.TELLER.USER'
    F.TELLER.USER=''
    CALL OPF(FN.TELLER.USER,F.TELLER.USER)

    FN.TELLER = 'F.TELLER'
    F.TELLER = ''
    CALL OPF(FN.TELLER,F.TELLER)

    FN.REDO.OUT.CLEAR.FILE='F.REDO.OUT.CLEAR.FILE'
    F.REDO.OUT.CLEAR.FILE=''
    CALL OPF(FN.REDO.OUT.CLEAR.FILE,F.REDO.OUT.CLEAR.FILE)

    LOC.REF.APPLICATION="T24.FUND.SERVICES"
    LOC.REF.FIELDS='L.TT.NO.OF.CHQ':@VM:'L.T24FS.TRA.DAY'
    LOC.REF.POS=''
    CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LOC.REF.POS)
    L.TT.NO.OF.CHQ.POS = LOC.REF.POS<1,1>
    POS.L.T24FS.TRA.DAY= LOC.REF.POS<1,2>

RETURN

*---------------------------------------------------------------------------------
PROCESS:
*---------------------------------------------------------------------------------
*Selecting FN.FUND.SERVICES
    LOCATE 'CURRENCY' IN D.FIELDS SETTING CCY.POS THEN
        CCY.ID = D.RANGE.AND.VALUE<CCY.POS>
    END
    LOCATE 'COMP.CODE' IN D.FIELDS SETTING COMP.POS THEN
        COMP.ID = D.RANGE.AND.VALUE<COMP.POS>
    END

    DD=TODAY

    TXN.IDD='SYSTEM' ; TXN.ID=''
    VAR.COUNT = '' ; PREV.TFS.ID = ''

    CALL CACHE.READ(FN.REDO.APAP.CLEAR.PARAM,TXN.IDD,R.REDO.APAP.CLEAR.PARAM,TRAN.TYPE.ERR)
    LCY.TT.FT.TRAN.TYPE=R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.TT.FT.TRAN.TYPE>

    CALL CACHE.READ(FN.REDO.COLLECT.PARAM,TXN.IDD,R.REDO.COLLECT.PARAM,TRAN.COLL.ERR)
    FCY.TT.FT.TRAN.TYPE=R.REDO.COLLECT.PARAM<COLLECT.PARAM.TT.FT.TRAN.TYPE>



    SEL.CMD = "SELECT ":FN.FUND.SERVICES:" WITH BOOKING.DATE EQ ":DD:" AND R.UL.STATUS NE REVE"
    IF CCY.ID THEN
        SEL.CMD := " WITH CURRENCY EQ ":CCY.ID
    END
    IF COMP.ID THEN
        SEL.CMD := " WITH CO.CODE EQ ":COMP.ID
    END
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,RET.CODE)
    VAR.COUNT = 1
    LOOP
        REMOVE TXN.ID FROM SEL.LIST SETTING TXN.POS
    WHILE VAR.COUNT LE NO.OF.REC

        CALL F.READ(FN.FUND.SERVICES,TXN.ID,R.FUND.SERVICES,F.FUND.SERVICES,SERVICES.ERR)
        CURRENCY.LIST = R.FUND.SERVICES<TFS.CURRENCY>
        Y.TRANSACTION.LIST = R.FUND.SERVICES<TFS.TRANSACTION>
        Y.AMOUNT.LIST = R.FUND.SERVICES<TFS.AMOUNT>
        Y.UNDERLYING = R.FUND.SERVICES<TFS.UNDERLYING>

        TXN.LIST.COUNT = DCOUNT(Y.TRANSACTION.LIST,@VM)
        CHANGE @VM TO @FM IN Y.TRANSACTION.LIST
        CHANGE @VM TO @FM IN CURRENCY.LIST
        CHANGE @VM TO @FM IN Y.AMOUNT.LIST
        CHANGE @VM TO @FM IN Y.UNDERLYING
*PACS00164149-S
        GOSUB TRANSACTION.VALIDATE
*PACS00164149 -E
        PREV.TFS.ID = ''
        VAR.COUNT++
    REPEAT

RETURN

*---------
TRANSACTION.VALIDATE:

    VAR.TEMP.CNT = 1
    LOOP
        REMOVE Y.TRANSACTION FROM Y.TRANSACTION.LIST SETTING Y.TRANSACTION.POS
    WHILE VAR.TEMP.CNT LE TXN.LIST.COUNT

        GET.CURRENCY = CURRENCY.LIST<VAR.TEMP.CNT>
        TXN.VAL = Y.TRANSACTION.LIST<VAR.TEMP.CNT>
        GET.AMOUNT = Y.AMOUNT.LIST<VAR.TEMP.CNT>
        Y.TT.REF.ID  = Y.UNDERLYING<VAR.TEMP.CNT>
        CALL F.READ(FN.TELLER,Y.TT.REF.ID,R.TELLER,F.TELLER,TELLER.ERR)
        Y.TELLER.ID = R.TELLER<TT.TE.TELLER.ID.1>
        IF GET.CURRENCY EQ LCCY THEN
            TT.FT.TRAN.TYPE = LCY.TT.FT.TRAN.TYPE
        END ELSE
            TT.FT.TRAN.TYPE = FCY.TT.FT.TRAN.TYPE
        END

        IF TXN.VAL NE '' AND TXN.ID NE '' THEN
            GOSUB PROC.DATE
        END

        TT.FT.TRAN.TYPE = ''
        GET.CURRENCY = ''
        TXN.VAL = ''
        GET.AMOUNT = ''

        VAR.TEMP.CNT++

    REPEAT


RETURN
*---------
PROC.DATE:
*----------

    LOCATE TXN.VAL IN TT.FT.TRAN.TYPE<1,1> SETTING CLEAR.POS THEN
        FLAG = 1
        GOSUB PROCESS.DATE
    END

RETURN

*---------------------------------------------------------------------------------
PROCESS.DATE:
*---------------------------------------------------------------------------------
*Reading REDO.OUT.CLEAR.FILE Application


    R.REDO.OUT.CLEAR.FILE='' ; REDO.ERR = ''
    REDO.DATE=TODAY ; CLEAR.TFS.ID=''

    CALL F.READ(FN.REDO.OUT.CLEAR.FILE,REDO.DATE,R.REDO.OUT.CLEAR.FILE,F.REDO.OUT.CLEAR.FILE,REDO.ERR)
    IF R.REDO.OUT.CLEAR.FILE THEN
        CLEAR.TFS.ID = R.REDO.OUT.CLEAR.FILE<REDO.OUT.CLEAR.FILE.TFS.ID>
        LOCATE TXN.ID IN CLEAR.TFS.ID<1,1> SETTING POS ELSE
            GOSUB FS.DATE
        END
    END ELSE
        GOSUB FS.DATE
    END

RETURN

*-------------
FS.DATE:
*----------------
    R.FUND.SERVICES='' ; Y.FS.ERR = ''
    BOOKING.DATE  = ''
    PRIMARY.ACCOUNT = ''
    AUTHORISER = ''
    NO.OF.CHQ = ''
    CO.CODE = ''
    TFS.ID = ''
    AUTH = ''
    TELLER.ID = ''

    CALL F.READ(FN.FUND.SERVICES,TXN.ID,R.FUND.SERVICES,F.FUND.SERVICES,Y.FS.ERR)
    IF R.FUND.SERVICES AND FLAG EQ 1 AND R.FUND.SERVICES<TFS.LOCAL.REF,POS.L.T24FS.TRA.DAY> NE 'T24.FUND.SERVICES,SERVICE.CREATE' THEN        ;* Cheque deposit for AZ should not come in pending TFS
        IF TXN.ID NE PREV.TFS.ID THEN
            TFS.ID = TXN.ID
            BOOKING.DATE = R.FUND.SERVICES<TFS.BOOKING.DATE>
            PRIMARY.ACCOUNT = R.FUND.SERVICES<TFS.PRIMARY.ACCOUNT>
            AUTHORISER = R.FUND.SERVICES<TFS.AUTHORISER>
            NO.OF.CHQ = R.FUND.SERVICES<TFS.LOCAL.REF,L.TT.NO.OF.CHQ.POS>
            CO.CODE = R.FUND.SERVICES<TFS.CO.CODE>
            PREV.TFS.ID = TFS.ID
        END
        GOSUB FINAL.ARRAY
    END


RETURN

*---------------------------------------------------------------------------------
FINAL.ARRAY:
*---------------------------------------------------------------------------------

    Y.FINAL.ARRAY<-1> =TFS.ID:"*":BOOKING.DATE:"*":PRIMARY.ACCOUNT:"*":Y.TELLER.ID:"*":CO.CODE:"*":NO.OF.CHQ:"*":GET.CURRENCY:"*":GET.AMOUNT
*                        1              2                  3              4               5            6           7           8
RETURN
END
