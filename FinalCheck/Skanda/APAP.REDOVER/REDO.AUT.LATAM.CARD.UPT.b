* @ValidationCode : MjoxMDQ1OTMxOTExOkNwMTI1MjoxNjgwNjA2NzgzNjIwOnNhbWFyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 04 Apr 2023 16:43:03
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
SUBROUTINE REDO.AUT.LATAM.CARD.UPT
***********************************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: JEEVA T
* PROGRAM NAME: REDO.AUT.LATAM.CARD.UPT
* ODR NO      : ODR-2010-03-0400
*----------------------------------------------------------------------
*DESCRIPTION: This routine is authorisation routine to update LATAM CARD ORDER
*
*----------------------------------------------------------------------
* Modification History :
*-----------------------
*DATE           WHO                     REFERENCE               DESCRIPTION
*9.03.2011     JEEVA T               ODR-2010-03-0400          INITIAL CREATION
*10 JUN 2011   KAVITHA                PACS00063138               FIX
*16 JUN 2011   KAVITHA                 PACS00072694              PROSPECT CHANGES ADDED
*04-04-2023   Conversion Tool          R22 Auto Code conversion  VM TO @VM , Y.CNT + 1 TO +=1
*04-04-2023    Samaran T              Manual R22 Code Conversion    No Changes
*----------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_F.ACCOUNT
    $INSERT I_F.REDO.CARD.GENERATION
    $INSERT I_F.REDO.CARD.REQUEST
    $INSERT I_F.LATAM.CARD.ORDER
    $INSERT I_F.REDO.CARD.BIN



    IF R.NEW(REDO.CARD.REQ.ACCOUNT.NO)<1,1> EQ '' THEN

        RETURN
    END

    GOSUB OPEN.FILE
    GOSUB PROCESS.FILE
RETURN
*----------------------------------------------------------------------
OPEN.FILE:
*----------------------------------------------------------------------

    FN.REDO.CARD.GENERATION = 'F.REDO.CARD.GENERATION'
    F.REDO.CARD.GENERATION = ''
    CALL OPF(FN.REDO.CARD.GENERATION,F.REDO.CARD.GENERATION)

    FN.ACCOUNT ='F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.REDO.CARD.REQUEST = 'F.REDO.CARD.REQUEST'
    F.REDO.CARD.REQUEST = ''
    CALL OPF(FN.REDO.CARD.REQUEST,F.REDO.CARD.REQUEST)

    FN.LATAM.CARD.ORDER = 'F.LATAM.CARD.ORDER'
    F.LATAM.CARD.ORDER = ''
    CALL OPF(FN.LATAM.CARD.ORDER,F.LATAM.CARD.ORDER)

    FN.REDO.CARD.BIN = 'F.REDO.CARD.BIN'
    F.REDO.CARD.BIN = ''
    CALL OPF(FN.REDO.CARD.BIN,F.REDO.CARD.BIN)

RETURN
*----------------------------------------------------------------------
PROCESS.FILE:
*----------------------------------------------------------------------


    Y.PERD.CARD = R.NEW(REDO.CARD.REQ.PERS.CARD)
    IF Y.PERD.CARD THEN
        GOSUB SUB.PROCESS
    END
RETURN
*----------------------------------------------------------------------
SUB.PROCESS:
*----------------------------------------------------------------------
    Y.ID.GEN = ID.NEW
    Y.PRIMARY = ''
    Y.CUS.PRIM  = ''
    Y.CUS.NUM = ''
    R.LATAM.CARD.ORDER.PRI = ''
    R.LATAM.CARD.ORDER = ''

    COMMENTS =   R.NEW(REDO.CARD.REQ.VAULT.QTY)<1,1>
    ISSUE.INDICATOR = TRIM(FIELD(COMMENTS,":",1))

    CALL F.READ(FN.REDO.CARD.GENERATION,Y.ID.GEN,R.REDO.CARD.GENERATION,F.REDO.CARD.GENERATION,Y.ERR)
    Y.CARD.NUMBER.TYPE = R.REDO.CARD.GENERATION<REDO.CARD.GEN.CARD.TYPE>
    Y.CARD.NUMBERS =  R.REDO.CARD.GENERATION<REDO.CARD.GEN.CARD.NUMBERS>
    Y.PRIMARY = R.NEW(REDO.CARD.REQ.PRIMARY.CARD)
    Y.PRIMARY.VAL = Y.PRIMARY
    Y.PRIMARY.CARD = ''

    IF Y.PRIMARY THEN
        Y.BIN = Y.PRIMARY[1,6]
        CALL F.READ(FN.REDO.CARD.BIN,Y.BIN,R.REDO.CARD.BIN,F.REDO.CARD.BIN,Y.ERR)
        Y.Y.CARD.NUMBER.PRI = R.REDO.CARD.BIN<REDO.CARD.BIN.CARD.TYPE>
        GOSUB PRIMARY.UPT
    END

    Y.LATAM.ID = Y.CARD.NUMBER.TYPE:'.':Y.CARD.NUMBERS
*PACS00063138-S
    GOSUB LATAM.CARD.UPDATE
    GOSUB OFS.POST
*PACS00063138 -E
RETURN
*----------------------------------------------------------------------
PRIMARY.UPT:
*----------------------------------------------------------------------
    Y.COUNT = DCOUNT(Y.Y.CARD.NUMBER.PRI,@VM) ;*R22 AUTO CODE CONVERSION
    Y.CNT =1
    LOOP
    WHILE Y.CNT LE Y.COUNT

        Y.Y.CARD.NUMBER.PRIMARY = Y.Y.CARD.NUMBER.PRI<1,Y.CNT>
        Y.PRIMARY = Y.Y.CARD.NUMBER.PRIMARY:'.':Y.PRIMARY.VAL
        CALL F.READ(FN.LATAM.CARD.ORDER,Y.PRIMARY,R.LATAM.CARD.ORDER.PRI,F.LATAM.CARD.ORDER,Y.ERR)
        IF R.LATAM.CARD.ORDER.PRI THEN
            Y.PRIMARY.CARD = Y.PRIMARY
            Y.CUS.NUM  = R.LATAM.CARD.ORDER.PRI<CARD.IS.NAME.ON.PLASTIC>
            Y.CUS.NUM = Y.CUS.NUM<1,1>
            EXIT
        END
        Y.CNT += 1 ;*R22 AUTO CODE CONVERSION
    REPEAT
RETURN
*----------------------------------------------------------------------
LATAM.CARD.UPDATE:
*----------------------------------------------------------------------

    Y.CARD.TYPE = R.NEW(REDO.CARD.REQ.TYPE.OF.CARD)
    IF ISSUE.INDICATOR THEN
        IF ISSUE.INDICATOR EQ "SAME" THEN
            R.LATAM.CARD.ORDER<CARD.IS.CARD.STATUS> = '90'
            R.LATAM.CARD.ORDER<CARD.IS.ISSUE.STAGE> ='ISSUED'
        END ELSE
            GOSUB NORMAL.CARD.PROCESS
        END
    END ELSE
        GOSUB NORMAL.CARD.PROCESS
    END

RETURN
*-------------
NORMAL.CARD.PROCESS:

    R.LATAM.CARD.ORDER<CARD.IS.CARD.STATUS> = '90'
    Y.ACCOUNT = R.NEW(REDO.CARD.REQ.ACCOUNT.NO)
    R.LATAM.CARD.ORDER<CARD.IS.ACCOUNT> = R.NEW(REDO.CARD.REQ.ACCOUNT.NO)
    CALL F.READ(FN.ACCOUNT,Y.ACCOUNT,R.ACCOUNT,F.ACCOUNT,Y.ACC.ERR)
    Y.CURRENCY = R.ACCOUNT<AC.CURRENCY>
    R.LATAM.CARD.ORDER<CARD.IS.CURRENCY> = Y.CURRENCY
    IF Y.PRIMARY.CARD THEN
        Y.CARD.NUMBERS.LIST = Y.LATAM.ID:@VM:Y.PRIMARY.CARD
    END ELSE
        Y.CARD.NUMBERS.LIST = Y.LATAM.ID
    END
    R.LATAM.CARD.ORDER<CARD.IS.ISSUE.DATE> = TODAY
    R.LATAM.CARD.ORDER<CARD.IS.ISSUE.STAGE> ='ISSUED'
    R.LATAM.CARD.ORDER<CARD.IS.TYPE.OF.CARD>= Y.CARD.TYPE
    R.LATAM.CARD.ORDER<CARD.IS.CARD.TYPE> = Y.CARD.NUMBER.TYPE
    R.LATAM.CARD.ORDER<CARD.IS.CARD.NUMBER> = Y.CARD.NUMBERS.LIST
    R.LATAM.CARD.ORDER<CARD.IS.CUSTOMER.NO> = R.NEW(REDO.CARD.REQ.CUSTOMER.NO)
*PACS00072694-S
    R.LATAM.CARD.ORDER<CARD.IS.PROSPECT.ID> = R.NEW(REDO.CARD.REQ.PROSPECT.ID)
*PACS00072694-E
    IF Y.CUS.NUM THEN
        R.LATAM.CARD.ORDER<CARD.IS.NAME.ON.PLASTIC> = R.NEW(REDO.CARD.REQ.CUSTOMER.NAME):@VM:Y.CUS.NUM
    END ELSE
        R.LATAM.CARD.ORDER<CARD.IS.NAME.ON.PLASTIC> = R.NEW(REDO.CARD.REQ.CUSTOMER.NAME)
    END

RETURN
*----------------------------------------------------------------------
OFS.POST:
*----------------------------------------------------------------------
    OFS.SOURCE.ID = 'REDO.OFS.LATAM.UPD'
    APPLICATION.NAME = 'LATAM.CARD.ORDER'
    TRANS.FUNC.VAL = 'I'
    TRANS.OPER.VAL = 'PROCESS'
    APPLICATION.NAME.VERSION = 'LATAM.CARD.ORDER,ADDITIONAL'
    NO.AUT = ''
    OFS.MSG.ID = ''
    APPLICATION.ID =Y.LATAM.ID
    OFS.POST.MSG = ''
    OFS.ERR = ''

    CALL OFS.BUILD.RECORD(APPLICATION.NAME,TRANS.FUNC.VAL,TRANS.OPER.VAL,APPLICATION.NAME.VERSION,"",NO.AUT,APPLICATION.ID,R.LATAM.CARD.ORDER,OFS.REQ.MSG)
    CALL OFS.POST.MESSAGE(OFS.REQ.MSG,OFS.MSG.ID,OFS.SOURCE.ID,OFS.ERR)
RETURN
END
