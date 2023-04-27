$PACKAGE APAP.TAM
SUBROUTINE REDO.V.MM.AUTH.UPD.NCF
****************************************************************
*-------------------------------------------------------------------------
* Company Name  : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By  : GANESH R
* Program Name  : REDO.V.MM.AUTH.UPD.NCF
* ODR NUMBER    : ODR-2009-10-0321
*-------------------------------------------------------------------------
* Description : This Auth routine is triggered when MM transaction is Authorised
* In parameter : None
* out parameter : None
*MODIFICATION HISTORY
*------------------------------------------------------------------------------
*DATE            WHO                REFERENCE                   DESCRIPTION
*30-06-2011      Riyas              PACS00075394           Update exact local field name - L.NCF.NUMBER
*25-APR-2023    CONVERSION TOOL     R22 AUTO CONVERSION     VM TO @VM
*25-APR-2023    CONVERSION TOOL     R22 AUTO CONVERSION     ++ TO +=1
*25-APR-2023    VICTORIA S          R22 MANUAL CONVERSION   NO CHANGE
*-------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.MM.MONEY.MARKET
    $INSERT I_F.REDO.L.NCF.STOCK
    $INSERT I_F.REDO.L.NCF.STATUS
    $INSERT I_F.REDO.L.NCF.UNMAPPED
    $INSERT I_F.REDO.NCF.ISSUED

    GOSUB INIT
    GOSUB OPEN.FILES
    GOSUB CHECK.PRELIM.CONDITIONS
    IF PROCESS.GOAHEAD THEN
        GOSUB PROCESS
    END

RETURN
*----------------------------------------------------------------------------------
*******
INIT:
******
*Initialisation
    PROCESS.GOAHEAD = 1
    STOCK.ID='SYSTEM'
    VAR.TXN.ID=''
    LRF.APP='MM.MONEY.MARKET'
*PACS00075394-S
    LRF.FIELD='L.NCF.REQUIRED':@VM:'L.NCF.NUMBER' ;*R22 AUTO CONVERSION
*PACS00075394-E
    LRF.POS=''
RETURN
***********
OPEN.FILES:
***********
*Opening Files
    FN.REDO.L.NCF.STOCK='F.REDO.L.NCF.STOCK'
    F.REDO.L.NCF.STOCK =''
    CALL OPF(FN.REDO.L.NCF.STOCK,F.REDO.L.NCF.STOCK)

    FN.REDO.L.NCF.UNMAPPED='F.REDO.L.NCF.UNMAPPED'
    F.REDO.L.NCF.UNMAPPED=''
    CALL OPF(FN.REDO.L.NCF.UNMAPPED,F.REDO.L.NCF.UNMAPPED)

    FN.REDO.NCF.ISSUED='F.REDO.NCF.ISSUED'
    F.REDO.NCF.ISSUED=''
    CALL OPF(FN.REDO.NCF.ISSUED,F.REDO.NCF.ISSUED)

    FN.REDO.L.NCF.STATUS='F.REDO.L.NCF.STATUS'
    F.REDO.L.NCF.STATUS=''
    CALL OPF(FN.REDO.L.NCF.STATUS,F.REDO.L.NCF.STATUS)
RETURN
********
PROCESS:
********
*Checking for the values in the fields and Updating the Local Field
*Getting the local Field position
    CALL MULTI.GET.LOC.REF(LRF.APP,LRF.FIELD,LRF.POS)
    POS.MM.NCF.REQ=LRF.POS<1,1>
    POS.MM.NCF.NO=LRF.POS<1,2>
    CALL  F.READU(FN.REDO.L.NCF.STOCK,STOCK.ID,R.REDO.L.NCF.STOCK,F.REDO.L.NCF.STOCK,STOCK.ERR,RETRY)
    FROM.MAIL=R.REDO.L.NCF.STOCK<ST.L.FROM.EMAIL.ID>
    TO.MAIL=R.REDO.L.NCF.STOCK<ST.L.TO.EMAIL.ID>
    SUBJ.MAIL=R.REDO.L.NCF.STOCK<ST.L.SUBJECT.MAIL>
    MSG.MAIL=R.REDO.L.NCF.STOCK<ST.L.MESSAGE.MAIL>
    GOSUB MM.PROCESS
    QTY.AVAIL=R.REDO.L.NCF.STOCK<ST.QTY.AVAIL.ORG>
    MIN.NCF=R.REDO.L.NCF.STOCK<ST.L.MIN.NCF.ORG>
    IF QTY.AVAIL LT MIN.NCF THEN
        GOSUB MAIL.ALERT
    END
RETURN

*********
MM.PROCESS:
*********
*Checking for the Local Field
    VAR.TXN.DATE=R.NEW(MM.DATE.TIME)
    VAR.DATE=20:VAR.TXN.DATE[1,6]
    VAR.TOT.INT=R.NEW(MM.TOT.INTEREST.AMT)
    VAR.CHG.AMT=R.NEW(MM.CHARGE.AMOUNT)
    VAR.TOT.CHG=R.NEW(MM.TOTAL.CHARGE)
    VAR.NCF.REQ=R.NEW(MM.LOCAL.REF)<1,POS.MM.NCF.REQ>
    CHARGE.AMOUNT=VAR.TOT.INT+VAR.CHG.AMT+VAR.TOT.CHG
    VAR.CUS=R.NEW(MM.CUSTOMER.ID)
    TXN.ID=ID.NEW
    VAR.TAX.AMT=R.NEW(MM.CHG.TAX.AMT)

    IF CHARGE.AMOUNT NE 0 THEN
        IF VAR.NCF.REQ EQ 'YES' THEN
            GOSUB PROCESS1
        END

        ELSE
            GOSUB UPDATE.TABLE2
        END
    END
RETURN
*********
PROCESS1:
*********
    VAR.SERIES=R.REDO.L.NCF.STOCK<ST.SERIES>
    VAR.BUS.DIV=R.REDO.L.NCF.STOCK<ST.BUSINESS.DIV>
    VAR.PECF=R.REDO.L.NCF.STOCK<ST.PECF>
    VAR.AICF=R.REDO.L.NCF.STOCK<ST.AICF>
    VAR.TCF=R.REDO.L.NCF.STOCK<ST.TCF>
    VAR.SEQ.NO=R.REDO.L.NCF.STOCK<ST.SEQUENCE.NO>

    IF R.REDO.L.NCF.STOCK<ST.QTY.AVAIL.ORG> GT 0 THEN
        L.NCF.NUMBER=VAR.SERIES:VAR.BUS.DIV:VAR.PECF:VAR.AICF:VAR.TCF:VAR.SEQ.NO

        VAR.PREV.RANGE=R.REDO.L.NCF.STOCK<ST.PRE.ED.RG.OR>
        VAR.PREV.RANGE=VAR.PREV.RANGE<1,DCOUNT(VAR.PREV.RANGE,@VM)> ;*R22 AUTO CONVERSION
        IF VAR.SEQ.NO EQ VAR.PREV.RANGE THEN
            VAR.STRT.RG=R.REDO.L.NCF.STOCK<ST.L.STRT.RNGE.ORG>
            VAR.SEQ.NO=VAR.STRT.RG
        END
        ELSE
            VAR.SEQ.NO += 1 ;*R22 AUTO CONVERSION
        END
        VAR.QTY.AVAIL=R.REDO.L.NCF.STOCK<ST.QTY.AVAIL.ORG>
        VAR.NCF.ISSUE=R.REDO.L.NCF.STOCK<ST.NCF.ISSUED.ORG>
        VAR.MIN.NCF=R.REDO.L.NCF.STOCK<ST.L.MIN.NCF.ORG>
        VAR.NCF.STATUS=R.REDO.L.NCF.STOCK<ST.NCF.STATUS.ORG>
        VAR.NCF.ISSUE += 1 ;*R22 AUTO CONVERSION
        VAR.QTY.AVAIL -= 1 ;*R22 AUTO CONVERSION
        IF VAR.QTY.AVAIL GE VAR.MIN.NCF THEN
            R.REDO.L.NCF.STOCK<ST.NCF.STATUS.ORG>='AVAILABLE'
        END
        ELSE
            R.REDO.L.NCF.STOCK<ST.NCF.STATUS.ORG>='UNAVAILABLE'
        END
        R.REDO.L.NCF.STOCK<ST.SEQUENCE.NO>=FMT(VAR.SEQ.NO,'8"0"R')
        R.REDO.L.NCF.STOCK<ST.QTY.AVAIL.ORG>=VAR.QTY.AVAIL
        R.REDO.L.NCF.STOCK<ST.NCF.ISSUED.ORG>=VAR.NCF.ISSUE
        CALL F.WRITE(FN.REDO.L.NCF.STOCK,STOCK.ID,R.REDO.L.NCF.STOCK)
        CALL F.RELEASE(FN.REDO.L.NCF.STOCK,'SYSTEM',F.REDO.L.NCF.STOCK)
        GOSUB UPDATE.TABLE1
    END
    ELSE
        GOSUB UPDATE.TABLE2
    END
RETURN
**************
UPDATE.TABLE1:
**************
*Updating ISSUED and Status Table
    NCF.ISSUE.ID=VAR.CUS:'.':VAR.DATE:'.':TXN.ID
    CALL F.READ(FN.REDO.NCF.ISSUED,NCF.ISSUE.ID,R.REDO.NCF.ISSUED,F.REDO.NCF.ISSUED,ISSUE.MSG)
    R.REDO.NCF.ISSUED<ST.IS.TXN.ID>=TXN.ID
    R.REDO.NCF.ISSUED<ST.IS.CHARGE.AMOUNT>=CHARGE.AMOUNT
    R.REDO.NCF.ISSUED<ST.IS.TAX.AMOUNT>=VAR.TAX.AMT
    R.REDO.NCF.ISSUED<ST.IS.DATE> = VAR.DATE
    R.REDO.NCF.ISSUED<ST.IS.CUSTOMER>=VAR.CUS
    CALL F.WRITE(FN.REDO.NCF.ISSUED,NCF.ISSUE.ID,R.REDO.NCF.ISSUED)
    CALL F.READ(FN.REDO.L.NCF.STATUS,NCF.ISSUE.ID,R.REDO.L.NCF.STATUS,F.REDO.L.NCF.STATUS,STATUS.MSG)
    R.REDO.L.NCF.STATUS<NCF.ST.TRANSACTION.ID>=ID.NEW
    R.REDO.L.NCF.STATUS<NCF.ST.CUSTOMER.ID>=VAR.CUS
    R.REDO.L.NCF.STATUS<NCF.ST.DATE>= VAR.DATE
    R.REDO.L.NCF.STATUS<NCF.ST.CHARGE.AMOUNT>=CHARGE.AMOUNT
    R.REDO.L.NCF.STATUS<NCF.ST.TAX.AMOUNT>=VAR.TAX.AMT
    R.REDO.L.NCF.STATUS<NCF.ST.NCF>=L.NCF.NUMBER
    R.NEW(MM.LOCAL.REF)<1,POS.MM.NCF.NO>=L.NCF.NUMBER
    R.REDO.L.NCF.STATUS<NCF.ST.STATUS>='AVAILABLE'
    CALL F.WRITE(FN.REDO.L.NCF.STATUS,NCF.ISSUE.ID,R.REDO.L.NCF.STATUS)
RETURN
**************
UPDATE.TABLE2:
**************
*Updating UNMAPPED and Status Table
    NCF.ISSUE.ID=VAR.CUS:'.':VAR.DATE:'.':TXN.ID
    CALL F.READ(FN.REDO.L.NCF.UNMAPPED,NCF.ISSUE.ID,R.REDO.L.NCF.UNMAPPED,F.REDO.L.NCF.UNMAPPED,UNMAP.ERR)
    R.REDO.L.NCF.UNMAPPED<ST.UN.TXN.ID>=ID.NEW
    R.REDO.L.NCF.UNMAPPED<ST.UN.CHARGE.AMOUNT>=CHARGE.AMOUNT
    R.REDO.L.NCF.UNMAPPED<ST.UN.TAX.AMOUNT>=VAR.TAX.AMT
    R.REDO.L.NCF.UNMAPPED<ST.UN.DATE>=VAR.DATE
    R.REDO.L.NCF.UNMAPPED<ST.UN.CUSTOMER>=VAR.CUS
    R.REDO.L.NCF.UNMAPPED<ST.UN.BATCH>='NO'
    CALL F.WRITE(FN.REDO.L.NCF.UNMAPPED,NCF.ISSUE.ID,R.REDO.L.NCF.UNMAPPED)
    CALL F.READ(FN.REDO.L.NCF.STATUS,NCF.ISSUE.ID,R.REDO.L.NCF.STATUS,F.REDO.L.NCF.STATUS,STATUS.MSG)
    R.REDO.L.NCF.STATUS<NCF.ST.TRANSACTION.ID>=ID.NEW
    R.REDO.L.NCF.STATUS<NCF.ST.CUSTOMER.ID>=VAR.CUS
    R.REDO.L.NCF.STATUS<NCF.ST.DATE>=VAR.DATE
    R.REDO.L.NCF.STATUS<NCF.ST.CHARGE.AMOUNT>=CHARGE.AMOUNT
    R.REDO.L.NCF.STATUS<NCF.ST.TAX.AMOUNT>=VAR.TAX.AMT
    R.REDO.L.NCF.STATUS<NCF.ST.NCF>=''
    CALL F.WRITE(FN.REDO.L.NCF.STATUS,NCF.ISSUE.ID,R.REDO.L.NCF.STATUS)
    R.REDO.L.NCF.STATUS<NCF.ST.STATUS>='UNAVAILABLE'
    CALL F.WRITE(FN.REDO.L.NCF.STATUS,NCF.ISSUE.ID,R.REDO.L.NCF.STATUS)
RETURN
**********
MAIL.ALERT:
**********
*Sending Mail Alert
    CALL ALLOCATE.UNIQUE.TIME(UNIQUE.TIME)
    VAR.UNIQUE.ID=UNIQUE.TIME
    FILENAME='APAP':VAR.UNIQUE.ID:'.TXT'
    FN.HRMS.FILE='MAIL.BP'
    F.HRMA.FILE=''
    CALL OPF(FN.HRMS.FILE ,F.HRMA.FILE)
    REC=FROM.MAIL:"#":TO.MAIL:"#":SUBJ.MAIL:"#":MSG.MAIL
    WRITE REC TO F.HRMA.FILE,FILENAME
RETURN

*-------------------------------------------------------------------
CHECK.PRELIM.CONDITIONS:
*-------------------------------------------------------------------

    VAL.STATUS=R.NEW(MM.RECORD.STATUS)


    IF VAL.STATUS[1,1] EQ 'R' THEN
        PROCESS.GOAHEAD = ""
    END

RETURN
END
