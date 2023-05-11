* @ValidationCode : MjozNzc1ODk2NTg6Q3AxMjUyOjE2ODEyMTU3MDQxNzY6OTE2Mzg6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 11 Apr 2023 17:51:44
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : 91638
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.V.AC.UPD.NCF
*-------------------------------------------------------------------------
* Company Name  : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By  : Prabhu N
* Program Name  : REDO.V.AC.UPD.NCF
* ODR NUMBER    :
*-------------------------------------------------------------------------
* Description : This Auth routine is triggered when FT transaction is Authorised
*--------------------------------------------------------------------------
*Modification History:
*----------------------------------------------------------------------------
* DATE            WHO               REFERENCE        DESCRIPTION
* 16-10-2013    Prabhu N            PACS00322624     Update exact local field name - L.NCF.NUMBER
*Modification history
*Date                Who               Reference                  Description
*11-04-2023      conversion tool     R22 Auto code conversion     VM TO @VM, FM TO @FM
*11-04-2023      Mohanraj R          R22 Manual code conversion   No changes
*---------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FT.COMMISSION.TYPE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.ACCOUNT.CLOSURE
    $INSERT I_F.REDO.L.NCF.STOCK
    $INSERT I_F.REDO.L.NCF.STATUS
    $INSERT I_F.REDO.L.NCF.UNMAPPED
    $INSERT I_F.REDO.NCF.ISSUED
    $INSERT I_F.INTERFACE.CONFIG.PRT

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
    LRF.APP='ACCOUNT.CLOSURE'
    LRF.FIELD='L.NCF.NUMBER':@VM:'L.ACL.WAIVE.CHG'
    LRF.POS=''
    VAR.TAX.AMT=''

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

    FN.COMMISSION.TYPE = 'F.FT.COMMISSION.TYPE'
    F.COMMISSION.TYPE = ''
    CALL OPF(FN.COMMISSION.TYPE,F.COMMISSION.TYPE)

    FN.ACCOUNT.HIS = 'F.ACCOUNT$HIS'
    F.ACCOUNT.HIS  = ''
    CALL OPF(FN.ACCOUNT.HIS,F.ACCOUNT.HIS)

    FN.IC.CHARGE.PRODUCT='F.IC.CHARGE.PRODUCT'
    F.IC.CHARGE.PRODUCT=''
    CALL OPF(FN.IC.CHARGE.PRODUCT,F.IC.CHARGE.PRODUCT)

RETURN
********
PROCESS:
********
*Checking for the values in the fields and Updating the Local Field

    CALL MULTI.GET.LOC.REF(LRF.APP,LRF.FIELD,LRF.POS.ALL)
    LRF.POS =LRF.POS.ALL<1,1>
    LRF.POS2=LRF.POS.ALL<1,2>

    CALL F.READU(FN.REDO.L.NCF.STOCK,STOCK.ID,R.REDO.L.NCF.STOCK,F.REDO.L.NCF.STOCK,STOCK.ERR,"")   ;*Tus Start
*  CALL CACHE.READ(FN.REDO.L.NCF.STOCK,STOCK.ID,R.REDO.L.NCF.STOCK,STOCK.ERR)  ;* Tus End
    FROM.MAIL    = R.REDO.L.NCF.STOCK<ST.L.FROM.EMAIL.ID>
    TO.MAIL      = R.REDO.L.NCF.STOCK<ST.L.TO.EMAIL.ID>
    SUBJ.MAIL    = R.REDO.L.NCF.STOCK<ST.L.SUBJECT.MAIL>
    MSG.MAIL     = R.REDO.L.NCF.STOCK<ST.L.MESSAGE.MAIL>

    GOSUB CHECK.PROCESS
RETURN

*************
CHECK.PROCESS:
*************
*Checking for the Local Field

    GOSUB GET.VALUES

    GOSUB FT.PROCESS
    QTY.AVAIL=R.REDO.L.NCF.STOCK<ST.QTY.AVAIL.ORG>
    MIN.NCF=R.REDO.L.NCF.STOCK<ST.L.MIN.NCF.ORG>
    IF QTY.AVAIL LT MIN.NCF THEN
        GOSUB MAIL.ALERT
    END

RETURN

**********
GET.VALUES:
**********

    Y.AMT.TO.CHG.CL=0
    IF R.NEW(AC.ACL.LOCAL.REF)<1,LRF.POS2> NE 'Y' THEN
        Y.AMT.TO.CHG.CL=R.NEW(AC.ACL.CLO.CHARGE.AMT)
    END

    VAR.TOT.CHG.AMT = R.NEW(AC.ACL.TOTAL.CHARGES) + Y.AMT.TO.CHG.CL
    Y.TEMP.AMOUNT=VAR.TOT.CHG.AMT
    Y.LEN.CUR    ='2'
    Y.LEN.CUR="L%":Y.LEN.CUR
    Y.TEMP.AMOUNT.FIR=FIELD(Y.TEMP.AMOUNT,'.',1)
    Y.TEMP.AMOUNT.DEC=FIELD(Y.TEMP.AMOUNT,'.',2)
    Y.TEMP.AMOUNT.DEC=FMT(Y.TEMP.AMOUNT.DEC,Y.LEN.CUR)
    VAR.TOT.CHG.AMT= Y.TEMP.AMOUNT.FIR:'.':Y.TEMP.AMOUNT.DEC

    VAR.TXN.REF     = ID.NEW
    VAR.DATE        = TODAY
    VAR.CHG.CCY     = R.NEW(AC.ACL.CURRENCY)
    VAR.CHG.AMT     = VAR.TOT.CHG.AMT
    CHARGE.AMOUNT   = VAR.CHG.CCY:' ':VAR.CHG.AMT
    VAR.ACCOUNT     = ID.NEW

    Y.VAR.ACCOUNT=VAR.ACCOUNT
    CALL EB.READ.HISTORY.REC(F.ACCOUNT.HIS,Y.VAR.ACCOUNT,R.ACCT,ERR)
    VAR.CUS = R.ACCT<AC.CUSTOMER>
    GET.ACCT.OFFICER = R.ACCT<AC.ACCOUNT.OFFICER> ;* Not required
    GET.CATEGORY     = R.ACCT<AC.CATEGORY>        ;* Not required
    TXN.ID=ID.NEW
RETURN
**********
FT.PROCESS:
**********

    IF VAR.CHG.AMT GT 0 THEN
        GOSUB PROCESS1
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
        VAR.PREV.RANGE=VAR.PREV.RANGE<1,DCOUNT(VAR.PREV.RANGE,@VM)>
        IF VAR.SEQ.NO EQ VAR.PREV.RANGE THEN
            VAR.STRT.RG=R.REDO.L.NCF.STOCK<ST.L.STRT.RNGE.ORG>
            VAR.SEQ.NO=VAR.STRT.RG
        END ELSE
            VAR.SEQ.NO += 1 ;*R22 Auto Code Conversion
        END
        VAR.QTY.AVAIL=R.REDO.L.NCF.STOCK<ST.QTY.AVAIL.ORG>
        VAR.NCF.ISSUE=R.REDO.L.NCF.STOCK<ST.NCF.ISSUED.ORG>
        VAR.MIN.NCF=R.REDO.L.NCF.STOCK<ST.L.MIN.NCF.ORG>
        VAR.NCF.STATUS=R.REDO.L.NCF.STOCK<ST.NCF.STATUS.ORG>
        VAR.NCF.ISSUE += 1 ;*R22 Auto Code Conversion
        VAR.QTY.AVAIL -= 1 ;*R22 Auto Code Conversion
        IF VAR.QTY.AVAIL GE VAR.MIN.NCF THEN
            R.REDO.L.NCF.STOCK<ST.NCF.STATUS.ORG>='AVAILABLE'
        END ELSE
            R.REDO.L.NCF.STOCK<ST.NCF.STATUS.ORG>='UNAVAILABLE'
        END

        R.REDO.L.NCF.STOCK<ST.SEQUENCE.NO>=FMT(VAR.SEQ.NO,'8"0"R')
        R.REDO.L.NCF.STOCK<ST.QTY.AVAIL.ORG>=VAR.QTY.AVAIL
        R.REDO.L.NCF.STOCK<ST.NCF.ISSUED.ORG>=VAR.NCF.ISSUE
        R.REDO.L.NCF.STOCK<ST.L.MIN.NCF.ORG>=VAR.MIN.NCF
        CALL F.WRITE(FN.REDO.L.NCF.STOCK,STOCK.ID,R.REDO.L.NCF.STOCK)

        GOSUB UPDATE.TABLE
    END ELSE
        GOSUB UPDATE.TABLE2
    END

    CALL F.RELEASE(FN.REDO.L.NCF.STOCK,STOCK.ID,F.REDO.L.NCF.STOCK)

RETURN

**********
MAIL.ALERT:
**********

    CALL ALLOCATE.UNIQUE.TIME(UNIQUE.TIME)
    VAR.UNIQUE.ID=UNIQUE.TIME
    FILENAME='APAP':VAR.UNIQUE.ID:'.TXT'
    GOSUB GET.MAIL.FOLDER
    FN.HRMS.FILE = Y.MAILIN.FOLDER
    F.HRMA.FILE=''
    CALL OPF(FN.HRMS.FILE ,F.HRMA.FILE)
    REC=FROM.MAIL:"#":TO.MAIL:"#":SUBJ.MAIL:"#":MSG.MAIL
    WRITE REC TO F.HRMA.FILE,FILENAME
RETURN

*----------------
GET.MAIL.FOLDER:
*----------------
    R.INT.CONFIG = ''
    FN.INTERFACE.CONFIG.PRT = 'F.INTERFACE.CONFIG.PRT'
    F.INTERFACE.CONFIG.PRT = ''
    CALL OPF(FN.INTERFACE.CONFIG.PRT,F.INTERFACE.CONFIG.PRT)

    CALL CACHE.READ(FN.INTERFACE.CONFIG.PRT,"email",R.INT.CONFIG,EMAILL.ERR)
    IF R.INT.CONFIG THEN
        Y.FOLDER.TYPE = R.INT.CONFIG<INTRF.MSG.INT.FLD.NAME>
        Y.FOLDER.NAME = R.INT.CONFIG<INTRF.MSG.INT.FLD.VAL>
        CHANGE @VM TO @FM IN Y.FOLDER.TYPE
        CHANGE @VM TO @FM IN Y.FOLDER.NAME

        LOCATE "EMAIL_FOLDER_PATH" IN Y.FOLDER.TYPE SETTING TYPE.POS THEN
            Y.MAILIN.FOLDER = Y.FOLDER.NAME<TYPE.POS>
        END

        LOCATE "ERROR_FOLDER_PATH" IN Y.FOLDER.TYPE SETTING TYPE.POS THEN
            Y.ERROR.FOLDER = Y.FOLDER.NAME<TYPE.POS>
        END
    END

RETURN

**************
UPDATE.TABLE:
**************
*Updating ISSUED and Status Table
    NCF.ISSUE.ID=VAR.CUS:'.':VAR.DATE:'.':TXN.ID
    CALL F.READ(FN.REDO.NCF.ISSUED,NCF.ISSUE.ID,R.REDO.NCF.ISSUED,F.REDO.NCF.ISSUED,ISSUE.MSG)
    R.REDO.NCF.ISSUED<ST.IS.TXN.ID>=TXN.ID
    R.REDO.NCF.ISSUED<ST.IS.CHARGE.AMOUNT>=CHARGE.AMOUNT
    R.REDO.NCF.ISSUED<ST.IS.DATE> = VAR.DATE
    R.REDO.NCF.ISSUED<ST.IS.CUSTOMER>=VAR.CUS
    R.REDO.NCF.ISSUED<ST.IS.ACCOUNT> = VAR.ACCOUNT
    NCF.CNT = DCOUNT(R.REDO.NCF.ISSUED<ST.IS.NCF>,@VM)
    R.REDO.NCF.ISSUED<ST.IS.NCF,NCF.CNT+1> = L.NCF.NUMBER
    R.NEW(AC.ACL.LOCAL.REF)<1,LRF.POS>     =L.NCF.NUMBER

    CALL F.WRITE(FN.REDO.NCF.ISSUED,NCF.ISSUE.ID,R.REDO.NCF.ISSUED)
    CALL F.READ(FN.REDO.L.NCF.STATUS,NCF.ISSUE.ID,R.REDO.L.NCF.STATUS,F.REDO.L.NCF.STATUS,STATUS.MSG)
    R.REDO.L.NCF.STATUS<NCF.ST.TRANSACTION.ID>=ID.NEW
    R.REDO.L.NCF.STATUS<NCF.ST.CUSTOMER.ID>=VAR.CUS
    R.REDO.L.NCF.STATUS<NCF.ST.DATE>= VAR.DATE
    R.REDO.L.NCF.STATUS<NCF.ST.CHARGE.AMOUNT>=CHARGE.AMOUNT
    NCF.ST.CNT = DCOUNT(R.REDO.L.NCF.STATUS<NCF.ST.NCF>,@VM)
    R.REDO.L.NCF.STATUS<NCF.ST.NCF,NCF.ST.CNT+1>=L.NCF.NUMBER
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
    R.REDO.L.NCF.UNMAPPED<ST.UN.TXN.TYPE>=''
    R.REDO.L.NCF.UNMAPPED<ST.UN.TAX.AMOUNT>=VAR.TAX.AMT
    R.REDO.L.NCF.UNMAPPED<ST.UN.DATE>=VAR.DATE
    R.REDO.L.NCF.UNMAPPED<ST.UN.CUSTOMER>=VAR.CUS
    R.REDO.L.NCF.UNMAPPED<ST.UN.ACCOUNT> = VAR.ACCOUNT
    R.REDO.L.NCF.UNMAPPED<ST.UN.BATCH>='NO'
    CALL F.WRITE(FN.REDO.L.NCF.UNMAPPED,NCF.ISSUE.ID,R.REDO.L.NCF.UNMAPPED)
    CALL F.READ(FN.REDO.L.NCF.STATUS,NCF.ISSUE.ID,R.REDO.L.NCF.STATUS,F.REDO.L.NCF.STATUS,STATUS.MSG)
    R.REDO.L.NCF.STATUS<NCF.ST.TRANSACTION.ID>=ID.NEW
    R.REDO.L.NCF.STATUS<NCF.ST.CUSTOMER.ID>=VAR.CUS
    R.REDO.L.NCF.STATUS<NCF.ST.DATE>=VAR.DATE
    R.REDO.L.NCF.STATUS<NCF.ST.CHARGE.AMOUNT>=CHARGE.AMOUNT
    R.REDO.L.NCF.STATUS<NCF.ST.TAX.AMOUNT>=VAR.TAX.AMT
    R.REDO.L.NCF.STATUS<NCF.ST.NCF>=''
    R.REDO.L.NCF.STATUS<NCF.ST.STATUS>='UNAVAILABLE'
    CALL F.WRITE(FN.REDO.L.NCF.STATUS,NCF.ISSUE.ID,R.REDO.L.NCF.STATUS)
RETURN
*-------------------------------------------------------------------
CHECK.PRELIM.CONDITIONS:
*-------------------------------------------------------------------
    VAL.STATUS = R.NEW(AC.ACL.RECORD.STATUS)

    IF VAL.STATUS[1,1] EQ 'R' THEN
        PROCESS.GOAHEAD = ""
    END

RETURN
*----------------------------------------------------------------------
END
