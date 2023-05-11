* @ValidationCode : MjotNjU0ODUxMTk2OkNwMTI1MjoxNjgxMjc2NTUyMzUyOklUU1M6LTE6LTE6MTczNToxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 12 Apr 2023 10:45:52
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 1735
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDORETAIL
SUBROUTINE LATAM.CARD.ORDER.SPLIT.VALIDATE
*-----------------------------------------------------------------------------------
* Company   Name    : APAP
* Developed By      : Temenos Application Management
* Program   Name    : LATAM.CARD.ORDER.SPLIT.VALIDATE
*-----------------------------------------------------------------------------------
*DESCRIPTION
*-----------
* This is a routine called from LATAM.CARD.ORDER.VALIDATE
* This routine validate and update the necessary fields in LATAM.CARD.ORDER
*
*-----------------------------------------------------------------------------------------
* Input / Output
* --------------
* IN     : -na-
* OUT    : -na-
* Dependencies
* ------------
* CALLS     : F.READ
*-------------------------------------------------------------------------------------------
* Revision History
* ----------------

* Date              Who                        Reference          Description
* 17-OCT-2008     Mohan.N                     STBP20081017        Initial creation
* 22-JAN-2009     KarthiK                     STBP20090122        Changes made in Issue stage validation
* 11-MAR-2010     Gassali.S.K                 BPCR20100311        Validations for New fields added
* 13-MAY-2011     KAVITHA                     ODR-2010-08-0467    PACS00055017  FIX
* 27 MAY 2011     KAVITHA                     PACS00063156        PACS00063156 FIX
* 16 JUN 2011     KAVITHA                     PACS00072694        PROSPECT DETAILS ADDED
* 11 JUL 2011     KAVITHA                     PACS00063138        FIX FOR PACS00063138
* 06 SEP 2011     KAVITHA                     PACS00126010        PACS00126010 FIX
* 26 SEP 2011     KAVITHA                     PACS00088607        PACS00088607 FIX
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*10-04-2023            CONVERSION TOOL                AUTO R22 CODE CONVERSION            = TO EQ , VM TO @VM , FM TO @FM
*10-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*-------------------------------------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_METHODS.AND.PROPERTIES
    $INSERT I_F.COMPANY
    $INSERT I_F.CARD.TYPE
    $INSERT I_F.DATES
    $INSERT I_F.PAYMENT.STOP.TYPE

    $INSERT I_F.CURRENCY
    $INSERT I_F.ACCOUNT

    $INSERT I_F.CARD.STATUS
*   $INSERT I_F.COMPANY ;* R22 Auto conversion
    $INSERT I_F.CARD.REPAYMENT.DATE
    $INSERT I_F.CARD.BILL.CLOSE.DATE
    $INSERT I_F.STOCK.REGISTER
    $INSERT I_F.STOCK.PARAMETER
    $INSERT I_F.CUSTOMER
    $INSERT I_F.DEPT.ACCT.OFFICER

    $INSERT I_F.LOCAL.TABLE
    $INSERT I_GTS.COMMON

    $INSERT I_F.LATAM.CARD.CLASS.CODE
    $INSERT I_F.LATAM.CARD.ORDER
    $INSERT I_F.LATAM.CARD.DELIVERY.ADDRESS
*   $INSERT I_F.LATAM.CARD.ORDER ;* R22 Auto conversion
    $INSERT I_F.REDO.CARD.BIN
*TUS START
    $INSERT I_F.EB.CONTRACT.BALANCES
*TUS END
*----------------------------------------------------------------------------------------------------


    GOSUB INITALISE
    GOSUB CROSS.VALIDATION


RETURN
*----------------------------------------------------------------------------------------------------
INITALISE:
*----------------------------------------------------------------------------------------------------
    CLAS.CODE = ''
    ONLINE.LMT = ''
    CARD.TYPE = ''
    ISSUE.INDICATOR = ''

    FULL.ID.FLAG = ''

    FN.LATAM.CARD.ORDER = 'F.LATAM.CARD.ORDER'
    F.LATAM.CARD.ORDER = ''
    CALL OPF(FN.LATAM.CARD.ORDER,F.LATAM.CARD.ORDER)

    FN.REDO.CARD.BIN = 'F.REDO.CARD.BIN'
    F.REDO.CARD.BIN = ''
    CALL OPF(FN.REDO.CARD.BIN,F.REDO.CARD.BIN)

    FN.DEL.ADD = "F.LATAM.CARD.DELIVERY.ADDRESS"
    F.DEL.ADD = ""
    CALL OPF(FN.DEL.ADD,F.DEL.ADD)

    FN.CARD.TYPE = "F.CARD.TYPE"
    F.CARD.TYPE = ""
    CALL OPF(FN.CARD.TYPE,F.CARD.TYPE)

    FN.ACCOUNT = "F.ACCOUNT"
    F.ACCOUNT = ""
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    FN.CLASS.CODE = "F.LATAM.CARD.CLASS.CODE"
    F.CLASS.CODE = ""
    CALL OPF(FN.CLASS.CODE,F.CLASS.CODE)

    FN.CARD.ORDER = 'F.LATAM.CARD.ORDER'
    F.CARD.ORDER = ''
    CALL OPF(FN.CARD.ORDER,F.CARD.ORDER)

RETURN
*----------------------------------------------------------------------------------------------------
CROSS.VALIDATION:
*----------------------------------------------------------------------------------------------------

*Cross validation process done here

    GOSUB READING.RECORDS
    ISSUE.INDICATOR = ''
    ISSUE.INDICATOR = R.NEW(CARD.IS.ISSUE.INDICATOR)
    ISSUE.STAGE = R.NEW(CARD.IS.ISSUE.STAGE)

*--------------ISSUE STAGE UPDATION-------------
    BEGIN CASE

        CASE CARD.STATUS EQ '93'
            R.NEW(CARD.IS.ISSUE.STAGE) = "CANCELLED"

        CASE CARD.STATUS EQ '95'
            R.NEW(CARD.IS.ISSUE.STAGE) = "DESTROYED"

        CASE CARD.STATUS EQ '96'
            R.NEW(CARD.IS.ISSUE.STAGE) = "BLOCKED"

        CASE CARD.STATUS EQ 90

            R.NEW(CARD.IS.ISSUE.STAGE) = 'ISSUED'
            ISSUE.STAGE = R.NEW(CARD.IS.ISSUE.STAGE)

*    CASE CARD.STATUS EQ '94'
        CASE CARD.STATUS EQ '94' AND ISSUE.INDICATOR NE 'RENEWAL'
            R.NEW(CARD.IS.ISSUE.STAGE) = "ACTIVE"

        CASE CARD.STATUS EQ '92'
            R.NEW(CARD.IS.ISSUE.STAGE) = "SCRAP"

        CASE CARD.STATUS EQ '91'
            R.NEW(CARD.IS.ISSUE.STAGE) = "RETURN"

    END CASE


    TYPE.OF.CARD = R.NEW(CARD.IS.TYPE.OF.CARD)

    GOSUB CARD.TYPE.VALIDATION

    PLACE.DELIVERY = R.NEW(CARD.IS.DELIVERY.PLACE)
    IF PLACE.DELIVERY THEN
        BEGIN CASE
            CASE PLACE.DELIVERY EQ "1"
                R.NEW(CARD.IS.DELIVERY.ADD)<1,1> = R.PLACE.DEL<SBP.DC.DELIVERY.ADDRESS>
            CASE PLACE.DELIVERY EQ "2"
                R.NEW(CARD.IS.DELIVERY.ADD)<1,1> = R.PLACE.DEL<SBP.DC.DELIVERY.ADDRESS>
            CASE PLACE.DELIVERY EQ "3"
                R.NEW(CARD.IS.DELIVERY.ADD)<1,1> = R.CUSTOMER<EB.CUS.ADDRESS>
        END CASE
    END

    GOSUB CONTINUE.CROSS.VALIDATION

RETURN

*-------------------------------------------------------------------------------------------------------
CONTINUE.CROSS.VALIDATION:

*PACS00072694 -S


*----------CARD.IS.ISSUE.INDICATOR------
* check for reissue and renewal

    IF CARD.STATUS EQ '90' AND (Y.LATAM.CURR.NO EQ '') THEN
        R.NEW(CARD.IS.ISSUE.INDICATOR) = "ISSUE"
    END

*------CARD.IS.LIMIT.CONTROL------------
    LMT.CONTROL =""
    LMT.CONTROL = R.NEW(CARD.IS.LIMIT.CONTROL)
    LMT.CNTRL.AMT = R.NEW(CARD.IS.LIMIT.CONTROL.AMT)
    LMT.CNTRL.FREQ   = R.NEW(CARD.IS.FREQ.LIMIT.CONTROL)
    OLD.LMT.AMT = R.OLD(CARD.IS.LIMIT.CONTROL.AMT)
    LAST.LMT.AMT = R.NEW.LAST(CARD.IS.LIMIT.CONTROL.AMT)

    GOSUB LIMIT.CONTROL.VALIDATION
*------------------CATGORY.CARD----------

    IF R.NEW(CARD.IS.CATEGORY.CARD) EQ "" THEN
        R.NEW(CARD.IS.CATEGORY.CARD)= FIELD(R.CARD.TYPE<CARD.TYPE.CATEGORY>,@VM,1)
    END

*---------------------BLOCK.STATUS-------
*PACS00063156-S
    BLOCK.STATUS = R.NEW(CARD.IS.BLOCK.STATUS)

*PACS00063156-E
*---------------------CARD.TYPE----------

    IF R.NEW(CARD.IS.CARD.TYPE) EQ "" THEN ;*AUTO R22 CODE CONVERSION
        R.NEW(CARD.IS.CARD.TYPE) = Y.CARD.TYPE.ID
    END

RETURN
*---------------------------------------------------------------------------------------------------
LIMIT.CONTROL.VALIDATION:
*-----------------------------------------------------------------------------------------------------
*
*TUS START
*  PRINCIPAL.AMT = R.ACCOUNT<AC.ONLINE.ACTUAL.BAL>
    PRINCIPAL.AMT = R.ECB<ECB.ONLINE.ACTUAL.BAL>
*TUS END
    IF R.CLASS.CODE THEN

        GOSUB PART.LIMIT.CONTROL.VALIDATION

        IF LMT.CONTROL EQ 'NO' THEN
            GOSUB LIMIT.AMT.VALIDATION
        END ELSE
            IF LMT.CNTRL.AMT EQ '' THEN
                R.NEW(CARD.IS.LIMIT.CONTROL) = 'NO'
                R.NEW(CARD.IS.LIMIT.CONTROL.AMT) = ONLINE.LMT
            END
        END
    END

RETURN
*---------------------------------------------------------------------------------------------------------
PART.LIMIT.CONTROL.VALIDATION:

    GET.START = R.CLASS.CODE<SBP.DC.START.BAL.RANGE>
    GET.END = R.CLASS.CODE<SBP.DC.END.BAL.RANGE>
    COUNT.START = DCOUNT(GET.START,@VM)

    FOR EACH.VALUE = 1 TO  COUNT.START
        GOSUB LOOP.COUNTER.CHECK
    NEXT EACH.VALUE

    R.NEW(CARD.IS.CLASS.CODE) = CLAS.CODE
    LMT.CNTRL.DATE = R.NEW(CARD.IS.DATE.LIMIT.CONTROL)
    R.NEW(CARD.IS.DATE.LIMIT.CONTROL) = TODAY

RETURN
*---------------------------------------------------------------------------------------------------------
LOOP.COUNTER.CHECK:

    GET.START.AMT = FIELD(GET.START,@VM,EACH.VALUE)
    GET.END.AMT = FIELD(GET.END,@VM,EACH.VALUE)
    IF  PRINCIPAL.AMT GE GET.START.AMT THEN
        IF PRINCIPAL.AMT LE GET.END.AMT THEN
            ONLINE.LMT = FIELD(R.CLASS.CODE<SBP.DC.ONLINE.LIMIT>,@VM,EACH.VALUE)
            CLAS.CODE = FIELD(R.CLASS.CODE<SBP.DC.CLASS.CODE>,@VM,EACH.VALUE)
            EACH.VALUE = COUNT.START
        END
    END
RETURN
*---------------------------------------------------------------------------------------------------------
LIMIT.AMT.VALIDATION:
*---------------------------------------------------------------------------------------------------------
*
    R.NEW(CARD.IS.LIMIT.CONTROL) = 'NO'
    IF LMT.CNTRL.FREQ THEN
        AF = CARD.IS.FREQ.LIMIT.CONTROL
        ETEXT = "EB-INPUT.NOT.ALLOWED"
        CALL STORE.END.ERROR
    END
    IF (OLD.LMT.AMT EQ LMT.CNTRL.AMT) OR (LAST.LMT.AMT EQ LMT.CNTRL.AMT) THEN
        R.NEW(CARD.IS.LIMIT.CONTROL) = 'NO'
        R.NEW(CARD.IS.LIMIT.CONTROL.AMT) = ONLINE.LMT
    END ELSE
        IF LMT.CNTRL.AMT NE ONLINE.LMT THEN
            AF = CARD.IS.LIMIT.CONTROL.AMT
            ETEXT = "EB-INPUT.NOT.ALLOWED"
            CALL STORE.END.ERROR
        END
    END


RETURN
*---------------------------------------------------------------------------------------------------------
CARD.TYPE.VALIDATION:
*---------------------------------------------------------------------------------------------------------
*


    BEGIN CASE

        CASE TYPE.OF.CARD EQ 'PRINCIPAL'

            R.NEW(CARD.IS.CARD.NUMBER)<1,1> = ID.NEW

            IF R.ACCOUNT THEN
                R.NEW(CARD.IS.CUSTOMER.NO)<1,1> =  R.ACCOUNT<AC.CUSTOMER>

*PACS00088607 -S
                COMI.ENRI = ''

                COMI.ENRI = R.CUSTOMER<EB.CUS.SHORT.NAME>

                IF OFS$BROWSER THEN
                    OFS$ENRI<CARD.IS.CUSTOMER.NO,1> = COMI.ENRI
                END

*PACS00088607 -E

            END

*PACS00126010 -S
            IF R.NEW(CARD.IS.EMBOSS.TYPE) EQ "PREEMBOZADA" THEN

                CUS.NAME = R.CUSTOMER<EB.CUS.NAME.1,1>
                R.NEW(CARD.IS.NAME.ON.PLASTIC)<1,1> = CUS.NAME
*PACS00126010 -E
            END

            CUS.SHORT.NAME = R.CUSTOMER<EB.CUS.SHORT.NAME,1>
            R.NEW(CARD.IS.NAME)<1,1> = CUS.SHORT.NAME
*PACS00063138  -E

*PACS00072694 -E

*PACS00055017 -S
        CASE TYPE.OF.CARD EQ 'ADICIONAL'

            GOSUB ADDITIONAL.CARD.CHECK

    END CASE
*PACS00055017 -E


    ISSUE.NO = ''
    IF ISSUE.INDICATOR EQ 'REISSUE' OR ISSUE.INDICATOR EQ 'RENEWAL' THEN
        ISSUE.NO = R.NEW(CARD.IS.ISSUE.NUMBER)
        GOSUB ISSUE.NO.CHECK
    END

RETURN
*---------------------------------------------------------------------------------------------------------
ISSUE.NO.CHECK:

    IF ISSUE.NO EQ '' THEN
        AF = CARD.IS.ISSUE.NUMBER
        ETEXT = "EB-INPUT.MISSING"
        CALL STORE.END.ERROR
    END ELSE
        IF ISSUE.NO NE "SAME" AND ISSUE.INDICATOR EQ 'RENEWAL' THEN
            AF = CARD.IS.ISSUE.NUMBER
            ETEXT = "EB-SHOULD.BE.SAME"
            CALL STORE.END.ERROR
        END
    END

*    IF CARD.STATUS EQ '90' AND ISSUE.NO NE '' THEN
*       R.NEW(CARD.IS.ISSUE.NUMBER) = ''
*      R.NEW(CARD.IS.ISSUE.INDICATOR) = ''
*  END

RETURN
*---------------------------------------------------------------------------------------------------------
ADDITIONAL.CARD.CHECK:


    Y.CARD.ORDER.ID = ''

*PACS00072694-S
    R.NEW(CARD.IS.CARD.NUMBER)<1,1> = ID.NEW

    GET.PROS.FINAL.ID = R.NEW(CARD.IS.PROSPECT.ID)
    CALL F.READ(FN.CUSTOMER,GET.PROS.FINAL.ID,R.CUSTOMER.PROS,F.CUSTOMER,CUS.ERR)
    IF R.CUSTOMER.PROS THEN

        COMI.ENRI = ''

        PROS.CUS.NAME = R.CUSTOMER.PROS<EB.CUS.NAME.1,1>
        COMI.ENRI = R.CUSTOMER.PROS<EB.CUS.SHORT.NAME>

*PACS00088607 -S

        R.NEW(CARD.IS.CUSTOMER.NO)<1,1> = GET.PROS.FINAL.ID

        IF OFS$BROWSER THEN
            OFS$ENRI<CARD.IS.CUSTOMER.NO,1> = COMI.ENRI
        END


    END


    IF R.ACCOUNT THEN
        R.NEW(CARD.IS.CUSTOMER.NO)<1,2> =  R.ACCOUNT<AC.CUSTOMER>

        COMI.ENRI = ''

        COMI.ENRI = R.CUSTOMER<EB.CUS.SHORT.NAME>

        IF OFS$BROWSER THEN
            OFS$ENRI<CARD.IS.CUSTOMER.NO,2> = COMI.ENRI
        END

        R.NEW(CARD.IS.NAME.ON.PLASTIC)<1,2> = R.CUSTOMER<EB.CUS.NAME.1>
* PACS00088607 -E

    END

*PACS00126010 -S
    IF R.NEW(CARD.IS.EMBOSS.TYPE) EQ "PREEMBOZADA" THEN


        R.NEW(CARD.IS.NAME.ON.PLASTIC)<1,1> = PROS.CUS.NAME
    END
*PACS00126010  -E
    PROS.SHORT.NAME = R.CUSTOMER.PROS<EB.CUS.SHORT.NAME,1>

*PACS00063138 -S
    R.NEW(CARD.IS.NAME)<1,1> = PROS.SHORT.NAME
*PACS00063138 -E

*PACS00072694 -S

*PACS00072694 -E

    GET.PRIN.CARD.NO = R.NEW(CARD.IS.CARD.NUMBER)<1,2>
    FINDSTR '.' IN GET.PRIN.CARD.NO SETTING DOT.POS THEN
        PRIN.CARD.NO = FIELD(GET.PRIN.CARD.NO,".",2)
        FULL.ID.FLAG = 1
    END ELSE
        PRIN.CARD.NO = GET.PRIN.CARD.NO
    END

    IF PRIN.CARD.NO THEN

        IF FULL.ID.FLAG NE 1 THEN
            GOSUB GET.LATAM.ID
        END ELSE
            Y.CARD.ORDER.ID = GET.PRIN.CARD.NO
        END

        IF Y.CARD.ORDER.ID THEN
            R.CARD.ORDER = ''
            CARD.ORDER.ERR =''

            CALL F.READ(FN.CARD.ORDER,Y.CARD.ORDER.ID,R.CARD.ORDER,F.CARD.ORDER,CARD.ORDER.ERR)

            R.NEW(CARD.IS.DELIVERY.PLACE)<1,2> = R.CARD.ORDER<CARD.IS.DELIVERY.PLACE>
            R.NEW(CARD.IS.DELIVERY.ADD)<1,2> = R.CARD.ORDER<CARD.IS.DELIVERY.ADD>
        END ELSE

            AF = CARD.IS.CARD.NUMBER
            AV = 2
            ETEXT = "EB-PRIM.VALIDATE"
            CALL STORE.END.ERROR

        END
    END


RETURN
*---------------------------------------------------------------------------------------------------------
GET.LATAM.ID:

    Y.BIN  = PRIN.CARD.NO[1,6]
    CALL F.READ(FN.REDO.CARD.BIN,Y.BIN,R.REDO.CARD.BIN,F.REDO.CARD.BIN,Y.ERR)
    Y.Y.CARD.NUMBER.PRI = R.REDO.CARD.BIN<REDO.CARD.BIN.CARD.TYPE>
    Y.COUNT = DCOUNT(Y.Y.CARD.NUMBER.PRI,@VM)

    Y.CNT =1
    LOOP
    WHILE Y.CNT LE Y.COUNT
        Y.Y.CARD.NUMBER.PRIMARY = Y.Y.CARD.NUMBER.PRI<1,Y.CNT>
        Y.PRIMARY = Y.Y.CARD.NUMBER.PRIMARY:'.':PRIN.CARD.NO
        CALL F.READ(FN.LATAM.CARD.ORDER,Y.PRIMARY,R.LATAM.CARD.ORDER.PRI,F.LATAM.CARD.ORDER,LATAM.ERR)
        IF R.LATAM.CARD.ORDER.PRI THEN
            Y.CARD.ORDER.ID = Y.PRIMARY
            RETURN
        END
        Y.CNT += 1
    REPEAT

RETURN
*------------------------------------------------------------------------
BLOCK.STATUS.VALIDATION:
*---------------------------------------------------------------------------------------------------------

    BLOCK.STAT = ''
    IF CARD.STATUS EQ '94' THEN
        BLOCK.STAT = R.NEW(CARD.IS.BLOCK.STATUS)
        IF BLOCK.STAT EQ '' THEN
            R.NEW(CARD.IS.BLOCK.STATUS) = "WITHOUT BLOCK"
        END
    END
    IF BLOCK.STATUS THEN
        GOSUB BLOCK.STATUS.CHECK
    END

RETURN

*---------------------------------------------------------------------------------------------------------
BLOCK.STATUS.CHECK:

    BEGIN CASE
        CASE BLOCK.STATUS EQ 'CANCELLED' OR BLOCK.STATUS EQ 'ACCOUNT CANCELLED'
            IF CARD.STATUS NE "93" THEN
                AF = CARD.IS.BLOCK.STATUS
                ETEXT =  "EB-INVALID-STATUS"
                CALL STORE.END.ERROR
            END
        CASE BLOCK.STATUS EQ 'STOLEN' OR BLOCK.STATUS EQ 'LOST' OR BLOCK.STATUS EQ 'CUSTOMER REQUEST'
            IF CARD.STATUS NE "93" THEN
                AF = CARD.IS.BLOCK.STATUS
                ETEXT = "EB-INVALID-STATUS"
                CALL STORE.END.ERROR
            END
        CASE BLOCK.STATUS EQ "BLOCKED DUE TO DAMAGE"
            IF CARD.STATUS NE "95" THEN
                AF = CARD.IS.BLOCK.STATUS
                ETEXT = "EB-INVALID-STATUS"
                CALL STORE.END.ERROR

            END
        CASE BLOCK.STATUS EQ "JUDICIAL ORDER" OR BLOCK.STATUS EQ 'NOT ACTIVATED'
            IF CARD.STATUS NE '96' THEN
                AF = CARD.IS.BLOCK.STATUS
                ETEXT = "EB-INVALID-STATUS"
                CALL STORE.END.ERROR
            END

        CASE BLOCK.STATUS EQ 'WITHOUT BLOCK'
            IF CARD.STATUS NE '94' THEN
                AF = CARD.IS.BLOCK.STATUS
                ETEXT = "EB-INVALID-STATUS"
                CALL STORE.END.ERROR
            END

    END CASE

RETURN

*---------------------------------------------------------------------------------------------------------
READING.RECORDS:
*---------------------------------------------------------------------------------------------------------
*


    Y.ACCOUNT.ID = R.NEW(CARD.IS.ACCOUNT)<1,1>
    R.ACCOUNT = ''
    ACCOUNT.ERR = ''
*TUS START
    R.ECB = '' ; ECB.ERR ='' ;
    CALL F.READ(FN.ACCOUNT,Y.ACCOUNT.ID,R.ACCOUNT,F.ACCOUNT,ACCOUNT.ERR)
    CALL EB.READ.HVT('EB.CONTRACT.BALANCES', Y.ACCOUNT.ID, R.ECB, ECB.ERR)
*TUS END
    CUS.ID = R.ACCOUNT<AC.CUSTOMER>
    R.CUSTOMER = ''
    CUSTOMER.ERR = ''
    CALL F.READ(FN.CUSTOMER,CUS.ID,R.CUSTOMER,F.CUSTOMER,CUSTOMER.ERR)

    Y.PLACE.DEL = R.NEW(CARD.IS.DELIVERY.PLACE)
    R.PLACE.DEL = ''
    PLACE.DEL.ERR = ''
    CALL F.READ(FN.DEL.ADD,Y.PLACE.DEL,R.PLACE.DEL,F.DEL.ADD,PLACE.DEL.ERR)

    LCL.CLASS.CODE.ID = ID.COMPANY
    R.CLASS.CODE = ''
    CLASS.CODE.ERR = ''
    CALL F.READ(FN.CLASS.CODE,LCL.CLASS.CODE.ID,R.CLASS.CODE,F.CLASS.CODE,CLASS.CODE.ERR)

    Y.CARD.TYPE.ID = FIELD(ID.NEW,'.',1)
    R.CARD.TYPE = ''
    CARD.TYPE.ERR = ''
    CALL F.READ(FN.CARD.TYPE,Y.CARD.TYPE.ID,R.CARD.TYPE,F.CARD.TYPE,CARD.TYPE.ERR)

    CARD.STATUS = R.NEW(CARD.IS.CARD.STATUS)
    Y.LATAM.CURR.NO = R.NEW(CARD.IS.CURR.NO)

RETURN
*---------------------------------------------------End----------------------------------------
END
