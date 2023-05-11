$PACKAGE APAP.REDOENQ
SUBROUTINE NOFILE.REDO.APAP.CARD.REQ(Y.FINAL.ARRAY)
*-----------------------------------------------------------------------------
* Company Name : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By : A.C.Rajkumar
* Program Name : NOFILE.REDO.APAP.CARD.REQ
* ODR NUMBER : ODR-2010-03-0092
*----------------------------------------------------------------------------------
* Description : This is a Nofile routine for the Enquiry REDO.APAP.CARD.REQ.REP
* to display the on line report needed to keep branches requests
* control that have been closed
* In parameter : None
* out parameter : None
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------
* DATE WHO REFERENCE DESCRIPTION
* 09-09-2010 A.C.Rajkumar ODR-2010-03-0092 Initial Creation
*  DATE             WHO                   REFERENCE 
* 06-APRIL-2023      Conversion Tool       R22 Auto Conversion  - VM to @VM , FM to @FM ,CONVERT TO CHANGE and ++ to +=1 and F.READ to CACHE.READ
* 06-APRIL-2023      Harsha                R22 Manual Conversion - No changes 
* ----------------------------------------------------------------------------
*
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.CARD.TYPE
    $INSERT I_F.COMPANY
    $INSERT I_F.EB.LOOKUP
    $INSERT I_F.REDO.CARD.REQUEST
*
    GOSUB INIT
    GOSUB LOCATE.VARS
    GOSUB SELECT.IDS
    GOSUB PROCESS
RETURN
*
*****
INIT:
*****
*
    FN.REDO.CARD.REQUEST = 'F.REDO.CARD.REQUEST'
    F.REDO.CARD.REQUEST = ''
    CALL OPF(FN.REDO.CARD.REQUEST, F.REDO.CARD.REQUEST)
*
    FN.COMPANY = 'F.COMPANY'
    F.COMPANY = ''
    CALL OPF(FN.COMPANY, F.COMPANY)
*
    FN.CARD.TYPE = 'F.CARD.TYPE'
    F.CARD.TYPE = ''
    CALL OPF(FN.CARD.TYPE, F.CARD.TYPE)
*
    FN.EB.LOOKUP = 'F.EB.LOOKUP'
    F.EB.LOOKUP = ''
    CALL OPF(FN.EB.LOOKUP, F.EB.LOOKUP)
*
    Y.LRF.APPL = 'CARD.TYPE'
    Y.LRF.FIELDS = 'L.CT.SUMIN.CO'
    FIELD.POS = ''
*
    CALL GET.LOC.REF(Y.LRF.APPL, Y.LRF.FIELDS, FIELD.POS)
*
    Y.PROD.COD.POS = FIELD.POS<1,1>
*
    Y.DUMMY = ''
    Y.JOIN.DATE = ''
    Y.USER = ''
    Y.REQUEST.TYPE = ''
    Y.PRODUCT.TYPE = ''
    Y.BRANCH = ''
    Y.TEMP.CARD = ''
    Y.TEMP.AMT.CARDS = ''
    Y.TEMP.CARD = ''
    Y.TEMP.AMT.CARDS = ''
    Y.RET.ARRAY = ''
    Y.FIN.DISP = ''
    Y.FINAL.TOTAL = ''
RETURN
*
************
LOCATE.VARS:
************
*
    LOCATE "DATE" IN D.FIELDS<1> SETTING DATE.POS THEN
        Y.JOIN.DATE = D.RANGE.AND.VALUE<DATE.POS>
    END

    LOCATE "USER" IN D.FIELDS<1> SETTING USER.POS THEN
        Y.USER = D.RANGE.AND.VALUE<USER.POS>
    END

    LOCATE "REQUEST.TYPE" IN D.FIELDS<1> SETTING REQUEST.TYPE.POS THEN
        Y.REQUEST.TYPE = D.RANGE.AND.VALUE<REQUEST.TYPE.POS>
    END

    LOCATE "PRODUCT.TYPE" IN D.FIELDS<1> SETTING PRODUCT.TYPE.POS THEN
        Y.PRODUCT.TYPE = D.RANGE.AND.VALUE<PRODUCT.TYPE.POS>
    END

    LOCATE "BRANCH" IN D.FIELDS<1> SETTING BRANCH.POS THEN
        Y.BRANCH = D.RANGE.AND.VALUE<BRANCH.POS>
    END
RETURN
*
***********
SELECT.IDS:
************
*
    SEL.CMD = "SELECT ":FN.REDO.CARD.REQUEST
*
    IF Y.JOIN.DATE NE '' THEN
        SEL.CMD := " AND WITH DATE EQ ":Y.JOIN.DATE
    END

    IF Y.USER NE '' THEN
        SEL.CMD := " AND WITH AUTHORISER LK ...":Y.USER:"..."
    END

    IF Y.REQUEST.TYPE EQ 'YES' THEN
        SEL.CMD := " AND WITH AUTO.REQUEST.FLAG EQ 'YES'"
    END

    IF Y.REQUEST.TYPE EQ 'NO' THEN
        SEL.CMD := " AND WITH AUTO.REQUEST.FLAG NE 'YES'"
    END

    IF Y.PRODUCT.TYPE NE '' THEN
        SEL.CMD := " AND WITH CARD.TYPE EQ ":Y.PRODUCT.TYPE
    END
    IF Y.BRANCH NE '' THEN
        SEL.CMD := " AND WITH AGENCY EQ ":Y.BRANCH
    END
RETURN
*
********
PROCESS:
********
*
    CALL EB.READLIST(SEL.CMD,SEL.LIST,"",NO.OF.REC,RET.CODE)
*
    LOOP
        REMOVE Y.ID FROM SEL.LIST SETTING Y.POS
    WHILE Y.ID:Y.POS
*
        CALL F.READ(FN.REDO.CARD.REQUEST, Y.ID, R.REC.REDO.CARD.REQUEST, F.REDO.CARD.REQUEST, Y.ERR.REDO.CARD.REQUEST)
        IF R.REC.REDO.CARD.REQUEST THEN
            Y.REQ.CODE = Y.ID
            Y.REQUEST.TYPE = R.REC.REDO.CARD.REQUEST<REDO.CARD.REQ.AUTO.REQUEST.FLAG>
            IF Y.REQUEST.TYPE EQ 'YES' THEN
                Y.REQ.TYPE = 'AUTOMATIC'
            END ELSE
                Y.REQ.TYPE = 'MANUAL'
            END
            Y.BRANCH.CODE = R.REC.REDO.CARD.REQUEST<REDO.CARD.REQ.AGENCY>
            CALL CACHE.READ(FN.COMPANY, Y.BRANCH.CODE, R.REC.COMPANY, Y.ERR.COMPANY)   ;*R22 Auto Conversion  - F.READ to CACHE.READ
            Y.BRANCH = R.REC.COMPANY<EB.COM.COMPANY.NAME>
            Y.REQ.DATE = R.REC.REDO.CARD.REQUEST<REDO.CARD.REQ.DATE>
            Y.REQ.DATE = Y.REQ.DATE[7,2]:'/':Y.REQ.DATE[5,2]:'/':Y.REQ.DATE[1,4]
*
            Y.REQUEST.STAT = R.REC.REDO.CARD.REQUEST<REDO.CARD.REQ.STATUS>
*
            Y.REQ.CNT = DCOUNT(Y.REQUEST.STAT,@VM)
            Y.CNTR = 1
            Y.TEM.CARD = ''
            LOOP
            WHILE Y.CNTR LE Y.REQ.CNT
                Y.REQ.STATUS = Y.REQUEST.STAT<1,Y.CNTR>
                Y.ID.EB.LOOKUP = 'CARD.STATUS*':Y.REQ.STATUS
                CALL F.READ(FN.EB.LOOKUP, Y.ID.EB.LOOKUP, R.EB.LOOKUP, F.EB.LOOKUP, Y.ERR.LOOKUP)
                Y.REQ.STATUS = R.EB.LOOKUP<EB.LU.DESCRIPTION>
                Y.CARD.TYPE = R.REC.REDO.CARD.REQUEST<REDO.CARD.REQ.CARD.TYPE, Y.CNTR>
                IF Y.PRODUCT.TYPE THEN
                    IF Y.CARD.TYPE NE Y.PRODUCT.TYPE THEN
                        Y.CNTR += 1
                        CONTINUE
                    END
                END
                Y.QTY.CARDS = R.REC.REDO.CARD.REQUEST<REDO.CARD.REQ.BRANCH.ORDERQTY, Y.CNTR>
                Y.TMP.QTY.CARDS = Y.QTY.CARDS
*
                GOSUB TEMP.CARD.CALC
*
                Y.COMMENT = R.REC.REDO.CARD.REQUEST<REDO.CARD.REQ.COMMENTS, Y.CNTR>
*
                GOSUB PROD.CODE
*
                GOSUB RETURN.ARRAY
                Y.CNTR += 1
*
            REPEAT
        END
    REPEAT
*
    CHANGE @FM TO @VM IN Y.RET.ARRAY
    Y.VAR = Y.RET.ARRAY
    Y.FIN.ARRAY = SORT(Y.VAR)
    CHANGE @VM TO @FM IN Y.TEMP.CARD
    CHANGE @VM TO @FM IN Y.TEMP.AMT.CARDS
*
    Y.CNT.CARD = DCOUNT(Y.TEMP.CARD,@FM)
    Y.I = 1
    LOOP
    WHILE Y.I LE Y.CNT.CARD
        Y.DUMMY.CARD = FIELD(Y.TEMP.CARD,@FM,Y.I)
        Y.DUMMY.AMT = FIELD(Y.TEMP.AMT.CARDS,@FM,Y.I)
        Y.FIN.DUMMY = Y.DUMMY.CARD:' ':Y.DUMMY.AMT
        Y.FIN.DISP<-1> = Y.DUMMY:'*':Y.DUMMY:'*':Y.FIN.DUMMY:'*':Y.DUMMY:'*':Y.DUMMY:'*':Y.DUMMY::'*':Y.DUMMY:'*':Y.DUMMY:'*':Y.DUMMY
        Y.I += 1
    REPEAT
    Y.TOT.DISP = 'Total by Plastic Type'
    Y.TOT.DISP.VAR = Y.DUMMY:'*':Y.DUMMY:'*':Y.TOT.DISP:'*':Y.DUMMY:'*':Y.DUMMY:'*':Y.DUMMY::'*':Y.DUMMY:'*':Y.DUMMY:'*':Y.DUMMY
*
    LOOP
        REMOVE Y.EACH.AMT FROM Y.TEMP.AMT.CARDS SETTING EACH.CARD.POS
    WHILE Y.EACH.AMT:EACH.CARD.POS
        Y.FINAL.TOTAL + = Y.EACH.AMT
    REPEAT

    Y.TOT.VAR = 'Total':' ':Y.FINAL.TOTAL
    Y.TOT.FIN.DISP = Y.DUMMY:'*':Y.DUMMY:'*':Y.TOT.VAR:'*':Y.DUMMY:'*':Y.DUMMY:'*':Y.DUMMY::'*':Y.DUMMY:'*':Y.DUMMY:'*':Y.DUMMY
    Y.BRK.VAR = '--------------------------------'
    Y.BRK = Y.DUMMY:'*':Y.DUMMY:'*':Y.BRK.VAR:'*':Y.DUMMY:'*':Y.DUMMY:'*':Y.DUMMY::'*':Y.DUMMY:'*':Y.DUMMY:'*':Y.DUMMY

    Y.FINAL.ARRAY = Y.FIN.ARRAY:@FM:Y.BRK:@FM:Y.TOT.DISP.VAR:@FM:Y.BRK:@FM:Y.FIN.DISP:@FM:Y.BRK:@FM:Y.TOT.FIN.DISP
*
RETURN
*
***************
TEMP.CARD.CALC:
***************
*
    LOCATE Y.CARD.TYPE IN Y.TEMP.CARD<1,1> SETTING CARD.POS THEN
        Y.CURR.AMT = Y.TEMP.AMT.CARDS<1,CARD.POS>
        Y.TMP.QTY.CARDS += Y.CURR.AMT
        Y.TEMP.AMT.CARDS<1,CARD.POS> = Y.TMP.QTY.CARDS
    END ELSE
        INS Y.CARD.TYPE BEFORE Y.TEMP.CARD<1,-1>
        INS Y.TMP.QTY.CARDS BEFORE Y.TEMP.AMT.CARDS<1,-1>
    END
RETURN
*
**********
PROD.CODE:
**********
*
    CALL F.READ(FN.CARD.TYPE, Y.CARD.TYPE, R.REC.CARD.TYPE, F.CARD.TYPE, Y.ERR.CARD.TYPE)
    IF R.REC.CARD.TYPE THEN
        Y.PROD.CODE = R.REC.CARD.TYPE<CARD.TYPE.LOCAL.REF, Y.PROD.COD.POS>
    END
RETURN
*
**************
RETURN.ARRAY:
**************
*
    Y.RET.ARRAY<-1> = Y.BRANCH:'*':Y.CARD.TYPE:'*':Y.REQ.CODE:'*':Y.REQ.TYPE:'*':Y.REQ.DATE:'*':Y.REQ.STATUS:'*':Y.QTY.CARDS:'*':Y.PROD.CODE:'*':Y.COMMENT
RETURN
*
