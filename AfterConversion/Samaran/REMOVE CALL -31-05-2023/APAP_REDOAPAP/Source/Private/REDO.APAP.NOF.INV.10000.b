* @ValidationCode : MjotMTQ0MDYwNjY0MDpDcDEyNTI6MTY4NDgzNjA0ODg5NzpJVFNTOi0xOi0xOjEyNDc6MTpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 23 May 2023 15:30:48
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 1247
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOAPAP
SUBROUTINE REDO.APAP.NOF.INV.10000 (Y.OUT.ARRAY)

*-----------------------------------------------------------------------------
*Company Name : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Developed By : Temenos Application Management
*Program Name : REDO.APAP.NOF.INV.10000
*-----------------------------------------------------------------------------
*Description : REDO.APAP.NOF.INV.10000 is the Nofile Enquiry routine
*This routine is used to build data for Enquiry Display from AZ.ACCOUNT
*Attached to : Standard Selection For the Enquiry REDO.APAP.NOF.INVEST.10000
*In Parameter : N/A
*Out Parameter: Y.OUT.ARRAY
*-----------------------------------------------------------------------------
* Modification History :
*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*17-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION  FM to @FM , VM to @VM
*17-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION NO CHANGE
*----------------------------------------------------------------------------------------

*-----------------------
* Date Who Reference Description
* ------ ------ ------------- -------------
* 22 DEC 2010 Satish@Contractor ODR-2010-03-0117 Initial Creation
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CATEGORY
    $INSERT I_F.CURRENCY
    $INSERT I_F.CUSTOMER
    $INSERT I_F.STMT.ENTRY
    $INSERT I_F.COMPANY
    $INSERT I_F.DEPT.ACCT.OFFICER
    $INSERT I_F.TELLER
MAIN.PARA:
*=========

    GOSUB OPEN.PARA
    GOSUB PROCESS.PARA

RETURN

OPEN.PARA:
*=========
    FN.AZ.ACCOUNT = "F.AZ.ACCOUNT" ; F.AZ.ACCOUNT = ''
    CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)

    FN.ACCOUNT = "F.ACCOUNT" ; F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.TELLER = "F.TELLER" ; F.TELLER = ''
    CALL OPF(FN.TELLER,F.TELLER)

    FN.CUSTOMER = 'F.CUSTOMER' ; F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    FN.COMPANY = "F.COMPANY" ; F.COMPANY = ''
    CALL OPF(FN.COMPANY,F.COMPANY)

    FN.DEPT.ACCT.OFFICER = "F.DEPT.ACCT.OFFICER" ; F.DEPT.ACCT.OFFICER = ''
    CALL OPF(FN.DEPT.ACCT.OFFICER,F.DEPT.ACCT.OFFICER)

    FN.CCY = "F.CURRENCY"
    F.CCY = ''
    CALL OPF(FN.CCY,F.CCY)

    FN.STMT.ENTRY='F.STMT.ENTRY' ; F.STMT.ENRTY=''
    CALL OPF(FN.STMT.ENTRY,F.STMT.ENTRY)

* Fix for PACS00336044 [Getting Client Type local ref value from Customer application #1]

    Y.APPL = 'CUSTOMER':@FM:'AZ.ACCOUNT'
    Y.FIELDS = 'L.CU.TIPO.CL':@FM:'ORIG.DEP.AMT'
    Y.POS = ''
    CALL MULTI.GET.LOC.REF(Y.APPL,Y.FIELDS,Y.POS)
    Y.CL.TYPE.POS = Y.POS<1,1>
    Y.AZ.ORG.DP.AMT.POS = Y.POS<2,1>

* End of Fix

RETURN

PROCESS.PARA:
*=============

    GOSUB GET.SELECTION.LIST
    IF NOT(ENQ.ERROR) THEN
        GOSUB CALC.LCCY.EXCESS.AMT
        GOSUB GET.LCCY.SELECTION
        GOSUB GET.FCCY.SELECTION
        GOSUB CONCAT.BOTH.SEL.IDS
        GOSUB GET.AZ.ACCOUNTS
    END

RETURN

GET.SELECTION.LIST:
*==================
    Y.DATE = '' ; Y.OUT.ARRAY = '' ; Y.SEL.DISP = '-'
    Y.FROM.DATE = '' ; Y.TO.DATE = '' ; R.DAO.REC = ''
    LOCATE 'DATE' IN D.FIELDS<1> SETTING Y.DATE.POS THEN
        Y.DATE = D.RANGE.AND.VALUE<Y.DATE.POS>
        GOSUB VALIDATE.DATE.SEL
    END
    Y.AGENCY = "" ; Y.ACCT.EXECUTIVE = '' ; R.COM.REC = ''
    LOCATE 'AGENCY' IN D.FIELDS<1> SETTING Y.AGENCY.POS THEN
        Y.AGENCY = D.RANGE.AND.VALUE<Y.AGENCY.POS>
        CALL CACHE.READ(FN.COMPANY,Y.AGENCY,R.COM.REC,Y.COM.ERR)
        IF NOT(R.COM.REC) THEN
            ENQ.ERROR = "EB-INVALID.COMP.CODE"
            RETURN
        END

        IF Y.SEL.DISP EQ '-' THEN
            Y.SEL.DISP = " AGENCIA : ":R.COM.REC<EB.COM.COMPANY.NAME>
        END ELSE
            Y.SEL.DISP:= " - AGENCIA : ":R.COM.REC<EB.COM.COMPANY.NAME>
        END
    END
    LOCATE 'ACCT.EXECUTIVE' IN D.FIELDS<1> SETTING Y.ACCT.EXE.POS THEN
        Y.ACCT.EXECUTIVE = D.RANGE.AND.VALUE<Y.ACCT.EXE.POS>
        CALL CACHE.READ(FN.DEPT.ACCT.OFFICER,Y.ACCT.EXECUTIVE,R.DAO.REC,Y.DOA.ERR)
        IF NOT(R.DAO.REC) THEN
            ENQ.ERROR = "EB-INVALID.DAO"
            RETURN
        END
        IF Y.SEL.DISP EQ '-' THEN
            Y.SEL.DISP = " OFICIAL DE CUENTA : ":R.DAO.REC<EB.DAO.NAME>
        END ELSE
            Y.SEL.DISP:= " - OFICIAL DE CUENTA : ":R.DAO.REC<EB.DAO.NAME>
        END
    END

RETURN

VALIDATE.DATE.SEL:
*=================
    Y.FROM.DATE = FIELD(Y.DATE,@SM,1)
    Y.TO.DATE = FIELD(Y.DATE,@SM,2)
    IF NOT(NUM(Y.FROM.DATE)) OR NOT(NUM(Y.TO.DATE)) THEN
        ENQ.ERROR = "EB-DATE.NOT.VALID"
        RETURN
    END
    IF LEN(Y.FROM.DATE) NE 8 OR LEN(Y.TO.DATE) NE 8 THEN
        ENQ.ERROR = "EB-DATE.NOT.VALID"
        RETURN
    END
    IF Y.FROM.DATE AND NOT(Y.TO.DATE) THEN
        ENQ.ERROR = "EB-TO.DATE.MAND"
        RETURN
    END
    IF NOT(Y.FROM.DATE) AND Y.TO.DATE THEN
        ENQ.ERROR = "EB-FROM.DATE.MAND"
        RETURN
    END
    GOSUB CHECK.DATE.RANGE

RETURN

CHECK.DATE.RANGE:
*================
    IF Y.FROM.DATE GT Y.TO.DATE THEN
        ENQ.ERROR = "EB-TO.DATE.SHOULD.GT.FROM.DATE"
        RETURN
    END
    Y.FROM.DATE1 = ICONV(Y.FROM.DATE,"D2")
    Y.FROM.DATE1 = OCONV(Y.FROM.DATE1,"D2")
    Y.TO.DATE1 = ICONV(Y.TO.DATE,"D2")
    Y.TO.DATE1 = OCONV(Y.TO.DATE1,"D2")

    IF Y.SEL.DISP EQ '-' THEN
        Y.SEL.DISP = " FECHA : ":Y.FROM.DATE1:" ":Y.TO.DATE1
    END ELSE
        Y.SEL.DISP := " - FECHA : ":Y.FROM.DATE1:" ":Y.TO.DATE1
    END

RETURN

CALC.LCCY.EXCESS.AMT:
*====================
    Y.LR.CCY = '' ; Y.BRK.SORT = ''
    CALL GET.LOC.REF("CURRENCY","L.CU.AMLBUY.RT",Y.LR.CCY)
    L.CU.AMLBUY.RT = Y.LR.CCY

    CALL CACHE.READ(FN.CCY,'USD',R.CCY.REC,Y.CCY.ERR)
    EXCH.RATE = R.CCY.REC<EB.CUR.LOCAL.REF,L.CU.AMLBUY.RT>
    DOP.EXCESS.AMT = 10000 * EXCH.RATE

RETURN

GET.LCCY.SELECTION:
*==================
    SEL.AZ.COM = ''

    SEL.AZ.LCCY = "SELECT ":FN.AZ.ACCOUNT
    SEL.AZ.LCCY := " WITH CURRENCY EQ ":LCCY
    SEL.AZ.LCCY := " AND ORIG.DEP.AMT GE ":DOP.EXCESS.AMT

    IF Y.DATE THEN
        SEL.AZ.COM := " AND CREATE.DATE GE ":Y.FROM.DATE
        SEL.AZ.COM := " AND CREATE.DATE LE ":Y.TO.DATE
    END
    IF Y.AGENCY THEN
        SEL.AZ.COM := " AND CO.CODE EQ ":Y.AGENCY
    END
    SEL.AZ.SORT = " BY CO.CODE BY CATEGORY BY CURRENCY BY CREATE.DATE"
    SEL.AZ.LCCY := SEL.AZ.COM:SEL.AZ.SORT

    CALL EB.READLIST(SEL.AZ.LCCY,SEL.LIST.IDS,'',NO.OF.REC,SEL.ERR.CCY)

RETURN

GET.FCCY.SELECTION:
*==================

    SEL.AZ.FCCY = "SELECT ":FN.AZ.ACCOUNT
    SEL.AZ.FCCY := " WITH CURRENCY EQ USD "
    SEL.AZ.FCCY := " AND ORIG.DEP.AMT GE 10000"

    SEL.AZ.FCCY:= SEL.AZ.COM:SEL.AZ.SORT
    CALL EB.READLIST(SEL.AZ.FCCY,SEL.LIST.FCCY,'',NO.OF.FREC,SEL.ERR.FCCY)
* IF NO.OF.FREC EQ '' OR NO.OF.FREC EQ 0 THEN
* GOSUB GOEND
* END
RETURN

CONCAT.BOTH.SEL.IDS:
*===================
    SEL.LIST.IDS<-1> = SEL.LIST.FCCY
    NO.OF.REC+= NO.OF.FREC
RETURN

GET.AZ.ACCOUNTS:
*===============
    FOR AZ = 1 TO NO.OF.REC
        AZ.ID = SEL.LIST.IDS<AZ>
        GOSUB READ.ACCT.RECORDS
    NEXT AZ

* Y.OUT.ARRAY<1>:= Y.SEL.DISP:"*"

RETURN

GET.CASH.AMT:
*============

    Y.ID.LIST = '' ; Y.CASH.AMT = '' ; Y.CASH.ACCT = '10001'

    ENQ.SELECTION<2,1> = 'ACCOUNT' ; ENQ.SELECTION<2,2> = 'BOOKING.DATE'
    ENQ.SELECTION<3,1> = '1' ; ENQ.SELECTION<3,2> = '1'
    ENQ.SELECTION<4,1> = AZ.ID ; ENQ.SELECTION<4,2> = Y.AZ.DATE
    D.FIELDS = 'ACCOUNT':@FM:'BOOKING.DATE'
    D.RANGE.AND.VALUE = AZ.ID:@FM:Y.AZ.DATE
    D.LOGICAL.OPERANDS = '1':@FM:'1'
    CALL E.STMT.ENQ.BY.CONCAT(Y.ID.LIST)

    LOOP
        REMOVE Y.RES.LIST FROM Y.ID.LIST SETTING Y.RES.POS
    WHILE Y.RES.LIST:Y.RES.POS
        Y.SE.ID = FIELD(Y.RES.LIST,'*',2,1)
        GOSUB READ.STMT.ENTRY
    REPEAT

RETURN

READ.STMT.ENTRY:
*==============
    CALL CACHE.READ(FN.STMT.ENTRY,Y.SE.ID,R.STMT.ENTRY,STMT.ENTRY.ERR)
    IF R.STMT.ENTRY<AC.STE.SYSTEM.ID> EQ 'TT' THEN
        IF R.STMT.ENTRY<AC.STE.ACCOUNT.NUMBER>[4,5] EQ Y.CASH.ACCT THEN
            IF R.STMT.ENTRY<AC.STE.ACCOUNT.NUMBER>[1,3] EQ LCCY THEN
                Y.CASH.AMT = R.STMT.ENTRY<AC.STE.AMOUNT.LCY>
            END ELSE
                Y.CASH.AMT = R.STMT.ENTRY<AC.STE.AMOUNT.FCY>
            END
        END
    END

RETURN

READ.ACCT.RECORDS:
*=================

* Fix for PACS00336044 [Removed Cache read and using array instead #2]

    Y.ACCOUNT = AZ.ID

    LOCATE Y.ACCOUNT IN Y.ACCOUNT.ID SETTING ACCT.POS THEN
        Y.ACCT.JH = FIELD(Y.ACCOUNT.ARRAY<ACCT.POS>,'#',1)
        Y.ACCT.CU = FIELD(Y.ACCOUNT.ARRAY<ACCT.POS>,'#',2)
        Y.ACCT.OF = FIELD(Y.ACCOUNT.ARRAY<ACCT.POS>,'#',3)
    END ELSE
        CALL F.READ(FN.ACCOUNT,Y.ACCOUNT,R.ACCOUNT,F.ACCOUNT,ACCOUNT.ERR)
        Y.ACCT.JH = CHANGE(R.ACCOUNT<AC.JOINT.HOLDER>,@VM,'*')
        Y.ACCT.CU = R.ACCOUNT<AC.CUSTOMER>
        Y.ACCT.OF = R.ACCOUNT<AC.ACCOUNT.OFFICER>
        Y.ACCOUNT.ID<-1> = Y.ACCOUNT
        Y.ACCOUNT.ARRAY<-1> = Y.ACCT.JH:'#'Y.ACCT.CU:'#':Y.ACCT.OF
    END

* End of Fix

    CALL F.READ(FN.AZ.ACCOUNT,AZ.ID,R.AZ.REC,F.AZ.ACCOUNT,LERR.AZ)
    Y.TEMP = Y.ACCT.OF
    IF NOT(Y.ACCT.EXECUTIVE) THEN
        GOSUB BUILD.FIELD.DATA.LIST
    END ELSE
        IF Y.ACCT.EXECUTIVE AND Y.ACCT.EXECUTIVE EQ Y.ACCT.OF THEN
            GOSUB BUILD.FIELD.DATA.LIST
        END
    END
RETURN

BUILD.FIELD.DATA.LIST:
*=====================

    Y.AZ.DATE = R.AZ.REC<AZ.CREATE.DATE>
    Y.AGENCY = R.AZ.REC<AZ.CO.CODE>
    Y.ACCOUNT.EXECUTIVE = Y.ACCT.OF
    Y.INVESTMENT.TYPE = R.AZ.REC<AZ.CATEGORY>
    Y.CURRENCY = R.AZ.REC<AZ.CURRENCY>
    Y.INVESTMENT.NUMBER = AZ.ID
    Y.CUSTOMER.NAME = R.AZ.REC<AZ.CUSTOMER>
    Y.CLIENT.CODE = R.AZ.REC<AZ.CUSTOMER>

    GOSUB GET.BENEFICIARY.DETAILS

* Y.INVESTMENT.AMOUNT = R.AZ.REC<AZ.ORIG.PRINCIPAL>
    Y.CURRENCY = R.AZ.REC<AZ.CURRENCY>


    Y.INVST.AMT = R.AZ.REC<AZ.LOCAL.REF,Y.AZ.ORG.DP.AMT.POS>

    IF Y.CURRENCY EQ "DOP" AND Y.INVST.AMT GE DOP.EXCESS.AMT THEN
        Y.INVESTMENT.AMOUNT = Y.INVST.AMT
    END

    IF Y.CURRENCY EQ "USD" AND Y.INVST.AMT GE "10000" THEN
        Y.INVESTMENT.AMOUNT = Y.INVST.AMT
    END

* GOSUB GET.CASH.AMT
    SEL.TELL.CMD = "SELECT ":FN.TELLER: " WITH ACCOUNT.2 EQ ":AZ.ID
    CALL EB.READLIST(SEL.TELL.CMD,SEL.TELL.LIST,'',NOF,F.ERR)
    IF SEL.TELL.LIST THEN
        Y.TELL.ID = SEL.TELL.LIST<1>
        CALL F.READ(FN.TELLER,Y.TELL.ID,R.TELL.REC,F.TELLER,ERR)
        Y.CASH.AMT = R.TELL.REC<TT.TE.AMOUNT.LOCAL.1>
    END

    Y.CASH.AMOUNT = Y.CASH.AMT
    Y.USER.AUTHORIZES = FIELD(R.AZ.REC<AZ.AUTHORISER,1>,'_',2)
    Y.USER.INPUTS = FIELD(R.AZ.REC<AZ.INPUTTER,1>,'_',2)
    GOSUB OUTPUT.DATA

RETURN

* Fix for PACS00336044 [Including Beneficiary details seperated with ';' #3]

GET.BENEFICIARY.DETAILS:
*----------------------*

    Y.ACCT.JH = CHANGE(Y.ACCT.JH,'*',@VM)
    Y.ACCT.JH = INSERT(Y.ACCT.JH, 1,1;Y.ACCT.CU)
    Y.ACCT.JH.LIST = Y.ACCT.JH

    LOOP
        REMOVE Y.CUS.ID FROM Y.ACCT.JH.LIST SETTING Y.HOLDER.POS
    WHILE Y.CUS.ID:Y.HOLDER.POS
        R.CUSTOMER = ''

        LOCATE Y.CUS.ID IN Y.CUSTOMER.ID SETTING CUST.POS THEN
            Y.CUS.SHORT.NAME = FIELD(Y.CUSTOMER.ARRAY,'#',1)
            Y.CUS.NAME.1 = FIELD(Y.CUSTOMER.ARRAY,'#',2)
            Y.CUS.NAME.2 = FIELD(Y.CUSTOMER.ARRAY,'#',3)
            Y.CUS.G.NAME = FIELD(Y.CUSTOMER.ARRAY,'#',4)
            Y.CUS.F.NAME = FIELD(Y.CUSTOMER.ARRAY,'#',5)
            Y.CUS.CL.TYPE = FIELD(Y.CUSTOMER.ARRAY,'#',6)
        END ELSE
            CALL F.READ(FN.CUSTOMER, Y.CUS.ID, R.CUSTOMER, F.CUSTOMER, Y.READ.ERR)
            Y.CUS.SHORT.NAME = R.CUSTOMER<EB.CUS.SHORT.NAME,1>
            Y.CUS.NAME.1 = R.CUSTOMER<EB.CUS.NAME.1,1>
            Y.CUS.NAME.2 = R.CUSTOMER<EB.CUS.NAME.2,1>
            Y.CUS.G.NAME = R.CUSTOMER<EB.CUS.GIVEN.NAMES>
            Y.CUS.F.NAME = R.CUSTOMER<EB.CUS.FAMILY.NAME>
            Y.CUS.CL.TYPE = R.CUSTOMER<EB.CUS.LOCAL.REF,Y.CL.TYPE.POS>
            Y.CUSTOMER.ID<-1> = Y.CUS.ID
            Y.CUSTOMER.ARRAY<-1> = Y.CUS.SHORT.NAME:'#':Y.CUS.NAME.1:'#':Y.CUS.NAME.2:'#':Y.CUS.G.NAME:'#':Y.CUS.F.NAME:'#':Y.CUS.CL.TYPE
        END

        IF Y.CUS.CL.TYPE EQ "PERSONA FISICA" OR Y.CUS.CL.TYPE EQ "CLIENTE MENOR" THEN
            Y.CUS.NAMES = Y.CUS.G.NAME:' ':Y.CUS.F.NAME
        END
        IF Y.CUS.CL.TYPE EQ "PERSONA JURIDICA" THEN
            Y.CUS.NAMES = Y.CUS.NAME.1:' ':Y.CUS.NAME.2
        END
        IF NOT(Y.CUS.CL.TYPE) THEN
            Y.CUS.NAMES = Y.CUS.SHORT.NAME
        END

        Y.BEN.NAME<1,-1> = Y.CUS.NAMES

    REPEAT

RETURN

* End of Fix

OUTPUT.DATA:
*===========

    K.BCSORT1 = Y.AGENCY:Y.CURRENCY
    K.BCSORT1:= Y.INVESTMENT.TYPE:Y.ACCOUNT.EXECUTIVE:Y.AZ.DATE

    IF Y.FROM.DATE1 EQ '' AND Y.TO.DATE1 EQ '' THEN
    END

    IF Y.FROM.DATE1 EQ '' AND Y.TO.DATE1 NE '' THEN
        Y.FLAG11 =1
    END

    IF Y.FROM.DATE1 NE '' AND Y.TO.DATE1 EQ '' THEN
        Y.FLAG11 = 1
    END

    IF Y.FLAG11 NE '1' THEN

* Fix for PACS00336044 [Including Beneficiary details seperated with ';' #4]

        Y.BENEFICIARY = CHANGE(Y.BEN.NAME, @VM, '; ')
        Y.CLIENT.CODE = CHANGE(Y.ACCT.JH, @VM, '; ')

* End of Fix

        Y.OUT.ARRAY1 = Y.AZ.DATE:'*':Y.AGENCY:'*':Y.ACCOUNT.EXECUTIVE:'*'
        Y.OUT.ARRAY1:= Y.INVESTMENT.TYPE:'*':Y.CURRENCY:'*':Y.INVESTMENT.NUMBER:'*'
        Y.OUT.ARRAY1:= Y.CLIENT.CODE:'*':Y.INVESTMENT.AMOUNT:'*'
* Y.OUT.ARRAY1:= Y.CUSTOMER.NAME:'*':Y.CLIENT.CODE:'*':Y.INVESTMENT.AMOUNT:'*'
        Y.OUT.ARRAY1:= Y.CASH.AMOUNT:'*':Y.USER.AUTHORIZES:"*":Y.USER.INPUTS:'*':Y.BENEFICIARY

        LOCATE K.BCSORT1 IN Y.BRK.SORT<1> BY 'AR' SETTING Y.BK.POS ELSE NULL
        INS K.BCSORT1 BEFORE Y.BRK.SORT<Y.BK.POS>
        INS Y.OUT.ARRAY1 BEFORE Y.OUT.ARRAY<Y.BK.POS>

        Y.BEN.NAME = '' ;* PACS00336044
        Y.INVESTMENT.AMOUNT = ''
        Y.CASH.AMT = ''
    END ELSE
        GOSUB GOEND
    END
RETURN
*-----------
GOEND:
END
