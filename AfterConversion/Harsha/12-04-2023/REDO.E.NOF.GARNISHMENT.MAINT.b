$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.E.NOF.GARNISHMENT.MAINT(FINAL.ARRAY)
*-------------------------------------------------------------------------------
* This is a no file enquiry will fetch the details from various table
* based on the given selection criteria
*-------------------------------------------------------------------------------
* Company Name  : ASOCIACION POPUL
* Developed By  : BHARATH C
* Program Name  : REDO.V.INP.GARNISHMENT.MAINT
* ODR NUMBER    : ODR-2009-10-0531
*LINKED WITH    : REDO.E.MAINT.GARNISHMENT ENQUIRY
*----------------------------------------------------------------------
*Input param = none
*output param =none
*---------------------------------------------------------------------
*MODIFICATION:
*   DATE           ODR
*7.1.2010     ODR-2009-10-0531
*16-02-2011        Prabhu.N         B.88-HD1040884      id selection based on current variable
* 12-APRIL-2023      Harsha                R22 Auto Conversion  - Added IF E EQ "EB-UNKNOWN.VARIABLE" THEN , FM to @FM and SM to @SM
* 12-APRIL-2023      Harsha                R22 Manual Conversion - No changes 
*-------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER.ACCOUNT
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.ACCOUNT
    $INSERT I_F.APAP.H.GARNISH.DETAILS
    $INSERT I_System

    GOSUB INIT
    GOSUB OPENFILE
    GOSUB PROCESS
    GOSUB OPERAND.LIKE.CHECK
    GOSUB OPERAND.RANGE.CHECK
    GOSUB ENQ.ONLINE.BALANCE.CHECK
RETURN

*---------
INIT:

    SEL.COND=''
    D.LIST=''
    SEARCH.VALUE=''
    POS=''
    OP.POS=''
    OPERAND=''
    SEL.FIELD=''

    SEL.CMD=''
    SEL.LIST=''
    NO.REC=''
    REC.CODE=''
    G.ID=''
    SEL.POS=''

    ACCOUNT.LIST=''
    J.ACCOUNT=''
    GARNISHMENT.AMOUNT=''
    ID=''
    CUSTOMER.ID=''
    ONLINE.ACTUAL.BAL.LIST=''
    ONLINE.ACTUAL.BAL=''
    NO.OF.ACCOUNT=''
    D.COUNT=''
    ACCOUNT=''
    POS.1=''
    ENQ.ACCOUNT.NO=''
    ENQ.ACCOUNT.OP=''
    ENQ.ACCOUNT.OP.POS=''
    ENQ.ONLINE.BALANCE=''
    ENQ.ONLINE.BALANCE.FROM=''
    ENQ.ONLINE.BALANCE.TO=''
    COUNT.ONLINE.BALANCE=''
    ENQ.OP=''
    ENQ.OP.POS=''

RETURN
*----------------------------------------------------------------------------
OPENFILE:
    FN.CUSTOMER.ACCOUNT='F.CUSTOMER.ACCOUNT'
    F.CUSTOMER.ACCOUNT=''
    R.CUSTOMER.ACCOUNT=''
    ERR.CUSTOMER.ACCOUNT=''
    CALL OPF(FN.CUSTOMER.ACCOUNT,F.CUSTOMER.ACCOUNT)

    FN.ACCOUNT='F.ACCOUNT'
    F.ACCOUNT=''
    R.ACCOUNT=''
    ERR.ACCOUNT=''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.GARNISH.DETAILS='F.APAP.H.GARNISH.DETAILS'
    F.GARNISH.DETAILS=''
    R.GARNISH=''
    ERR.GARNISH=''
    CALL OPF(FN.GARNISH.DETAILS,F.GARNISH.DETAILS)

    FN.JOINT.CONTRACTS.XREF='F.JOINT.CONTRACTS.XREF'
    F.JOINT.CONTRACTS.XREF=''
    CALL OPF(FN.JOINT.CONTRACTS.XREF,F.JOINT.CONTRACTS.XREF)
RETURN
*-------
PROCESS:
***************CUSTOMER

    LOCATE 'CUSTOMER' IN D.FIELDS<1> SETTING POS THEN
        SEARCH.VALUE=D.RANGE.AND.VALUE<POS>
        OP.POS=D.LOGICAL.OPERANDS<POS>
        OPERAND =OPERAND.LIST<OP.POS>
        SEL.FIELD=D.FIELDS<POS>
        BEGIN CASE
            CASE OPERAND EQ 'RG' OR OPERAND EQ 'NR'
                SEL.COND=' WITH ('
                GOSUB  OPERAND.RANGE.CHECK
            CASE OPERAND EQ 'UL' OR OPERAND EQ 'LK'
                GOSUB OPERAND.LIKE.CHECK
                SEL.COND=' WITH ':SEL.FIELD:' ':OPERAND:' ':SEARCH.VALUE:' '
            CASE 1
                SEL.COND=' WITH ':SEL.FIELD:' ':OPERAND:' ':SEARCH.VALUE:' '
        END CASE
    END

******************GARNISH

*HD1040884-Code modification start-------------
    SEARCH.VALUE=System.getVariable('CURRENT.TXN.ID')
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN		;*R22 Auto Conversion  - Added IF E EQ "EB-UNKNOWN.VARIABLE" THEN
        SEARCH.VALUE = ""
    END

    OP.POS=D.LOGICAL.OPERANDS<POS>
    OPERAND='EQ'
    SEL.FIELD='GARNISHMENT.REF'
*HD1040884-Code modification end---------------
    BEGIN CASE
        CASE OPERAND EQ 'RG' OR OPERAND EQ 'NR'
            SEL.COND:=' AND ('
            GOSUB  OPERAND.RANGE.CHECK
        CASE OPERAND EQ 'UL' OR OPERAND EQ 'LK'
            GOSUB OPERAND.LIKE.CHECK
            SEL.COND:=' AND (':SEL.FIELD:'  ':OPERAND:' ':SEARCH.VALUE:') '
        CASE 1
            SEL.COND:=' AND (':SEL.FIELD:'  ':OPERAND:' ':SEARCH.VALUE:') '
    END CASE
******************AVAILABLE BALANCE
    LOCATE 'GARNISHMENT.AMT' IN D.FIELDS<1> SETTING POS THEN
        SEARCH.VALUE=D.RANGE.AND.VALUE<POS>
        OP.POS=D.LOGICAL.OPERANDS<POS>
        OPERAND =OPERAND.LIST<OP.POS>
        SEL.FIELD=D.FIELDS<POS>
        BEGIN CASE
            CASE  OPERAND EQ 'RG' OR OPERAND EQ 'NR'
                SEL.COND:=' AND ('
                GOSUB  OPERAND.RANGE.CHECK
            CASE OPERAND EQ 'UL' OR OPERAND EQ 'LK'
                GOSUB OPERAND.LIKE.CHECK
                SEL.COND:=' AND (':SEL.FIELD:'  ':OPERAND:' ':SEARCH.VALUE:') '
            CASE 1
                SEL.COND:=' AND (':SEL.FIELD:'  ':OPERAND:' ':SEARCH.VALUE:') '
        END CASE
    END
*******************************ACCOUNT GIVEN

    LOCATE 'ACCOUNT.NO' IN D.FIELDS<1> SETTING POS THEN
        ENQ.ACCOUNT.NO=D.RANGE.AND.VALUE<POS>
        ENQ.ACCOUNT.OP.POS=D.LOGICAL.OPERANDS<POS>
        ENQ.ACCOUNT.OP=OPERAND.LIST<ENQ.ACCOUNT.OP.POS>
    END
**********************************ONLINE BALANCE
    LOCATE 'AVAIL.BALANCE' IN D.FIELDS<1>  SETTING POS THEN
        ENQ.ONLINE.BALANCE=D.RANGE.AND.VALUE<POS>
        COUNT.ONLINE.BALANCE=DCOUNT(ENQ.ONLINE.BALANCE,@SM)
        ENQ.OP.POS=D.LOGICAL.OPERANDS<POS>
        ENQ.OP=OPERAND.LIST<ENQ.OP.POS>

        BEGIN CASE
            CASE ENQ.OP EQ 'RG' OR ENQ.OP EQ 'NR'
                ENQ.ONLINE.BALANCE.FROM=FIELD(ENQ.ONLINE.BALANCE,@SM,1)
                ENQ.ONLINE.BALANCE.TO=FIELD(ENQ.ONLINE.BALANCE,@SM,2)
            CASE 1
                ENQ.ONLINE.BALANCE=D.RANGE.AND.VALUE<POS>
        END CASE

    END


********************* WRITE IN FILNAL ARRAY
    SEL.CMD='SELECT ':FN.GARNISH.DETAILS:SEL.COND
    SEL.COND=''

    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.REC,REC.CODE)
    LOOP
        REMOVE G.ID FROM SEL.LIST SETTING SEL.POS
    WHILE G.ID:SEL.POS
        CALL F.READ(FN.GARNISH.DETAILS,G.ID,R.GARNISH,F.GARNISH.DETAILS,ERR.GARNISH)
        GARNISHMENT.AMOUNT=R.GARNISH<APAP.GAR.GARNISHMENT.AMT>
        CUSTOMER.ID=R.GARNISH<APAP.GAR.CUSTOMER>
        CALL F.READ(FN.CUSTOMER.ACCOUNT,CUSTOMER.ID,R.CUSTOMER.ACCOUNT,F.CUSTOMER.ACCOUNT,ERR.CUSTOMER.ACCOUNT)
        ACCOUNT.LIST=R.CUSTOMER.ACCOUNT
        CALL F.READ(FN.JOINT.CONTRACTS.XREF,CUSTOMER.ID,R.JOINT,F.JOINT.CONTRACTS.XREF,ERR.JOINT)


*************ADDING THE JOINT ACCOUNTS

        BEGIN CASE
            CASE R.JOINT NE '' AND ACCOUNT.LIST NE ''
                ACCOUNT.LIST=ACCOUNT.LIST:@FM:R.JOINT
            CASE R.JOINT NE '' AND ACCOUNT.LIST EQ ''
                ACCOUNT.LIST=R.JOINT
        END CASE
        NO.OF.ACCOUNT=DCOUNT(ACCOUNT.LIST,@FM)

        IF ENQ.ACCOUNT.NO NE '' THEN
            GOSUB ACCOUNT.CHECK
        END
        IF ACCOUNT.LIST NE '' THEN
            FOR D.COUNT=1 TO NO.OF.ACCOUNT
                ACCOUNT=FIELD(ACCOUNT.LIST,@FM,D.COUNT)

                IF ENQ.ACCOUNT.OP EQ 'LK' THEN
                    GOSUB ACCOUNT.CHECK
                END
                CALL F.READ(FN.ACCOUNT,ACCOUNT,R.ACCOUNT,F.ACCOUNT,ERR.ACCOUNT)
                Y.BAL.POS=''
                CALL MULTI.GET.LOC.REF('ACCOUNT','L.AC.AV.BAL',Y.BAL.POS)
                ONLINE.ACTUAL.BAL=R.ACCOUNT<AC.LOCAL.REF><1,Y.BAL.POS>

*********************************************************************************************************CHECKING FOR ONLINE BALANC
                IF ENQ.ONLINE.BALANCE EQ '' AND ACCOUNT NE '' THEN
                    IF ONLINE.ACTUAL.BAL GT 0 THEN
                        FINAL.ARRAY<-1>=ACCOUNT:'*':CUSTOMER.ID:'*':ONLINE.ACTUAL.BAL:'*':G.ID:'*':GARNISHMENT.AMOUNT
                        CUSTOMER.ID=''
                        GARNISHMENT.AMOUNT=''
                    END
                END ELSE
                    GOSUB ENQ.ONLINE.BALANCE.CHECK
                END

            NEXT
        END

        ONLINE.ACTUAL.BAL=''
    REPEAT

RETURN
*-------
OPERAND.LIKE.CHECK:

    BEGIN CASE
        CASE OPERAND EQ 'UL'
            OPERAND = 'UNLIKE'
        CASE  OPERAND EQ 'LK'
            OPERAND = 'LIKE'
    END CASE
RETURN
*-------
OPERAND.RANGE.CHECK:

    FROM.VAL=''
    T0.VAL=''
    FROM.VAL=FIELD(SEARCH.VALUE,@SM,1)
    TO.VAL=FIELD(SEARCH.VALUE,@SM,2)

    IF OPERAND EQ 'RG' THEN
        SEL.COND:=SEL.FIELD:' GE ':FROM.VAL:' AND ':SEL.FIELD:' LE ':TO.VAL:') '
        RETURN
    END
    IF OPERAND EQ 'NR' THEN

        SEL.COND:=SEL.FIELD:' LE ':FROM.VAL:' OR ':SEL.FIELD:' GE ':TO.VAL:') '

        RETURN
    END

RETURN
*-------
ENQ.ONLINE.BALANCE.CHECK:

    BEGIN CASE
        CASE ENQ.OP EQ 'RG'
            IF ONLINE.ACTUAL.BAL GE  ENQ.ONLINE.BALANCE.FROM AND ONLINE.ACTUAL.BAL LT ENQ.ONLINE.BALANCE.TO THEN
                FINAL.ARRAY<-1>=ACCOUNT:'*':CUSTOMER.ID:'*':ONLINE.ACTUAL.BAL:'*':G.ID:'*':GARNISHMENT.AMOUNT
            END
        CASE ENQ.OP EQ 'NR'
            IF ENQ.ONLINE.BALANCE.FROM LE ONLINE.ACTUAL.BAL OR ENQ.ONLINE.BALANCE.TO GE ONLINE.ACTUAL.BAL  THEN
                FINAL.ARRAY<-1>=ACCOUNT:'*':CUSTOMER.ID:'*':ONLINE.ACTUAL.BAL:'*':G.ID:'*':GARNISHMENT.AMOUNT
            END
        CASE ENQ.OP EQ 'EQ'
            IF ENQ.ONLINE.BALANCE EQ ONLINE.ACTUAL.BAL THEN
                FINAL.ARRAY<-1>=ACCOUNT:'*':CUSTOMER.ID:'*':ONLINE.ACTUAL.BAL:'*':G.ID:'*':GARNISHMENT.AMOUNT
            END
        CASE ENQ.OP EQ 'GT'
            IF ONLINE.ACTUAL.BAL GT  ENQ.ONLINE.BALANCE THEN
                FINAL.ARRAY<-1>=ACCOUNT:'*':CUSTOMER.ID:'*':ONLINE.ACTUAL.BAL:'*':G.ID:'*':GARNISHMENT.AMOUNT
            END
        CASE ENQ.OP EQ 'LT'
            IF ONLINE.ACTUAL.BAL LT  ENQ.ONLINE.BALANCE THEN
                FINAL.ARRAY<-1>=ACCOUNT:'*':CUSTOMER.ID:'*':ONLINE.ACTUAL.BAL:'*':G.ID:'*':GARNISHMENT.AMOUNT
            END
        CASE ENQ.OP EQ 'GE'
            IF ONLINE.ACTUAL.BAL GE  ENQ.ONLINE.BALANCE THEN
                FINAL.ARRAY<-1>=ACCOUNT:'*':CUSTOMER.ID:'*':ONLINE.ACTUAL.BAL:'*':G.ID:'*':GARNISHMENT.AMOUNT
            END
        CASE ENQ.OP EQ 'LE'
            IF ONLINE.ACTUAL.BAL LE  ENQ.ONLINE.BALANCE THEN
                FINAL.ARRAY<-1>=ACCOUNT:'*':CUSTOMER.ID:'*':ONLINE.ACTUAL.BAL:'*':G.ID:'*':GARNISHMENT.AMOUNT
            END
    END CASE
RETURN
*--------------------
ACCOUNT.CHECK:
    BEGIN CASE
        CASE ENQ.ACCOUNT.OP EQ 'EQ'
            LOCATE ENQ.ACCOUNT.NO IN ACCOUNT.LIST SETTING POS.1 THEN
                ACCOUNT.LIST = ENQ.ACCOUNT.NO
            END ELSE
                ACCOUNT.LIST = ''
            END
        CASE ENQ.ACCOUNT.OP EQ 'LK'

            IF(INDEX(ACCOUNT,ENQ.ACCOUNT.NO,1)) THEN
                ACCOUNT=ACCOUNT
            END ELSE
                ACCOUNT=''
            END

    END CASE

RETURN
*------------------
END
