* @ValidationCode : MjotMTQxMjMxMjU0OkNwMTI1MjoxNjgxMzc5ODczODY2OkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 13 Apr 2023 15:27:53
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
SUBROUTINE REDO.NOFILE.AI.DROP.DOWN.ENQ(FIN.ARR)
*-----------------------------------------------------------------------------
* Company Name : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By :
* Program Name : REDO.NOFILE.AI.DROP.DOWN.ENQ
*-----------------------------------------------------------------------------
* Description : This subroutine is attached as a BUILD routine in the Enquiry REDO.SAV.ACCOUNT.LIST
* Linked with : Enquiry REDO.SAV.ACCOUNT.LIST  as BUILD routine
* In Parameter : ENQ.DATA
* Out Parameter : None
*
**DATE           ODR                   DEVELOPER               VERSION
*
*01/11/11       PACS00146410            PRABHU N                MODIFICAION
*05/06/15       PACS00459918            ASLAM                   MODIFICATION
*14/06/15       PACS00459597            ASLAM                   MODIFICATION
*22/06/15       PACS00465952            ASLAM                   MODIFICATION
*
* 13-APR-2023     Conversion tool   R22 Auto conversion     FM TO @FM, VM to @VM, SM to @SM, ++ to +=
* 13-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_System
    $INSERT I_EB.EXTERNAL.COMMON
    $INSERT I_F.CUSTOMER.ACCOUNT
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CATEGORY
    $INSERT I_F.RELATION.CUSTOMER
    $INSERT I_F.AI.REDO.ARCIB.PARAMETER
    $INSERT I_F.AI.REDO.ARCIB.ALIAS.TABLE
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.AI.REDO.ACCT.RESTRICT.PARAMETER
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.RELATION
    GOSUB INITIALISE
    GOSUB FORM.ACCT.ARRAY

RETURN
*-----------------------------------------------------------------------------
INITIALISE:
*-----------------------------------------------------------------------------


    F.CUSTOMER.ACCOUNT = ''
    FN.CUSTOMER.ACCOUNT = 'F.CUSTOMER.ACCOUNT'

    CALL OPF(FN.CUSTOMER.ACCOUNT,F.CUSTOMER.ACCOUNT)
    FN.ACC = 'F.ACCOUNT'
    F.ACC = ''
    CALL OPF(FN.ACC,F.ACC)

    FN.RELATION.CUSTOMER = 'F.RELATION.CUSTOMER'
    F.RELATION.CUSTOMER  = ''
    CALL OPF(FN.RELATION.CUSTOMER,F.RELATION.CUSTOMER)

*------------PACS00459918----------------------------
    FN.REL = 'F.RELATION'
    F.REL = ''
    CALL OPF(FN.REL,F.REL)
*------------PACS00459918------------------------------
    FN.AZ = 'F.AZ.ACCOUNT'
    F.AZ = ''
    CALL OPF(FN.AZ,F.AZ)

    Y.VAR.EXT.CUSTOMER = ''
    Y.VAR.EXT.ACCOUNTS=''
    FN.CATEGORY = "F.CATEGORY"
    F.CATEGORY = ''
    FN.AI.REDO.ARC.PARAM='F.AI.REDO.ARCIB.PARAMETER'
    FN.AI.REDO.ACCT.RESTRICT.PARAMETER = 'F.AI.REDO.ACCT.RESTRICT.PARAMETER'

    FN.JOINT.CONTRACTS.XREF = 'F.JOINT.CONTRACTS.XREF'
    F.JOINT.CONTRACTS.XREF  = ''
    CALL OPF(FN.JOINT.CONTRACTS.XREF,F.JOINT.CONTRACTS.XREF)

    CALL OPF(FN.CATEGORY,F.CATEGORY)
    Y.FLAG = ''
    Y.NOTIFY = ''
    Y.FIELD.COUNT = ''
    R.ACCT.REC = ''
    LOAN.FLG = ''
    DEP.FLG = ''
    R.AZ.REC = ''
    R.ACC = ''
    LREF.POS = ''
    LREF.APP='ACCOUNT'
    LREF.FIELDS='L.AC.STATUS1':@VM:'L.AC.STATUS2':@VM:'L.AC.AV.BAL':@VM:'L.AC.NOTIFY.1'
    CALL MULTI.GET.LOC.REF(LREF.APP,LREF.FIELDS,LREF.POS)
    ACCT.STATUS.POS=LREF.POS<1,1>
    ACCT.STATUS2.POS = LREF.POS<1,2>
    ACCT.OUT.BAL.POS=LREF.POS<1,3>
    NOTIFY.POS = LREF.POS<1,4>

    CUSTOMER.ID = System.getVariable('EXT.SMS.CUSTOMERS')
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN ;*R22 Auto conversion - start
        CUSTOMER.ID = ""
    END					;*R22 Auto conversion - end

RETURN
******************
FORM.ACCT.ARRAY:
*****************

    CALL CACHE.READ(FN.AI.REDO.ARC.PARAM,'SYSTEM',R.AI.REDO.ARC.PARAM,PARAM.ERR)
    CALL CACHE.READ(FN.AI.REDO.ACCT.RESTRICT.PARAMETER,'SYSTEM',R.AI.REDO.ACCT.RESTRICT.PARAMETER,RES.ERR)
    LIST.ACCT.TYPE=R.AI.REDO.ARC.PARAM<AI.PARAM.ACCOUNT.TYPE>
    Y.DEBIT.ACCT.AMT = System.getVariable('CURRENT.ARC.AMT')
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN ;*R22 Auto conversion - start
        Y.DEBIT.ACCT.AMT = ""
    END					;*R22 Auto conversion - end
    Y.RESTRICT.ACCT.TYPE = R.AI.REDO.ACCT.RESTRICT.PARAMETER<AI.RES.PARAM.RESTRICT.ACCT.TYPE>
    CHANGE @VM TO @FM IN Y.RESTRICT.ACCT.TYPE
    CALL F.READ(FN.CUSTOMER.ACCOUNT,CUSTOMER.ID,R.ACCT.REC,F.CUSTOMER.ACCOUNT,CUST.ERR)
    Y.REL.ACCTS = R.ACCT.REC
    CHECK.FLAG = 1
    R.ACCT.REC = ''
*------------PACS00459918----------------------------
    GOSUB CHECK.RELATION
    CHECK.FLAG = ''
*------------PACS00459918----------------------------
    GOSUB MINOR.CUST.PARA
    LOOP
        REMOVE ACCT.ID FROM R.ACCT.REC SETTING ACCT.POS
    WHILE ACCT.ID:ACCT.POS
        ACC.ERR= ''
        CHECK.CATEG=''
        SAV.FLG=''
        CURR.FLG=''
        Y.FLAG = ''
        CUR.ACCT.STATUS = ''
        AC.NOFITY.STATUS = ''
        Y.POSTING.RESTRICT = ''
        Y.RELATION.CODE = ''
        CALL F.READ(FN.ACC,ACCT.ID,R.ACC,F.ACC,ACC.ERR)
        IF R.ACC THEN
            AC.NOFITY.STATUS = R.ACC<AC.LOCAL.REF><1,NOTIFY.POS>
            CUR.ACCT.STATUS1 = R.ACC<AC.LOCAL.REF><1,ACCT.STATUS.POS>
            CUR.ACCT.STATUS2=R.ACC<AC.LOCAL.REF><1,ACCT.STATUS2.POS>
            ACCT.BAL = R.ACC<AC.LOCAL.REF><1,ACCT.OUT.BAL.POS>
            CUR.ACCT.CUSTOMER = R.ACC<AC.CUSTOMER>
            CHECK.CATEG = R.ACC<AC.CATEGORY>
            Y.POSTING.RESTRICT= R.ACC<AC.POSTING.RESTRICT>

            CHANGE @SM TO @FM IN CUR.ACCT.STATUS2
            CHANGE @SM TO @FM IN AC.NOFITY.STATUS
            LOCATE 'DEBIT' IN Y.RESTRICT.ACCT.TYPE SETTING RES.ACCT.POS THEN

                GOSUB STATUS.RESTRICTION.PARA
                GOSUB NOTIFY.RESTRICTION.PARA
                GOSUB POSTING.RESTRICTION.PARA
                GOSUB RELATION.PARA
            END

            GOSUB ACCT.CATEG.PARA
            GOSUB PAYMENT.ENQUIRY.PARA
            Y.LOOP.CONTINUE=''
            IF Y.FLAG THEN
                Y.LOOP.CONTINUE=1
            END
            IF NOT(Y.LOOP.CONTINUE) THEN
                GOSUB LOCATE.CURR.ACC
                GOSUB LOCATE.SAV.ACC
                GOSUB CHECK.STATUS.SUB
            END
        END

    REPEAT

RETURN
*------------PACS00459918----------------------------
*-----------------------------------------------------------------------------------------------------
CHECK.RELATION1:
*---------------------

    IF CHECK.FLAG EQ 1 THEN
        Y.REL.ACCT.CODES = R.ACCT<AC.RELATION.CODE>
        LOCATE 500 IN Y.REL.ACCT.CODES<1,1> SETTING REL.POS THEN

            RETURN
        END ELSE
            R.ACCT.REC<-1> = Y.REL.ACCT
            RETURN
        END
    END ELSE
        Y.JOINT.HOLDER = R.ACCT<AC.JOINT.HOLDER>

        LOCATE CUSTOMER.ID IN Y.JOINT.HOLDER<1,1> SETTING REL.ACCT.POS THEN
            Y.REL.ACCT.CODE = R.ACCT<AC.RELATION.CODE,REL.ACCT.POS>

        END
    END


    IF Y.REL.ACCT.CODE EQ 501 OR Y.REL.ACCT.CODE EQ 510 THEN
        R.ACCT.REC<-1> = Y.REL.ACCT
    END

*------------PACS00459597-----------------------


RETURN
*------------------------------------------------------------------------------------------------------
CHECK.RELATION:
*---------------
    Y.REL.ACCT.DC = DCOUNT(Y.REL.ACCTS,@FM)
    Y.REL.ACCT.CT = 1
    LOOP
    WHILE Y.REL.ACCT.CT LE Y.REL.ACCT.DC

        Y.REL.ACCT = FIELD(Y.REL.ACCTS,@FM,Y.REL.ACCT.CT)
        R.ACCT = ''
        CALL F.READ(FN.ACC,Y.REL.ACCT,R.ACCT,F.ACC,ERR.ACC)
        IF R.ACCT THEN
            GOSUB CHECK.RELATION1
        END
        Y.REL.ACCT.CT = 1 + Y.REL.ACCT.CT
    REPEAT

RETURN
*------------PACS00459918----------------------------
*------------------------------------------------------------------------------------------------------
*----------------*
MINOR.CUST.PARA:
*----------------*


    CALL F.READ(FN.JOINT.CONTRACTS.XREF,CUSTOMER.ID,R.JOINT.CONTRACTS.XREF,F.JOINT.CONTRACTS.XREF,JNT.XREF.ERR)
    IF R.JOINT.CONTRACTS.XREF THEN
        Y.REL.ACCTS = R.JOINT.CONTRACTS.XREF

        GOSUB CHECK.RELATION
    END


*    R.ACCT.REC<-1> = R.JOINT.CONTRACTS.XREF
RETURN
************************
STATUS.RESTRICTION.PARA:
************************
    IF CUR.ACCT.STATUS1 THEN
        CUR.ACCT.STATUS = CUR.ACCT.STATUS1
    END ELSE
        Y.FLAG = 1
        RETURN
    END
    IF CUR.ACCT.STATUS2 THEN
        CUR.ACCT.STATUS = CUR.ACCT.STATUS2
    END
    IF CUR.ACCT.STATUS1 AND CUR.ACCT.STATUS2 THEN
        CUR.ACCT.STATUS = CUR.ACCT.STATUS1:@FM:CUR.ACCT.STATUS2
    END

    Y.CNT.STATUS = DCOUNT(CUR.ACCT.STATUS,@FM)
    IF Y.CNT.STATUS GE 1 THEN
        Y.INT.STATUS.CNT = 1
        LOOP
        WHILE Y.INT.STATUS.CNT LE Y.CNT.STATUS
            Y.STATUS2 = CUR.ACCT.STATUS<Y.INT.STATUS.CNT>
            Y.RESTRICT.STATUS = R.AI.REDO.ACCT.RESTRICT.PARAMETER<AI.RES.PARAM.ACCT.STATUS,RES.ACCT.POS>
            CHANGE @SM TO @FM IN Y.RESTRICT.STATUS
            LOCATE Y.STATUS2 IN Y.RESTRICT.STATUS SETTING RES.STATUS.POS THEN
                Y.FLAG = 1
                RETURN
            END
            Y.INT.STATUS.CNT += 1
        REPEAT
    END

RETURN

************************
NOTIFY.RESTRICTION.PARA:
************************

    Y.CNT.NOTIFY = DCOUNT(AC.NOFITY.STATUS,@FM)
    IF Y.CNT.NOTIFY GE 1 THEN
        Y.INT.NOTIFY.CNT = 1
        LOOP
        WHILE Y.INT.NOTIFY.CNT LE Y.CNT.NOTIFY
            Y.NOTIFY = AC.NOFITY.STATUS<Y.INT.NOTIFY.CNT>
            Y.RESTRICT.NOTIFY = R.AI.REDO.ACCT.RESTRICT.PARAMETER<AI.RES.PARAM.ACCT.NOTIFY.1,RES.ACCT.POS>
            CHANGE @SM TO @FM IN Y.RESTRICT.NOTIFY
            LOCATE Y.NOTIFY IN Y.RESTRICT.NOTIFY SETTING RES.NOTIFY.POS THEN
                Y.FLAG = 1
                RETURN
            END
            Y.INT.NOTIFY.CNT += 1
        REPEAT
    END

RETURN
*************************
POSTING.RESTRICTION.PARA:
*************************
    Y.RESTRICT.ACCT.POSTING = R.AI.REDO.ACCT.RESTRICT.PARAMETER<AI.RES.PARAM.POSTING.RESTRICT,RES.ACCT.POS>
    CHANGE @SM TO @FM IN Y.RESTRICT.ACCT.POSTING
    IF Y.POSTING.RESTRICT THEN
        LOCATE Y.POSTING.RESTRICT IN Y.RESTRICT.ACCT.POSTING SETTING POSTING.POS THEN
            Y.FLAG = 1
        END
    END
RETURN
**********************
PAYMENT.ENQUIRY.PARA:
**********************

    IF ENQ.SELECTION<1> EQ 'AI.REDO.ACCT.LIST.FROM.PAY' THEN
        IF ACCT.BAL LT Y.DEBIT.ACCT.AMT THEN
            Y.FLAG = 1
        END
    END
RETURN

*----------------*
RELATION.PARA:
*----------------*
*------------------------PACS00465952---------------------------------------------
    Y.RELATION.CODE = R.ACC<AC.RELATION.CODE>
    Y.REL.PARAM = R.AI.REDO.ARC.PARAM<AI.PARAM.RELATION.CODE>
    Y.JOINTS.HOLD = R.ACC<AC.JOINT.HOLDER>
    LOCATE CUSTOMER.ID IN Y.JOINTS.HOLD<1,1> SETTING HOLD.POS THEN
        Y.REL.CODE = R.ACC<AC.RELATION.CODE,HOLD.POS>
    END
    CHANGE @VM TO @FM IN Y.REL.PARAM
    IF CUR.ACCT.CUSTOMER NE CUSTOMER.ID THEN
        IF Y.REL.PARAM THEN
            LOCATE Y.REL.CODE IN Y.REL.PARAM SETTING Y.REL.POS THEN
                RETURN
            END ELSE
                Y.FLAG = 1
            END
        END ELSE
            Y.FLAG = 1
        END
    END
*-------------------------------------PACS00465952-------------------------------------
RETURN
*----------------*
ACCT.CATEG.PARA:
*----------------*
    Y.RES.ACCT.CATEG = R.AI.REDO.ACCT.RESTRICT.PARAMETER<AI.RES.PARAM.ACCT.CATEGORY>
    CHANGE @VM TO @FM IN Y.RES.ACCT.CATEG
    LOCATE CHECK.CATEG IN Y.RES.ACCT.CATEG SETTING RES.ACCT.POS THEN
        Y.FLAG = 1
    END
RETURN
******************
CHECK.STATUS.SUB:
******************
    IF ACCT.BAL GT '0' THEN

        GOSUB CHECK.LOAN.ACC
        GOSUB CHECK.AZ.ACC

        IF  (SAV.FLG EQ '1') OR (CURR.FLG EQ '1') THEN
            FIN.ARR<-1> = ACCT.ID:"@":CUSTOMER.ID
        END
    END
RETURN
******************
CHECK.LOAN.ACC:
******************

    ARR.ID = R.ACC<AC.ARRANGEMENT.ID>
    IF ARR.ID NE '' THEN

        LOAN.FLG = 1
    END

RETURN

*******************
CHECK.AZ.ACC:
******************

    CALL F.READ(FN.AZ,ACCT.ID,R.AZ.REC,F.AZ,AZ.ERR)

    IF R.AZ.REC THEN
        DEP.FLG = 1

    END
RETURN
*******************
LOCATE.SAV.ACC:
*****************

    CHANGE @VM TO @FM IN LIST.ACCT.TYPE
    Y.CNT.CATEG.TYPE  = DCOUNT(LIST.ACCT.TYPE,@FM)
    Y.CNT.CAT = 1
    LOOP
    WHILE  Y.CNT.CAT LE Y.CNT.CATEG.TYPE
        Y.CATEG.ID = LIST.ACCT.TYPE<Y.CNT.CAT>
        IF 'SAVINGS' EQ Y.CATEG.ID THEN
            SAV.STR.RGE=R.AI.REDO.ARC.PARAM<AI.PARAM.CATEG.START,Y.CNT.CAT>
            SAV.END.RGE=R.AI.REDO.ARC.PARAM<AI.PARAM.CATEG.END,Y.CNT.CAT>
            IF CHECK.CATEG GE SAV.STR.RGE AND CHECK.CATEG LE SAV.END.RGE THEN
                SAV.FLG = 1
                RETURN
            END
        END
        Y.CNT.CAT += 1
    REPEAT

RETURN

***************
LOCATE.CURR.ACC:
***************
    CHANGE @VM TO @FM IN LIST.ACCT.TYPE
    LOCATE 'CURRENT' IN LIST.ACCT.TYPE SETTING SAV.ACCT.POS THEN
        SAV.STR.RGE=R.AI.REDO.ARC.PARAM<AI.PARAM.CATEG.START,SAV.ACCT.POS>
        SAV.END.RGE=R.AI.REDO.ARC.PARAM<AI.PARAM.CATEG.END,SAV.ACCT.POS>
        IF CHECK.CATEG GE SAV.STR.RGE AND CHECK.CATEG LE SAV.END.RGE THEN

            CURR.FLG=1
        END
    END

RETURN
*-----------------------------------------------------------------------------
END
*---------------------------*END OF SUBROUTINE*-------------------------------
