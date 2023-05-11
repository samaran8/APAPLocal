* @ValidationCode : Mjo2MzEwMDU0MzQ6Q3AxMjUyOjE2ODIwNzg4NzM0NjA6SVRTUzotMTotMToxMjExOjE6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 21 Apr 2023 17:37:53
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 1211
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.NOFILE.GET.CUST.DEP.ACCTS(CUST.ACC.DET)
*-----------------------------------------------------------------------------
* Company Name : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By : Prabhu N
* Program Name :
*-----------------------------------------------------------------------------
* Description    :  This Nofile routine will get required details of Customer Accts

* In Parameter   :
* Out Parameter  :
**DATE           ODR                   DEVELOPER               VERSION
*
*05/06/15       PACS00459918            ASLAM                   MODIFICATION
*22/06/15       PACS00465952            ASLAM   1                MODIFICATION
*
* 18-APR-2023     Conversion tool   R22 Auto conversion   FM TO @FM, VM to @VM, if condition added
* 18-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_System
    $INSERT I_EB.EXTERNAL.COMMON
    $INSERT I_F.CUSTOMER.ACCOUNT
    $INSERT I_F.ACCOUNT
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.CATEGORY
    $INSERT I_F.RELATION.CUSTOMER
    $INSERT I_F.AI.REDO.ARCIB.PARAMETER
    $INSERT I_F.AI.REDO.ARCIB.ALIAS.TABLE


*---------*
MAIN.PARA:
*---------*
    GOSUB INITIALISE
    GOSUB OPEN.FILES
    GOSUB PROCESS

RETURN
*----------*
INITIALISE:
*----------*
    FN.CUSTOMER.ACCOUNT = "F.CUSTOMER.ACCOUNT"
    F.CUSTOMER.ACCOUNT = ''
    FN.ACCOUNT = "F.ACCOUNT"
    F.ACCOUNT = ''
    FN.AZ.ACC = 'F.AZ.ACCOUNT'
    F.AZ.ACC = ''
    FN.CATEGORY = "F.CATEGORY"
    F.CATEGORY = ''
    FN.RELATION.CUSTOMER = 'F.RELATION.CUSTOMER'
    F.RELATION.CUSTOMER  = ''
    FN.AI.REDO.ARC.PARAM='F.AI.REDO.ARCIB.PARAMETER'
    FN.AI.REDO.ALIAS.TABLE = 'F.AI.REDO.ARCIB.ALIAS.TABLE'
    F.AI.REDO.ALIAS.TABLE = ''
    FN.JOINT.CONTRACTS.XREF = 'F.JOINT.CONTRACTS.XREF'
    F.JOINT.CONTRACTS.XREF  = ''
    LREF.POS = ''
    ACCT.STATUS=''
    SAV.ACCT.POS=''
    SAVINGS.ACCT= ''
    Y.FLAG = ''
    LREF.APP='ACCOUNT':@FM:'AZ.ACCOUNT'
    LREF.FIELDS='L.AC.STATUS1':@VM:'L.AC.AV.BAL':@FM:'L.TYPE.INT.PAY'

RETURN
*----------*
OPEN.FILES:
*----------*
    CALL OPF(FN.CUSTOMER.ACCOUNT,F.CUSTOMER.ACCOUNT)
    CALL OPF(FN.RELATION.CUSTOMER,F.RELATION.CUSTOMER)
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
    CALL OPF(FN.CATEGORY,F.CATEGORY)
    CALL OPF(FN.AZ.ACC,F.AZ.ACC)
    CALL OPF(FN.AI.REDO.ALIAS.TABLE,F.AI.REDO.ALIAS.TABLE)
    CALL OPF(FN.JOINT.CONTRACTS.XREF,F.JOINT.CONTRACTS.XREF)
    CALL MULTI.GET.LOC.REF(LREF.APP,LREF.FIELDS,LREF.POS)
    ACCT.STATUS.POS=LREF.POS<1,1>
    ACCT.OUT.BAL.POS=LREF.POS<1,2>
    POS.L.TYPE.INT.PAY.POS = LREF.POS<2,1>
RETURN

*--------*
PROCESS:
*--------*

    CUST.ID = System.getVariable("EXT.SMS.CUSTOMERS")
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN ;*R22 Auto conversion - start
        CUST.ID = ""
    END					;*R22 Auto conversion - end

    CALL CACHE.READ(FN.AI.REDO.ARC.PARAM,'SYSTEM',R.AI.REDO.ARC.PARAM,PARAM.ERR)
    CALL F.READ(FN.CUSTOMER.ACCOUNT,CUST.ID,R.CUST.ACC,F.CUSTOMER.ACCOUNT,CUST.ACC.ERR)
*------------PACS00459918----------------------------
    GOSUB MINOR.CUST.PARA
*------------PACS00459918----------------------------
    IF R.CUST.ACC THEN

        LOOP

            REMOVE ACCT.ID FROM R.CUST.ACC SETTING CUST.ACC.POS
        WHILE ACCT.ID:CUST.ACC.POS
            GOSUB ACCT.DETAILS.PARA
        REPEAT
    END

RETURN
*----------------*
ACCT.DETAILS.PARA:
*----------------*
    CALL F.READ(FN.ACCOUNT,ACCT.ID,R.ACCT,F.ACCOUNT,ACCT.ERR)

    IF R.ACCT THEN

        GOSUB AZ.READ.ACC
        CUR.ACCT.STATUS=R.ACCT<AC.LOCAL.REF><1,ACCT.STATUS.POS>
        IF CUR.ACCT.STATUS EQ 'ACTIVE' AND AZ.PRIN.AMT GT '0' THEN

            GOSUB READ.ALIAS.TABLE
            CUR.ACCT.BAL = R.ACCT<AC.LOCAL.REF><1,ACCT.OUT.BAL.POS>
            CUR.ACCT.CATEG = R.ACCT<AC.CATEGORY>
            CUR.ACCT.SHRT.TITLE = R.ACCT<AC.SHORT.TITLE>
            CUR.ACCT.OPEN.DTE = R.ACCT<AC.OPENING.DATE>
            GOSUB AZ.REINVEST.CHECK
            CUR.ACCT.CUSTOMER = R.ACCT<AC.CUSTOMER>
            CALL CACHE.READ(FN.CATEGORY, CUR.ACCT.CATEG, R.CATEG, CATEG.ERR) ;*R22 Auto conversion
            CATEG.DESC=R.CATEG<EB.CAT.DESCRIPTION>
            CHECK.CATEG=R.ACCT<AC.CATEGORY>
            LIST.ACCT.TYPE=R.AI.REDO.ARC.PARAM<AI.PARAM.ACCOUNT.TYPE>
            GOSUB RELATION.PARA
            IF NOT(Y.FLAG) THEN
                GOSUB DEPOSIT.ACCT.PARA
            END

        END
    END
RETURN
*----------------*
MINOR.CUST.PARA:
*----------------*
    CALL F.READ(FN.JOINT.CONTRACTS.XREF,CUST.ID,R.JOINT.CONTRACTS.XREF,F.JOINT.CONTRACTS.XREF,JNT.XREF.ERR)
    R.CUST.ACC<-1> = R.JOINT.CONTRACTS.XREF
RETURN
*----------------*
RELATION.PARA:
*----------------*
*------------------------PACS00465952---------------------------------------------
    Y.FLAG = ''
    Y.RELATION.CODE = R.ACCT<AC.RELATION.CODE>
    Y.JOINTS.HOLD = R.ACCT<AC.JOINT.HOLDER>
    LOCATE CUST.ID IN Y.JOINTS.HOLD<1,1> SETTING HOLD.POS THEN
        Y.REL.CODE = R.ACCT<AC.RELATION.CODE,HOLD.POS>
    END
    Y.REL.PARAM = R.AI.REDO.ARC.PARAM<AI.PARAM.RELATION.CODE>
    CHANGE @VM TO @FM IN Y.REL.PARAM
    IF CUR.ACCT.CUSTOMER NE CUST.ID THEN
        IF Y.REL.PARAM THEN
*  Y.CNT.REL.CODE  = DCOUNT(Y.RELATION.CODE,VM)
* Y.CNT.REL = 1
* LOOP
*  WHILE Y.CNT.REL LE Y.CNT.REL.CODE
*     Y.REL.CODE = Y.RELATION.CODE<1,Y.CNT.REL>
            LOCATE Y.REL.CODE IN Y.REL.PARAM SETTING Y.REL.POS THEN
                RETURN
            END ELSE
                Y.FLAG = 1
            END
*     Y.CNT.REL++
* REPEAT
        END ELSE
            Y.FLAG = 1
        END
    END
*------------------------PACS00465952---------------------------------------------
RETURN
*---------------*
DEPOSIT.ACCT.PARA:
*---------------*
    CHANGE @VM TO @FM IN LIST.ACCT.TYPE
    LOCATE 'DEPOSIT' IN LIST.ACCT.TYPE SETTING SAV.ACCT.POS THEN
        SAV.STR.RGE=R.AI.REDO.ARC.PARAM<AI.PARAM.CATEG.START,SAV.ACCT.POS>
        SAV.END.RGE=R.AI.REDO.ARC.PARAM<AI.PARAM.CATEG.END,SAV.ACCT.POS>
        IF CHECK.CATEG GE SAV.STR.RGE AND CHECK.CATEG LE SAV.END.RGE THEN
            CUST.ACC.DET<-1> =ACCT.ID:"@":CATEG.DESC:"@":CUR.ALIAS.NAME:"@":CUR.ACCT.OPEN.DTE:"@":AZ.PRIN.AMT:"@":CUR.ACCT.OPEN.BAL
        END
    END

RETURN
*---------------*
AZ.READ.ACC:
*---------------*
    R.AZ.REC=''
    AZ.PRIN.AMT = ''
    CALL F.READ(FN.AZ.ACC,ACCT.ID,R.AZ.REC,F.AZ.ACC,AZ.ERR)
    IF R.AZ.REC THEN
        Y.DEP.TYPE  = R.AZ.REC<AZ.LOCAL.REF,POS.L.TYPE.INT.PAY.POS>
        AZ.PRIN.AMT = R.AZ.REC<AZ.PRINCIPAL>
        IF Y.DEP.TYPE EQ 'Reinvested' THEN
            Y.INTEREST.LIQU.ACCT  = R.AZ.REC<AZ.INTEREST.LIQU.ACCT>
            CALL F.READ(FN.ACCOUNT,Y.INTEREST.LIQU.ACCT,R.LIQ.ACCOUNT,F.ACCOUNT,ACCOUNT.ERR)
            CUR.ACCT.OPEN.BAL = AZ.PRIN.AMT + R.LIQ.ACCOUNT<AC.LOCAL.REF,ACCT.OUT.BAL.POS>
        END ELSE
            CUR.ACCT.OPEN.BAL = AZ.PRIN.AMT
        END

    END



RETURN
*---------------*
AZ.REINVEST.CHECK:
*---------------*
RETURN

*----------------*
READ.ALIAS.TABLE:
*---------------*
    R.ALIAS.REC = ''
    CUR.ALIAS.NAME = ''
    CALL F.READ(FN.AI.REDO.ALIAS.TABLE,CUST.ID,R.ALIAS.REC,F.AI.REDO.ALIAS.TABLE,ALIAS.ERR)
    IF R.ALIAS.REC THEN
        CUR.ALIAS.NAME = R.ALIAS.REC<AI.ALIAS.ALIAS.NAME>

    END

RETURN

END
