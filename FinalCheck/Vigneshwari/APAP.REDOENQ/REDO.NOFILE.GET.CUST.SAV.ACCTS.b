* @ValidationCode : MjoyMDEyOTk2MzUwOkNwMTI1MjoxNjgyMDczMzg0NjEyOklUU1M6LTE6LTE6MTAxMjoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 21 Apr 2023 16:06:24
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 1012
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.NOFILE.GET.CUST.SAV.ACCTS(CUST.ACC.DET)


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
*22/06/15       PACS00465952            ASLAM  1                 MODIFICATION
*
* 17-APR-2023     Conversion tool   R22 Auto conversion   FM TO @FM, VM to @VM, ++ to +=
* 17-APR-2023      Harishvikram C   Manual R22 conversion      No changes
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
    $INSERT I_F.AI.REDO.ACCT.RESTRICT.PARAMETER
*Tus Start
    $INSERT I_F.EB.CONTRACT.BALANCES
*Tus End

*---------*
MAIN.PARA:
*---------*

    GOSUB INITIALISE
    GOSUB OPEN.FILES
    GOSUB PROCESS

    CRT CUST.ACC.DET

RETURN
*----------*
INITIALISE:
*----------*
    FN.CUSTOMER.ACCOUNT = "F.CUSTOMER.ACCOUNT"
    F.CUSTOMER.ACCOUNT = ''
    FN.ACCOUNT = "F.ACCOUNT"
    F.ACCOUNT = ''
    FN.CATEGORY = "F.CATEGORY"
    F.CATEGORY = ''
    FN.RELATION.CUSTOMER = 'F.RELATION.CUSTOMER'
    F.RELATION.CUSTOMER  = ''
    FN.AI.REDO.ARC.PARAM='F.AI.REDO.ARCIB.PARAMETER'
    FN.ACCT.RESTRICT.PARAMETER = 'F.AI.REDO.ACCT.RESTRICT.PARAMETER'
    FN.AI.REDO.ALIAS.TABLE = 'F.AI.REDO.ARCIB.ALIAS.TABLE'
    F.AI.REDO.ALIAS.TABLE = ''
    FN.JOINT.CONTRACTS.XREF = 'F.JOINT.CONTRACTS.XREF'
    F.JOINT.CONTRACTS.XREF  = ''
    LREF.POS = ''
    ACCT.STATUS=''
    SAV.ACCT.POS=''
    SAVINGS.ACCT= ''
    LREF.APP='ACCOUNT'
    LREF.FIELDS='L.AC.STATUS1':@VM:'L.AC.AV.BAL'

RETURN
*----------*
OPEN.FILES:
*----------*
    CALL OPF(FN.CUSTOMER.ACCOUNT,F.CUSTOMER.ACCOUNT)
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
    CALL OPF(FN.CATEGORY,F.CATEGORY)
    CALL OPF(FN.AI.REDO.ALIAS.TABLE,F.AI.REDO.ALIAS.TABLE)
    CALL OPF(FN.RELATION.CUSTOMER,F.RELATION.CUSTOMER)
    CALL OPF(FN.JOINT.CONTRACTS.XREF,F.JOINT.CONTRACTS.XREF)
    CALL MULTI.GET.LOC.REF(LREF.APP,LREF.FIELDS,LREF.POS)
    ACCT.STATUS.POS=LREF.POS<1,1>
    ACCT.OUT.BAL.POS=LREF.POS<1,2>
RETURN
*----------------------------------------------------

*--------*
PROCESS:
*--------*

    CUST.ID = System.getVariable("EXT.SMS.CUSTOMERS")
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN ;*R22 Auto conversion - START
        CUST.ID = ""
    END					;*R22 Auto conversion - END

    CALL CACHE.READ(FN.ACCT.RESTRICT.PARAMETER,'SYSTEM',R.ACCT.RESTRICT.PARAMETER,RESTR.ERR)
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
    Y.FLAG  = ''
    Y.RES.FLAG = ''
    R.ACCT= ''
    CALL F.READ(FN.ACCOUNT,ACCT.ID,R.ACCT,F.ACCOUNT,ACCT.ERR)
    R.ECB= '' ; ECB.ERR= '' ;*Tus Start
    CALL EB.READ.HVT("EB.CONTRACT.BALANCES",ACCT.ID,R.ECB,ECB.ERR);*Tus End
    IF NOT(ACCT.ERR) THEN
        CUR.ACCT.STATUS=R.ACCT<AC.LOCAL.REF><1,ACCT.STATUS.POS>
        IF CUR.ACCT.STATUS EQ 'ACTIVE' THEN
            GOSUB READ.ALIAS.TABLE
            CUR.ACCT.BAL = R.ACCT<AC.LOCAL.REF><1,ACCT.OUT.BAL.POS>
            CUR.ACCT.CATEG = R.ACCT<AC.CATEGORY>
            CUR.ACCT.SHRT.TITLE = R.ACCT<AC.SHORT.TITLE>
            CUR.ACCT.OPEN.DTE = R.ACCT<AC.OPENING.DATE>
*   CUR.ACCT.OPEN.BAL = R.ACCT<AC.ONLINE.ACTUAL.BAL>;*Tus Start
            CUR.ACCT.OPEN.BAL = R.ECB<ECB.ONLINE.ACTUAL.BAL>;*Tus End
            CUR.ACCT.CUSTOMER = R.ACCT<AC.CUSTOMER>
            CALL CACHE.READ(FN.CATEGORY, CUR.ACCT.CATEG, R.CATEG, CATEG.ERR) ;*R22 Auto conversion
            CATEG.DESC=R.CATEG<EB.CAT.DESCRIPTION>
            CHECK.CATEG=R.ACCT<AC.CATEGORY>
            LIST.ACCT.TYPE=R.AI.REDO.ARC.PARAM<AI.PARAM.ACCOUNT.TYPE>
            GOSUB RELATION.PARA
            IF NOT(Y.FLAG) THEN
                GOSUB ACCT.CATEG.PARA
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
    Y.RELATION.CODE = R.ACCT<AC.RELATION.CODE>
    Y.JOINTS.HOLD = R.ACCT<AC.JOINT.HOLDER>
    LOCATE CUST.ID IN Y.JOINTS.HOLD<1,1> SETTING HOLD.POS THEN
        Y.REL.CODE = R.ACCT<AC.RELATION.CODE,HOLD.POS>
    END
    Y.REL.PARAM = R.AI.REDO.ARC.PARAM<AI.PARAM.RELATION.CODE>
    CHANGE @VM TO @FM IN Y.REL.PARAM
    IF CUR.ACCT.CUSTOMER NE CUST.ID THEN
        IF Y.REL.PARAM THEN
* Y.CNT.REL.CODE  = DCOUNT(Y.RELATION.CODE,VM)

* Y.CNT.REL = 1
* LOOP
* WHILE Y.CNT.REL LE Y.CNT.REL.CODE
*    Y.REL.CODE = Y.RELATION.CODE<1,Y.CNT.REL>

            LOCATE Y.REL.CODE IN Y.REL.PARAM SETTING Y.REL.POS THEN
                RETURN
            END ELSE
                Y.FLAG = 1
            END
*   Y.CNT.REL++
*REPEAT
        END ELSE
            Y.FLAG = 1
        END
    END

RETURN
*------------------------PACS00465952---------------------------------------------
*----------------*
ACCT.CATEG.PARA:
*----------------*
    Y.RES.ACCT.CATEG = R.ACCT.RESTRICT.PARAMETER<AI.RES.PARAM.ACCT.CATEGORY>
    CHANGE @VM TO @FM IN Y.RES.ACCT.CATEG
    LOCATE CHECK.CATEG IN Y.RES.ACCT.CATEG SETTING RES.ACCT.POS THEN
        Y.RES.FLAG = 1
    END
    IF NOT(Y.RES.FLAG) THEN
        GOSUB SAVINGS.PARA
    END
RETURN
*----------------*
SAVINGS.PARA:
*---------------*

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
                CUST.ACC.DET<-1> =ACCT.ID:"@":CATEG.DESC:"@":CUR.ALIAS.NAME:"@":CUR.ACCT.OPEN.DTE:"@":CUR.ACCT.BAL:"@":CUR.ACCT.OPEN.BAL
                RETURN
            END
        END
        Y.CNT.CAT += 1
    REPEAT

RETURN
*----------------*
READ.ALIAS.TABLE:
*---------------*
    R.ALIAS.REC = ''
    CUR.ALIAS.NAME = ''
    CALL F.READ(FN.AI.REDO.ALIAS.TABLE,ACCT.ID,R.ALIAS.REC,F.AI.REDO.ALIAS.TABLE,ALIAS.ERR)
    IF R.ALIAS.REC THEN
        CUR.ALIAS.NAME = R.ALIAS.REC<AI.ALIAS.ALIAS.NAME>

    END

RETURN

END
