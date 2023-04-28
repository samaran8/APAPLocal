$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.NOFILE.GET.CUST.CUR.ACCTS(CUST.ACC.DET)
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
*22/06/15       PACS00465952            ASLAM 1                  MODIFICATION
*-----------------------------------------------------------------------------
*------------------------------------------------------------------------
* Modification History :
*------------------------------------------------------------------------
*  DATE             WHO                   REFERENCE                  
* 13-APRIL-2023      Conversion Tool       R22 Auto Conversion  - Added IF E EQ "EB-UNKNOWN.VARIABLE" THEN , F.READ to CACHE.READ , VM to @VM , FM to @FM
* 13-APRIL-2023      Harsha                R22 Manual Conversion - No changes                             
*------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_System
    $INSERT I_EB.EXTERNAL.COMMON
    $INSERT I_F.CUSTOMER.ACCOUNT
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CATEGORY
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
    CALL OPF(FN.JOINT.CONTRACTS.XREF,F.JOINT.CONTRACTS.XREF)
    CALL MULTI.GET.LOC.REF(LREF.APP,LREF.FIELDS,LREF.POS)
    ACCT.STATUS.POS=LREF.POS<1,1>
    ACCT.OUT.BAL.POS=LREF.POS<1,2>
RETURN

*--------*
PROCESS:
*--------*

    CUST.ID = System.getVariable("EXT.SMS.CUSTOMERS")
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN	;*R22 Auto Conversion  - Added IF E EQ "EB-UNKNOWN.VARIABLE" THEN
        CUST.ID = ""
    END


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
    R.ACCT= ''
    CALL F.READ(FN.ACCOUNT,ACCT.ID,R.ACCT,F.ACCOUNT,ACCT.ERR)
    R.ECB= '' ; ECB.ERR= '' ;*Tus Start
    CALL EB.READ.HVT("EB.CONTRACT.BALANCES",ACCT.ID,R.ECB,ECB.ERR);*Tus End
    IF R.ACCT THEN
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
            CALL CACHE.READ(FN.CATEGORY, CUR.ACCT.CATEG, R.CATEG, CATEG.ERR)	;*R22 Auto Conversion  - F.READ to CACHE.READ
            CATEG.DESC=R.CATEG<EB.CAT.DESCRIPTION>
            CHECK.CATEG=R.ACCT<AC.CATEGORY>
            LIST.ACCT.TYPE=R.AI.REDO.ARC.PARAM<AI.PARAM.ACCOUNT.TYPE>
            GOSUB RELATION.PARA
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
    Y.REL.FLAG = ''
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
*  Y.CNT.REL = 1
*   LOOP
*   WHILE Y.CNT.REL LE Y.CNT.REL.CODE
*       Y.REL.CODE = Y.RELATION.CODE<1,Y.CNT.REL>
            LOCATE Y.REL.CODE IN Y.REL.PARAM SETTING Y.REL.POS THEN
                RETURN
            END ELSE
                Y.REL.FLAG = 1
            END
*      Y.CNT.REL++
*  REPEAT
        END ELSE
            Y.REL.FLAG = 1
        END
    END
    IF NOT(Y.REL.FLAG) THEN
        GOSUB CURRENT.ACCT.PARA
    END
RETURN
*------------------------PACS00465952---------------------------------------------
*----------------*
CURRENT.ACCT.PARA:
*----------------*

    LOCATE 'CURRENT' IN LIST.ACCT.TYPE<1,1> SETTING SAV.ACCT.POS THEN
        SAV.STR.RGE=R.AI.REDO.ARC.PARAM<AI.PARAM.CATEG.START,SAV.ACCT.POS>
        SAV.END.RGE=R.AI.REDO.ARC.PARAM<AI.PARAM.CATEG.END,SAV.ACCT.POS>
        IF CHECK.CATEG GE SAV.STR.RGE AND CHECK.CATEG LE SAV.END.RGE THEN
            CUST.ACC.DET<-1> =ACCT.ID:"@":CATEG.DESC:"@":CUR.ALIAS.NAME:"@":CUR.ACCT.OPEN.DTE:"@":CUR.ACCT.BAL:"@":CUR.ACCT.OPEN.BAL
        END
    END
RETURN

*----------------*
READ.ALIAS.TABLE:
*----------------*
    R.ALIAS.REC=''
    CUR.ALIAS.NAME=''
    CALL F.READ(FN.AI.REDO.ALIAS.TABLE,ACCT.ID,R.ALIAS.REC,F.AI.REDO.ALIAS.TABLE,ALIAS.ERR)
    IF R.ALIAS.REC THEN
        CUR.ALIAS.NAME = R.ALIAS.REC<AI.ALIAS.ALIAS.NAME>

    END

RETURN

END
