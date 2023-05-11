$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.NOFILE.GET.CUST.ACCTS(CUST.ACC.DET)
*-----------------------------------------------------------------------------
* Company Name : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By : Martin Macias
* Program Name :
*-----------------------------------------------------------------------------
* Description    :  This Nofile routine will get required details of Customer Accts
* Linked with    :
* In Parameter   :
* Out Parameter  :
*-----------------------------------------------------------------------------
*------------------------------------------------------------------------
* Modification History :
*------------------------------------------------------------------------
*  DATE             WHO                   REFERENCE                  
* 13-APRIL-2023      Conversion Tool      R22 Auto Conversion  - Added IF E EQ "EB-UNKNOWN.VARIABLE" THEN , VM ro @VM
* 13-APRIL-2023      Harsha                R22 Manual Conversion - No changes                             
*------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_System

    $INSERT I_EB.EXTERNAL.COMMON
    $INSERT I_F.CUSTOMER.ACCOUNT
    $INSERT I_F.ACCOUNT
    $INSERT I_F.AI.REDO.ARCIB.PARAMETER

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
    FN.AI.REDO.ARC.PARAM='F.AI.REDO.ARCIB.PARAMETER'

    LREF.POS = ''
    SAV.ACCT.POS=''
    CUR.ACCT.POS=''

    LREF.APP='ACCOUNT'
    LREF.FIELDS='L.AC.STATUS1':@VM:'L.AC.AV.BAL'

RETURN

*----------*
OPEN.FILES:
*----------*

    CALL OPF(FN.AI.REDO.ALIAS.TABLE,F.AI.REDO.ALIAS.TABLE)
    CALL OPF(FN.CUSTOMER.ACCOUNT,F.CUSTOMER.ACCOUNT)
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

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

    CALL CACHE.READ(FN.AI.REDO.ARC.PARAM,'SYSTEM',R.AI.REDO.ARC.PARAM,PARAM.ERR)

    LIST.ACCT.TYPE=R.AI.REDO.ARC.PARAM<AI.PARAM.ACCOUNT.TYPE>
    LOCATE 'SAVINGS' IN LIST.ACCT.TYPE<1,1> SETTING SAV.ACCT.POS THEN
        SAV.STR.RGE=R.AI.REDO.ARC.PARAM<AI.PARAM.CATEG.START,SAV.ACCT.POS>
        SAV.END.RGE=R.AI.REDO.ARC.PARAM<AI.PARAM.CATEG.END,SAV.ACCT.POS>
    END

    LOCATE 'CURRENT' IN LIST.ACCT.TYPE<1,1> SETTING CUR.ACCT.POS THEN
        CUR.STR.RGE=R.AI.REDO.ARC.PARAM<AI.PARAM.CATEG.START,CUR.ACCT.POS>
        CUR.END.RGE=R.AI.REDO.ARC.PARAM<AI.PARAM.CATEG.END,CUR.ACCT.POS>
    END

    CALL F.READ(FN.CUSTOMER.ACCOUNT,CUST.ID,R.CUST.ACC,F.CUSTOMER.ACCOUNT,CUST.ACC.ERR)

    IF NOT(CUST.ACC.ERR) THEN

        LOOP
            REMOVE ACCT.ID FROM R.CUST.ACC SETTING CUST.ACC.POS
        WHILE ACCT.ID:CUST.ACC.POS
            R.ACCT= ''
            CALL F.READ(FN.ACCOUNT,ACCT.ID,R.ACCT,F.ACCOUNT,ACCT.ERR)
            IF NOT(ACCT.ERR) THEN

                CUR.ACCT.STATUS=R.ACCT<AC.LOCAL.REF><1,ACCT.STATUS.POS>
                IF CUR.ACCT.STATUS EQ 'ACTIVE' THEN
                    CHECK.CATEG=R.ACCT<AC.CATEGORY>

                    IF (CHECK.CATEG GE SAV.STR.RGE AND CHECK.CATEG LE SAV.END.RGE) OR (CHECK.CATEG GE CUR.STR.RGE AND CHECK.CATEG LE CUR.END.RGE) THEN
                        CUST.ACC.DET<-1> =ACCT.ID:"@"
                    END

                END
            END

        REPEAT
    END
RETURN

END
