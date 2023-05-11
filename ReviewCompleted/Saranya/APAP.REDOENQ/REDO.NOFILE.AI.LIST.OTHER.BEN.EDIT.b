* @ValidationCode : MjotMTYxNzIxMzA2NjpDcDEyNTI6MTY4MjA3MzM4NDI5ODpJVFNTOi0xOi0xOjU4MToxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 21 Apr 2023 16:06:24
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 581
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.NOFILE.AI.LIST.OTHER.BEN.EDIT(FIN.ARR)
*-----------------------------------------------------------------------------
* Company Name : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By :
* Program Name REDO.NOFILE.AI.LIST.APAP.BEN.EDIT
*-----------------------------------------------------------------------------
* Description : This subroutine is attached as a BUILD routine in the Enquiry REDO.SAV.ACCOUNT.LIST
* Linked with : Enquiry REDO.SAV.ACCOUNT.LIST  as BUILD routine
* In Parameter : ENQ.DATA
* Out Parameter : None
*
**DATE           ODR                   DEVELOPER               VERSION
*
*01/11/11       PACS00146410            PRABHU N                MODIFICAION
** 17-APR-2023     Conversion tool   R22 Auto conversion   FM TO @FM, VM to @VM
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
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.AI.REDO.ARCIB.PARAMETER
    $INSERT I_F.AI.REDO.ARCIB.ALIAS.TABLE
    $INSERT I_F.BENEFICIARY
    $INSERT I_F.AI.REDO.ACCT.RESTRICT.PARAMETER

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

    FN.BENEFICIARY = 'F.BENEFICIARY'
    F.BENEFICIARY = ''
    CALL OPF(FN.BENEFICIARY,F.BENEFICIARY)

    FN.AI.REDO.ARCIB.ALIAS.TABLE = 'F.AI.REDO.ARCIB.ALIAS.TABLE'
    F.AI.REDO.ARCIB.ALIAS.TABLE = ''
    CALL OPF(FN.AI.REDO.ARCIB.ALIAS.TABLE,F.AI.REDO.ARCIB.ALIAS.TABLE)

    FN.AI.REDO.ACCT.RESTRICT.PARAMETER = 'F.AI.REDO.ACCT.RESTRICT.PARAMETER'

    Y.VAR.EXT.CUSTOMER = ''
    Y.VAR.EXT.ACCOUNTS=''
    FN.CATEGORY = "F.CATEGORY"
    F.CATEGORY = ''

    FN.CUS.BEN.LIST = 'F.CUS.BEN.LIST'
    F.CUS.BEN.LIST  = ''
    CALL OPF(FN.CUS.BEN.LIST,F.CUS.BEN.LIST)

    Y.FIELD.COUNT = ''
    R.ACCT.REC = ''
    LOAN.FLG = ''
    DEP.FLG = ''
    Y.FLAG = ''
    R.AZ.REC = ''
    R.ACC = ''
    LREF.POS = ''
    LREF.APP='ACCOUNT':@FM:'BENEFICIARY'
    LREF.FIELDS='L.AC.STATUS1':@VM:'L.AC.AV.BAL':@VM:'L.AC.NOTIFY.1':@VM:'L.AC.STATUS2':@FM:'L.BEN.CUST.NAME':@VM:'L.BEN.CEDULA':@VM:'L.BEN.BANK':@VM:':L.BEN.OWN.ACCT':@VM:'L.BEN.PROD.TYPE':@VM:'L.BEN.EMAIL':@VM:'L.BEN.ACH.ARCIB':@VM:'L.BEN.ACCOUNT'
    CALL MULTI.GET.LOC.REF(LREF.APP,LREF.FIELDS,LREF.POS)
    ACCT.STATUS.POS  = LREF.POS<1,1>
    ACCT.OUT.BAL.POS = LREF.POS<1,2>
    NOTIFY.POS       = LREF.POS<1,3>
    ACCT.STATUS2.POS = LREF.POS<1,4>
    POS.L.BEN.CUST.NAME = LREF.POS<2,1>
    POS.L.BEN.CEDULA    = LREF.POS<2,2>
    POS.L.BEN.BANK      = LREF.POS<2,3>
    POS.L.BEN.OWN.ACCT  = LREF.POS<2,4>
    POS.L.BEN.PROD.TYPE = LREF.POS<2,5>
    POS.L.BEN.EMAIL = LREF.POS<2,6>
    POS.L.BEN.ACH.ARCIB = LREF.POS<2,7>
    POS.L.BEN.ACCOUNT  = LREF.POS<2,8>
    CUSTOMER.ID = System.getVariable('EXT.SMS.CUSTOMERS')
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN ;*R22 Auto conversion - START
        CUSTOMER.ID = ""
    END					;*R22 Auto conversion - END

    Y.CUSTOMER.BEN.ID = CUSTOMER.ID:'-OTHER'

RETURN
******************
FORM.ACCT.ARRAY:
*****************

    CALL CACHE.READ(FN.AI.REDO.ACCT.RESTRICT.PARAMETER,'SYSTEM',R.AI.REDO.ACCT.RESTRICT.PARAMETER,RES.ERR)
    Y.RESTRICT.ACCT.TYPE = R.AI.REDO.ACCT.RESTRICT.PARAMETER<AI.RES.PARAM.RESTRICT.ACCT.TYPE>
    CHANGE @VM TO @FM IN Y.RESTRICT.ACCT.TYPE

    CALL F.READ(FN.CUS.BEN.LIST,Y.CUSTOMER.BEN.ID,R.CUS.BEN.LIST,F.CUS.BEN.LIST,CUS.BEN.LIST.ERR)

    LOOP
        REMOVE BEN.ID FROM R.CUS.BEN.LIST SETTING BEN.POS
    WHILE BEN.ID:BEN.POS

        Y.L.BEN.CUST.NAME='' ; Y.L.BEN.CEDULA =''; Y.L.BEN.BANK= ''; Y.L.BEN.OWN.ACCT = ''; Y.L.BEN.EMAIL= ''; Y.L.BEN.ACH.ARCIB= ''; Y.L.BEN.ACCOUNT = ''
        Y.NICKNAME = ''; Y.BEN.PROD.TYPE = ''; ACCT.ID = ''
        BEN.PART.ID = FIELD(BEN.ID,'*',2)
        CALL CACHE.READ(FN.BENEFICIARY, BEN.PART.ID, R.BENEFICIARY, BENEFICIARY.ERR) ;*R22 Auto conversion
        IF R.BENEFICIARY THEN
            Y.BEN.PROD.TYPE = R.BENEFICIARY<ARC.BEN.LOCAL.REF,POS.L.BEN.PROD.TYPE>
            ACCT.ID = R.BENEFICIARY<ARC.BEN.BEN.ACCT.NO>
            Y.NICKNAME = R.BENEFICIARY<ARC.BEN.NICKNAME>
            Y.TRANSACTION.TYPE = R.BENEFICIARY<ARC.BEN.TRANSACTION.TYPE>
            Y.L.BEN.CUST.NAME = R.BENEFICIARY<ARC.BEN.LOCAL.REF,POS.L.BEN.CUST.NAME>
            Y.L.BEN.CEDULA    = R.BENEFICIARY<ARC.BEN.LOCAL.REF,POS.L.BEN.CEDULA>
            Y.L.BEN.BANK      = R.BENEFICIARY<ARC.BEN.LOCAL.REF,POS.L.BEN.BANK>
            Y.L.BEN.OWN.ACCT  = R.BENEFICIARY<ARC.BEN.LOCAL.REF,POS.L.BEN.OWN.ACCT>
            Y.L.BEN.EMAIL     = R.BENEFICIARY<ARC.BEN.LOCAL.REF,POS.L.BEN.EMAIL>
            Y.L.BEN.ACH.ARCIB = R.BENEFICIARY<ARC.BEN.LOCAL.REF,POS.L.BEN.ACH.ARCIB>
            Y.L.BEN.ACCOUNT   = R.BENEFICIARY<ARC.BEN.LOCAL.REF,POS.L.BEN.ACCOUNT>
            FIN.ARR<-1> = BEN.PART.ID:"*":Y.L.BEN.CUST.NAME:"*":ACCT.ID:"*":Y.BEN.PROD.TYPE:"*":Y.NICKNAME:"*":Y.L.BEN.EMAIL:"*":Y.L.BEN.ACH.ARCIB:"*":Y.L.BEN.ACCOUNT
        END
    REPEAT
RETURN
*-----------------------------------------------------------------------------
END
*---------------------------*END OF SUBROUTINE*-------------------------------
