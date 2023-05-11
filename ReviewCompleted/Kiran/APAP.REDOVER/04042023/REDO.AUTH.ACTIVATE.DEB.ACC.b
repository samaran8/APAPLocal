* @ValidationCode : Mjo4MzQyOTM2MzU6Q3AxMjUyOjE2ODA2MTIzMDEzNDk6c2FtYXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 04 Apr 2023 18:15:01
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : samar
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.AUTH.ACTIVATE.DEB.ACC
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :Sudharsanan S
*Program   Name    :REDO.AUTH.ACTIVATE.DEB.ACC
*---------------------------------------------------------------------------------

*DESCRIPTION       :Routine changes L.AC.STATUS1 to ACTIVE when AZ REPAY/MULTIPLE SOURCE OF CUSTOMER account
*                   account which is not active  and transaction is authorized
*LINKED WITH       : AZ OPENING VERSIONS
* ----------------------------------------------------------------------------------
*Modification History
*DATE                    WHO                          REFERENCE                    DESCRIPITION
*04-04-2023           Conversion Tool          R22 Auto Code conversion      FM TO @FM,VM TO @VM , SM TO @SM, ++ TO +=1
*04-04-2023            Samaran T                Manual R22 Code Conversion    No Changes
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.ACCOUNT
    $INSERT I_F.REDO.CUST.PRD.LIST
    $INSERT I_REDO.TELLER.COMMON

    GOSUB INIT
    GOSUB FILEOPEN
    GOSUB PROCESS
RETURN
*----
INIT:
*----
    FN.ACCOUNT='F.ACCOUNT'
    F.ACCOUNT=''
    FN.CUST.PRD.LIST='F.REDO.CUST.PRD.LIST'
    F.CUST.PRD.LIST =''
    LREF.APP='ACCOUNT':@FM:'AZ.ACCOUNT' ;*R22 AUTO CODE CONVERSION
    LREF.FIELD='L.AC.STATUS1':@VM:'L.AC.TRAN.AVAIL':@FM:'L.AZ.METHOD.PAY':@VM:'L.AZ.AMOUNT':@VM:'L.AZ.DEBIT.ACC' ;*R22 AUTO CODE CONVERSION

RETURN
*--------
FILEOPEN:
*--------
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
    CALL OPF(FN.CUST.PRD.LIST,F.CUST.PRD.LIST)

    CALL MULTI.GET.LOC.REF(LREF.APP,LREF.FIELD,LREF.POS)
    L.AC.STAT.POS = LREF.POS<1,1>
    L.TRAN.AVAIL.POS = LREF.POS<1,2>
    L.AZ.METHOD.PAY.POS = LREF.POS<2,1>
    L.AZ.AMOUNT.POS = LREF.POS<2,2>
    L.AZ.DEBIT.ACC.POS = LREF.POS<2,3>

RETURN
*---------
PROCESS:
*---------
    VAR.ACCOUNT=R.NEW(AZ.REPAY.ACCOUNT)
    IF NOT(VAR.ACCOUNT) THEN
        GOSUB CHECK.ACCOUNT
    END ELSE
        GOSUB ACTIVATE
    END
RETURN
*-------------
CHECK.ACCOUNT:
*-------------
    VAR.METHOD.PAY = R.NEW(AZ.LOCAL.REF)<1,L.AZ.METHOD.PAY.POS>
    METHOD.COUNT = DCOUNT(VAR.METHOD.PAY,@SM) ; CNT = 1  ;*R22 AUTO CODE CONVERSION
    LOOP
    WHILE CNT LE METHOD.COUNT
        CHANGE @SM TO @FM IN VAR.METHOD.PAY  ;*R22 AUTO CODE CONVERSION
        POS = ''
        VAR.DEB.ACC = R.NEW(AZ.LOCAL.REF)<1,L.AZ.DEBIT.ACC.POS>
        CHANGE @SM TO @FM IN VAR.DEB.ACC  ;*R22 AUTO CODE CONVERSION
        IF (VAR.METHOD.PAY<CNT> EQ 'FROM.CUST.ACC' OR VAR.METHOD.PAY<CNT> EQ 'FROM.INT.ACC' OR VAR.METHOD.PAY<CNT> EQ 'FROM.NOST.ACC') AND (VAR.DEB.ACC<CNT> NE '') THEN
            VAR.ACCOUNT = VAR.DEB.ACC<CNT>
            GOSUB ACTIVATE
        END
        CNT += 1  ;*R22 AUTO CODE CONVERSION
    REPEAT
*-------
ACTIVATE:
*--------
    R.ACCOUNT = ''
    CALL F.READ(FN.ACCOUNT,VAR.ACCOUNT,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
    Y.CUSTOMER = R.ACCOUNT<AC.CUSTOMER>
    IF (R.ACCOUNT<AC.LOCAL.REF,L.AC.STAT.POS> NE 'ACTIVE') AND (Y.CUSTOMER NE '') THEN
        R.ACCOUNT.OFS<AC.LOCAL.REF,L.AC.STAT.POS>='ACTIVE'
        Y.CUSTOMER.ID=R.ACCOUNT<AC.CUSTOMER>
        R.ACCOUNT.OFS<AC.WAIVE.LEDGER.FEE> = 'NULL'   ;*Initialising 'NULL' instead of given '' for OFS message
        CUST.JOIN='CUSTOMER'
        GOSUB UPD.PRD.LIST
        GOSUB PRD.UPD.JOIN
        GOSUB UPDATE.ACC
    END
RETURN
*------------
UPD.PRD.LIST:
*------------
    R.CUST.PRD.LIST = ''
    CALL F.READ(FN.CUST.PRD.LIST,Y.CUSTOMER.ID,R.CUST.PRD.LIST,F.CUST.PRD.LIST,CUS.ERR)
    Y.PRD.LIST=R.CUST.PRD.LIST<PRD.PRODUCT.ID>
    CHANGE @VM TO @FM IN Y.PRD.LIST  ;*R22 AUTO CODE CONVERSION
    LOCATE VAR.ACCOUNT IN Y.PRD.LIST SETTING PRD.POS THEN
    END
    R.CUST.PRD.LIST<PRD.PRD.STATUS,PRD.POS> ='ACTIVE'
    R.CUST.PRD.LIST<PRD.TYPE.OF.CUST,PRD.POS>=CUST.JOIN
    R.CUST.PRD.LIST<PRD.DATE,PRD.POS>=TODAY
    R.CUST.PRD.LIST<PRD.PROCESS.DATE> = TODAY
    CALL F.WRITE(FN.CUST.PRD.LIST,Y.CUSTOMER.ID,R.CUST.PRD.LIST)
RETURN
*------------
PRD.UPD.JOIN:
*-------------
    IF R.ACCOUNT<AC.JOINT.HOLDER> NE '' THEN
        Y.CUSTOMER.ID=R.ACCOUNT<AC.JOINT.HOLDER>
        CUST.JOIN='JOINT.HOLDER'
        GOSUB UPD.PRD.LIST
    END
RETURN
*--------------
UPDATE.ACC:
*-------------
    APP.NAME = 'ACCOUNT'
    OFSFUNCT = 'I'
    PROCESS  = 'PROCESS'
    OFSVERSION = 'ACCOUNT,OFS.UPD'
    GTSMODE = ''
    NO.OF.AUTH = '0'
    TRANSACTION.ID = VAR.ACCOUNT
    OFSRECORD = ''

    OFS.MSG.ID =''
    OFS.SOURCE.ID = 'REDO.OFS.ACC.UPDATE'
    OFS.ERR = ''

    CALL OFS.BUILD.RECORD(APP.NAME,OFSFUNCT,PROCESS,OFSVERSION,GTSMODE,NO.OF.AUTH,TRANSACTION.ID,R.ACCOUNT.OFS,OFSRECORD)
    CALL OFS.POST.MESSAGE(OFSRECORD,OFS.MSG.ID,OFS.SOURCE.ID,OFS.ERR)
RETURN
*----------------------
END
