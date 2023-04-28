* @ValidationCode : MjotMTc4MjA5NDE1OTpDcDEyNTI6MTY4MjY5MDEwNjUwMTpzYW1hcjotMTotMTowOjE6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 28 Apr 2023 19:25:06
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : samar
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
*-----------------------------------------------------------------------------
* <Rating>-238</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE REDO.V.INP.VERIFY.STAFF

****************************************************************
*-------------------------------------------------------------------------
* Company Name  : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By  : RAJA SAKTHIVEL K P
* Program Name  : REDO.V.INP.VERIFY.STAFF
*-------------------------------------------------------------------------
* Description: This routine is a INPUT routine attached to
* FT,UNAUTH.STAFF.LOG,DATA.CAPTURE,UNAUTH.STAFF.LOG,TELLER,UNAUTH.STAFF.LOG
*----------------------------------------------------------
* Linked with: VERSION.CONTROL of FT,TELLER,DATA.CAPTURE as validation routine
* In parameter : None
* out parameter : None
*------------------------------------------------------------------------
* MODIFICATION HISTORY
*--------------------------------------------
*   DATE    NAME                 Refernce                             DESCRIPTION
* 02-03-10  Raja sakthivel    ODR-2009-10-0532                     Initial Creation
* 01-10-11  Prajeesh           CR.21                               Modify the code
* 25-10-11  Sudharsanan S      CR.18                               Change the condition for AZ.ACCOUNT Process
* 26-03-13  Sivakumar.K        PACS00253470                        If application is not a REDO.CREATE.ARRANGEMENT Then "USER.REL.TO.EMP" should not throw.
* 03-09-15  Maheswaran J       PACS00479725                        Routine skipped when it triggering on COB.
*------------------------------------------------------------------------
*Modification History
*DATE                WHO                         REFERENCE                DESCRIPTION
*19-04-2023       Conversion Tool        R22 Auto Code conversion          No Changes
*19-04-2023       Samaran T               R22 Manual Code Conversion       VM TO @VM
*-------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_F.ACCOUNT
    $INSERT I_F.TELLER
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.DATA.CAPTURE
    $INSERT I_F.REDO.EMPLOYEE.ACCOUNTS
    $INSERT I_F.REDO.UNAUTH.STAFF.LOG
    $INSERT I_F.ACCOUNT.CLOSURE
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.ACCT.CAPITALISATION
    $INSERT I_F.AC.LOCKED.EVENTS
    $INSERT I_F.ACCOUNT.CREDIT.INT
    $INSERT I_F.ACCOUNT.DEBIT.INT
    $INSERT I_F.ACCOUNT.STATEMENT
    $INSERT I_F.AC.CHARGE.REQUEST
    $INSERT I_F.IC.CHARGE
    $INSERT I_F.RELATION.CUSTOMER
    $INSERT I_AA.LOCAL.COMMON
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.AA.CUSTOMER
    $INSERT I_F.T24.FUND.SERVICES
    $INSERT I_F.REDO.CREATE.ARRANGEMENT

    IF RUNNING.UNDER.BATCH THEN ;*PACS00479725 The changes made for COB performance
        RETURN
    END

    GOSUB OPENFILES
    GOSUB PROCESS
    GOSUB PROG.END
RETURN
*------------
OPENFILES:
*------------

    FN.REDO.EMPLOYEE.ACCOUNTS = 'F.REDO.EMPLOYEE.ACCOUNTS'
    F.REDO.EMPLOYEE.ACCOUNTS = ''
    CALL OPF(FN.REDO.EMPLOYEE.ACCOUNTS,F.REDO.EMPLOYEE.ACCOUNTS)

    FN.AA.ARRANGEMENT='F.AA.ARRANGEMENT'
    F.AA.ARRANGEMENT =''
    CALL OPF (FN.AA.ARRANGEMENT,F.AA.ARRANGEMENT)

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    FN.ACCT.CAPITALISATION='F.ACCT.CAPITALISATION'
    F.ACCT.CAPITALISATION=''
    CALL OPF(FN.ACCT.CAPITALISATION,F.ACCT.CAPITALISATION)

    FN.REDO.UNAUTH.STAFF.LOG = 'F.REDO.UNAUTH.STAFF.LOG'
    F.REDO.UNAUTH.STAFF.LOG = ''
    CALL OPF(FN.REDO.UNAUTH.STAFF.LOG,F.REDO.UNAUTH.STAFF.LOG)

    FN.ACCOUNT.CLOSURE = 'F.ACCOUNT.CLOSURE'
    F.ACCOUNT.CLOSURE = ''
    CALL OPF(FN.ACCOUNT.CLOSURE,F.ACCOUNT.CLOSURE)

    FN.AZ.ACCOUNT='F.AZ.ACCOUNT'
    F.AZ.ACCOUNT=''
    CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)

    FN.ACCOUNT.STATEMENT='F.ACCOUNT.STATEMENT'
    F.ACCOUNT.STATEMENT=''
    CALL OPF(FN.ACCOUNT.STATEMENT,F.ACCOUNT.STATEMENT)

    FN.AC.LOCKED.EVENTS='F.AC.LOCKED.EVENTS'
    F.AC.LOCKED.EVENTS=''
    CALL OPF(FN.AC.LOCKED.EVENTS,F.AC.LOCKED.EVENTS)

    FN.ACCOUNT.CREDIT.INT = 'F.ACCOUNT.CREDIT.INT'
    F.ACCOUNT.CREDIT.INT  = ''
    CALL OPF(FN.ACCOUNT.CREDIT.INT,F.ACCOUNT.CREDIT.INT)

    FN.ACCOUNT.DEBIT.INT = 'F.ACCOUNT.DEBIT.INT'
    F.ACCOUNT.DEBIT.INT  = ''
    CALL OPF(FN.ACCOUNT.DEBIT.INT ,F.ACCOUNT.DEBIT.INT)

    FN.AC.CHARGE.REQUEST  = 'F.AC.CHARGE.REQUEST'
    F.AC.CHARGE.REQUEST   = ''
    CALL OPF(FN.AC.CHARGE.REQUEST,F.AC.CHARGE.REQUEST)

    FN.IC.CHARGE = 'F.IC.CHARGE'
    F.IC.CHARGE  = ''
    CALL OPF(FN.IC.CHARGE,F.IC.CHARGE)

    FN.RELATION.CUSTOMER = 'F.RELATION.CUSTOMER'
    F.RELATION.CUSTOMER = ''
    CALL OPF(FN.RELATION.CUSTOMER,F.RELATION.CUSTOMER)

    FN.CUSTOMER.ACCOUNT = 'F.CUSTOMER.ACCOUNT'
    F.CUSTOMER.ACCOUNT = ''
    CALL OPF(FN.CUSTOMER.ACCOUNT,F.CUSTOMER.ACCOUNT)


RETURN
*-----------
PROCESS:
*-----------
* Process are carried out for various applications
*---------------------------------

    GOSUB GET.LOGGEDIN.CUSTOMER

    BEGIN CASE
        CASE APPLICATION EQ "TELLER"
            GOSUB TELLER.PROCESS
        CASE APPLICATION EQ "FUNDS.TRANSFER"
            GOSUB FT.PROCESS
        CASE APPLICATION EQ "T24.FUND.SERVICES"
            GOSUB TFS.PROCESS
        CASE APPLICATION EQ "DATA.CAPTURE"
            GOSUB DC.PROCESS
        CASE APPLICATION EQ "ACCOUNT"
            GOSUB AC.PROCESS
        CASE APPLICATION EQ "AZ.ACCOUNT"
            GOSUB AZ.PROCESS
        CASE APPLICATION EQ "ACCOUNT.CLOSURE"
            GOSUB ACC.CLOSURE.PROCESS
        CASE APPLICATION EQ "ACCT.CAPITALISATION"
            GOSUB ACCT.CAPITALISATION.PROCESS
        CASE APPLICATION EQ "AC.LOCKED.EVENTS"
            GOSUB AC.LOCKED.EVENTS.PROCESS
        CASE APPLICATION EQ "ACCOUNT.STATEMENT"
            GOSUB ACCOUNT.STATEMENT.PROCESS
        CASE APPLICATION EQ "ACCOUNT.CREDIT.INT"
            GOSUB ACCOUNT.CREDIT.INT.PROCESS
        CASE APPLICATION EQ "ACCOUNT.DEBIT.INT"
            GOSUB ACCOUNT.DEBIT.INT.PROCESS
        CASE APPLICATION EQ "AC.CHARGE.REQUEST"
            GOSUB AC.CHARGE.REQUEST.PROCESS
        CASE APPLICATION EQ "IC.CHARGE"
            GOSUB IC.CHARGE.PROCESS
        CASE APPLICATION EQ "AA.ARRANGEMENT.ACTIVITY"
            GOSUB AA.PROCESS
        CASE APPLICATION EQ "AA.ARR.CUSTOMER"
            GOSUB AA.API.PROCESS
        CASE APPLICATION EQ "REDO.CREATE.ARRANGEMENT"
            GOSUB RCA.PROCESS
    END CASE

RETURN

RCA.PROCESS:


    Y.CUS.DC=R.NEW(REDO.FC.CUSTOMER)

    IF APPLICATION NE 'REDO.CREATE.ARRANGEMENT' THEN
        IF Y.CUS.DC THEN
            LOCATE Y.CUS.DC IN Y.EMP.REL.CUS<1,1> SETTING EMP.POS THEN
                OVERRIDE.FIELD.VALUE = R.NEW(FT.OVERRIDE)
                CURR.NO = DCOUNT(OVERRIDE.FIELD.VALUE,@VM) + 1
                TEXT = 'USER.REL.TO.EMP'
                CALL STORE.OVERRIDE(CURR.NO)
            END
        END
    END

    CALL F.READ(FN.REDO.EMPLOYEE.ACCOUNTS,Y.CUS.DC,R.REDO.EMPLOYEE.ACCOUNTS,F.REDO.EMPLOYEE.ACCOUNTS,ERR.REDO.EMPLOYEE.ACCOUNTS)
    CALL F.READ(FN.CUSTOMER,Y.CUS.DC,R.REL.CUSTOMER,F.CUSTOMER,CUS.ERR)
    AF = REDO.FC.CUSTOMER
    GOSUB SEND.ERROR

    Y.CUS.VAL.JOIN=R.NEW(REDO.FC.OTHER.PARTY)

*    CALL F.READ(FN.ACCOUNT,Y.ACC.DC,R.ACCOUNT,F.ACCOUNT,ERR.ACCOUNT)
*    Y.CUS.VAL = R.ACCOUNT<AC.JOINT.HOLDER>

    Y.CUS.VAL  =R.REL.CUSTOMER<EB.CUS.REL.CUSTOMER>
    OVERRIDE.FIELD.VALUE = R.NEW(REDO.FC.OVERRIDE)

    GOSUB CUST.CHECK

    CALL F.READ(FN.REDO.EMPLOYEE.ACCOUNTS,Y.CUS.VAL.JOIN,R.REDO.EMPLOYEE.ACCOUNTS,F.REDO.EMPLOYEE.ACCOUNTS,ERR.REDO.EMPLOYEE.ACCOUNTS)
    CALL F.READ(FN.CUSTOMER,Y.CUS.VAL.JOIN,R.REL.CUSTOMER,F.CUSTOMER,CUS.ERR)
    AF = REDO.FC.OTHER.PARTY
    GOSUB SEND.ERROR

    Y.CUS.VAL = R.REL.CUSTOMER<EB.CUS.REL.CUSTOMER>
    OVERRIDE.FIELD.VALUE = R.NEW(REDO.FC.OVERRIDE)
    GOSUB CUST.CHECK

RETURN

*--------------------------------------------------------------
CHECK.ACCOUNT:
*--------------------------------------------------------------
    CURR.NO = ''

    CHANGE @VM TO @FM IN Y.EMP.REL.CUS
    CHANGE @VM TO @FM IN Y.EMP.ACCOUNT

    CALL F.READ(FN.ACCOUNT,Y.ACC.NO,R.ACC.CUST,F.ACCOUNT,ACC.ERR)
    Y.ACC.CUS.ID = R.ACC.CUST<AC.CUSTOMER>
    IF Y.ACC.CUS.ID EQ '' THEN
        RETURN
    END
    CALL F.READ(FN.CUSTOMER,Y.ACC.CUS.ID,R.CUST,F.CUSTOMER,CUS.ERR)
    Y.FAX = R.CUST<EB.CUS.FAX.1,1>
    IF Y.FAX EQ OPERATOR THEN
        GOSUB UPDATE.LOG
        ETEXT = 'EB-USER.NOT.ALLOW'
        CALL STORE.END.ERROR
        GOSUB PROG.END
    END
    IF Y.EMP.CUS.ID THEN
        LOCATE Y.EMP.CUS.ID IN R.ACC.CUST<AC.JOINT.HOLDER,1> SETTING POS.JOIN THEN
            GOSUB UPDATE.LOG
            ETEXT = 'EB-USER.NOT.ALLOW'
            CALL STORE.END.ERROR
            GOSUB PROG.END
        END
    END

    LOCATE Y.ACC.CUS.ID IN Y.EMP.REL.CUS SETTING POS.REL THEN
        TEXT = 'USER.REL.TO.EMP'
        CALL STORE.OVERRIDE(CURR.NO)
    END
    Y.JOINT.HOLDERS =  R.ACC.CUST<AC.JOINT.HOLDER>
    IF Y.JOINT.HOLDERS THEN
        GOSUB CHECK.JOINT.HOLDER
    END
RETURN
*--------------------------------------------------------------
CHECK.JOINT.HOLDER:
*--------------------------------------------------------------
    Y.JOINT.CNT  = DCOUNT(Y.JOINT.HOLDERS,@VM)
    Y.VAR2 = 1
    LOOP
    WHILE Y.VAR2 LE Y.JOINT.CNT
        Y.JOIN.CUS.ID = Y.JOINT.HOLDERS<1,Y.VAR2>
        LOCATE Y.JOIN.CUS.ID IN Y.EMP.REL.CUS SETTING POS.JOIN1 THEN
            TEXT = 'USER.REL.TO.EMP'
            CALL STORE.OVERRIDE(CURR.NO)
        END
        Y.VAR2++
    REPEAT
RETURN
*---------------
TELLER.PROCESS:
*---------------
* Teller process is carried on here
*-------------------------------------
    Y.ACC.NO = R.NEW(TT.TE.ACCOUNT.1)
    AF = TT.TE.ACCOUNT.1
    IF Y.ACC.NO THEN
        GOSUB CHECK.ACCOUNT
    END

    Y.ACC.NO = R.NEW(TT.TE.ACCOUNT.2)
    AF = TT.TE.ACCOUNT.2
    IF Y.ACC.NO THEN
        GOSUB CHECK.ACCOUNT
    END

RETURN

*-------------
FT.PROCESS:
*-------------------------------
* FT process is carried on here
*-------------------------------
    Y.ACC.NO = R.NEW(FT.DEBIT.ACCT.NO)
    AF = FT.DEBIT.ACCT.NO
    IF Y.ACC.NO THEN
        GOSUB CHECK.ACCOUNT
    END

    Y.ACC.NO = R.NEW(FT.CREDIT.ACCT.NO)
    AF = FT.CREDIT.ACCT.NO
    IF Y.ACC.NO THEN
        GOSUB CHECK.ACCOUNT
    END

RETURN
*-------------------------------
TFS.PROCESS:
*-------------------------------
    Y.ACC.NO = R.NEW(TFS.PRIMARY.ACCOUNT)
    AF = TFS.PRIMARY.ACCOUNT
    IF Y.ACC.NO THEN
        GOSUB CHECK.ACCOUNT
    END

RETURN
*----------------------------
ACCT.CAPITALISATION.PROCESS:
*------------------------------
    Y.ACCT.NO = ID.NEW
    CALL F.READ(FN.ACCOUNT,Y.ACCT.NO,R.ACCOUNT,F.ACCOUNT,ERR.ACCOUNT)
    Y.DBT.CUS = R.ACCOUNT<AC.CUSTOMER>
    Y.CUS.VAL = R.ACCOUNT<AC.JOINT.HOLDER>
    IF Y.DBT.CUS THEN
        LOCATE Y.DBT.CUS IN Y.EMP.REL.CUS<1,1> SETTING EMP.POS THEN
*           OVERRIDE.FIELD.VALUE = R.NEW(FT.OVERRIDE)
*           CURR.NO = DCOUNT(OVERRIDE.FIELD.VALUE,VM) + 1
            CURR.NO=1
            TEXT = 'USER.REL.TO.EMP'
            CALL STORE.OVERRIDE(CURR.NO)
        END
    END
    CALL F.READ(FN.REDO.EMPLOYEE.ACCOUNTS,Y.CUS.VAL,R.REDO.EMPLOYEE.ACCOUNTS,F.REDO.EMPLOYEE.ACCOUNTS,ERR.REDO.EMPLOYEE.ACCOUNTS)
    CALL F.READ(FN.CUSTOMER,Y.CUS.VAL,R.REL.CUSTOMER,F.CUSTOMER,CUS.ERR)
    AF = '' ; AV = '' ; AS = '' ;****
    GOSUB SEND.ERROR
    CALL F.READ(FN.REDO.EMPLOYEE.ACCOUNTS,Y.DBT.CUS,R.REDO.EMPLOYEE.ACCOUNTS,F.REDO.EMPLOYEE.ACCOUNTS,ERR.REDO.EMPLOYEE.ACCOUNTS)
    CALL F.READ(FN.CUSTOMER,Y.DBT.CUS,R.REL.CUSTOMER,F.CUSTOMER,CUS.ERR)
    AF = '' ; AV = '' ; AS = ''
    GOSUB SEND.ERROR
*  OVERRIDE.FIELD.VALUE = R.NEW(FT.OVERRIDE)
    GOSUB CUST.CHECK
    Y.CUS.VAL = R.REL.CUSTOMER<EB.CUS.REL.CUSTOMER>
*   OVERRIDE.FIELD.VALUE = R.NEW(FT.OVERRIDE)
*    GOSUB CUST.CHECK
RETURN

*---------------------------
ACCOUNT.STATEMENT.PROCESS:
*---------------------------
    Y.ACCT.NO = ID.NEW
    CALL F.READ(FN.ACCOUNT,Y.ACCT.NO,R.ACCOUNT,F.ACCOUNT,ERR.ACCOUNT)
    Y.DBT.CUS = R.ACCOUNT<AC.CUSTOMER>
    Y.CUS.VAL = R.ACCOUNT<AC.JOINT.HOLDER>
    IF Y.DBT.CUS THEN
        LOCATE Y.DBT.CUS IN Y.EMP.REL.CUS<1,1> SETTING EMP.POS THEN
            OVERRIDE.FIELD.VALUE = R.NEW(AC.STA.OVERRIDE)
            CURR.NO = DCOUNT(OVERRIDE.FIELD.VALUE,@VM) + 1
            CURR.NO=1
            TEXT = 'USER.REL.TO.EMP'
            CALL STORE.OVERRIDE(CURR.NO)
        END
    END
    CALL F.READ(FN.REDO.EMPLOYEE.ACCOUNTS,Y.CUS.VAL,R.REDO.EMPLOYEE.ACCOUNTS,F.REDO.EMPLOYEE.ACCOUNTS,ERR.REDO.EMPLOYEE.ACCOUNTS)
    CALL F.READ(FN.CUSTOMER,Y.CUS.VAL,R.REL.CUSTOMER,F.CUSTOMER,CUS.ERR)
    AF = '' ; AV = '' ; AS = '' ;****
    GOSUB SEND.ERROR
    CALL F.READ(FN.REDO.EMPLOYEE.ACCOUNTS,Y.DBT.CUS,R.REDO.EMPLOYEE.ACCOUNTS,F.REDO.EMPLOYEE.ACCOUNTS,ERR.REDO.EMPLOYEE.ACCOUNTS)
    CALL F.READ(FN.CUSTOMER,Y.DBT.CUS,R.REL.CUSTOMER,F.CUSTOMER,CUS.ERR)
    AF = '' ; AV = '' ; AS = ''
    GOSUB SEND.ERROR
    OVERRIDE.FIELD.VALUE = R.NEW(AC.STA.OVERRIDE)
    GOSUB CUST.CHECK
    Y.CUS.VAL = R.REL.CUSTOMER<EB.CUS.REL.CUSTOMER>
    OVERRIDE.FIELD.VALUE = R.NEW(AC.STA.OVERRIDE)
    GOSUB CUST.CHECK
RETURN

*------------------------------
ACCOUNT.CREDIT.INT.PROCESS:
*------------------------------
    Y.ACCT.NO = FIELDS(ID.NEW,'-',1)
    CALL F.READ(FN.ACCOUNT,Y.ACCT.NO,R.ACCOUNT,F.ACCOUNT,ERR.ACCOUNT)
    Y.DBT.CUS = R.ACCOUNT<AC.CUSTOMER>
    Y.CUS.VAL = R.ACCOUNT<AC.JOINT.HOLDER>
    CALL F.READ(FN.REDO.EMPLOYEE.ACCOUNTS,Y.CUS.VAL,R.REDO.EMPLOYEE.ACCOUNTS,F.REDO.EMPLOYEE.ACCOUNTS,ERR.REDO.EMPLOYEE.ACCOUNTS)
    CALL F.READ(FN.CUSTOMER,Y.CUS.VAL,R.REL.CUSTOMER,F.CUSTOMER,CUS.ERR)
    AF = '' ; AV = '' ; AS = '' ;****
    GOSUB SEND.ERROR
    CALL F.READ(FN.REDO.EMPLOYEE.ACCOUNTS,Y.DBT.CUS,R.REDO.EMPLOYEE.ACCOUNTS,F.REDO.EMPLOYEE.ACCOUNTS,ERR.REDO.EMPLOYEE.ACCOUNTS)
    CALL F.READ(FN.CUSTOMER,Y.DBT.CUS,R.REL.CUSTOMER,F.CUSTOMER,CUS.ERR)
    AF = '' ; AV = '' ; AS = ''
    GOSUB SEND.ERROR
RETURN

*--------------------------
ACCOUNT.DEBIT.INT.PROCESS:
*--------------------------

    Y.ACCT.NO = FIELDS(ID.NEW,'-',1)
    CALL F.READ(FN.ACCOUNT,Y.ACCT.NO,R.ACCOUNT,F.ACCOUNT,ERR.ACCOUNT)
    Y.DBT.CUS = R.ACCOUNT<AC.CUSTOMER>
    Y.CUS.VAL = R.ACCOUNT<AC.JOINT.HOLDER>
    CALL F.READ(FN.REDO.EMPLOYEE.ACCOUNTS,Y.CUS.VAL,R.REDO.EMPLOYEE.ACCOUNTS,F.REDO.EMPLOYEE.ACCOUNTS,ERR.REDO.EMPLOYEE.ACCOUNTS)
    CALL F.READ(FN.CUSTOMER,Y.CUS.VAL,R.REL.CUSTOMER,F.CUSTOMER,CUS.ERR)
    AF = '' ; AV = '' ; AS = '' ;****
    GOSUB SEND.ERROR
    CALL F.READ(FN.REDO.EMPLOYEE.ACCOUNTS,Y.DBT.CUS,R.REDO.EMPLOYEE.ACCOUNTS,F.REDO.EMPLOYEE.ACCOUNTS,ERR.REDO.EMPLOYEE.ACCOUNTS)
    CALL F.READ(FN.CUSTOMER,Y.DBT.CUS,R.REL.CUSTOMER,F.CUSTOMER,CUS.ERR)
    AF = '' ; AV = '' ; AS = ''
    GOSUB SEND.ERROR
RETURN
*--------------------------
AC.CHARGE.REQUEST.PROCESS:
*-------------------------

    Y.ACCT.NO = R.NEW(CHG.DEBIT.ACCOUNT)
    CALL F.READ(FN.ACCOUNT,Y.ACCT.NO,R.ACCOUNT,F.ACCOUNT,ERR.ACCOUNT)
    Y.DBT.CUS = R.ACCOUNT<AC.CUSTOMER>
    Y.CUS.VAL = R.ACCOUNT<AC.JOINT.HOLDER>
    IF Y.DBT.CUS THEN
        LOCATE Y.DBT.CUS IN Y.EMP.REL.CUS<1,1> SETTING EMP.POS THEN
            OVERRIDE.FIELD.VALUE = R.NEW(AC.STA.OVERRIDE)
            CURR.NO = DCOUNT(OVERRIDE.FIELD.VALUE,@VM) + 1
            CURR.NO=1
            TEXT = 'USER.REL.TO.EMP'
            CALL STORE.OVERRIDE(CURR.NO)
        END
    END
    CALL F.READ(FN.REDO.EMPLOYEE.ACCOUNTS,Y.CUS.VAL,R.REDO.EMPLOYEE.ACCOUNTS,F.REDO.EMPLOYEE.ACCOUNTS,ERR.REDO.EMPLOYEE.ACCOUNNTS)
    CALL F.READ(FN.CUSTOMER,Y.CUS.VAL,R.REL.CUSTOMER,F.CUSTOMER,CUS.ERR)
    AF = CHG.DEBIT.ACCOUNT
    CALL F.READ(FN.REDO.EMPLOYEE.ACCOUNTS,Y.DBT.CUS,R.REDO.EMPLOYEE.ACCOUNTS,F.REDO.EMPLOYEE.ACCOUNTS,ERR.REDO.EMPLOYEE.ACCOUNTS)
    CALL F.READ(FN.CUSTOMER,Y.DBT.CUS,R.REL.CUSTOMER,F.CUSTOMER,CUS.ERR)
    AF = CHG.DEBIT.ACCOUNT
    GOSUB SEND.ERROR
    OVERRIDE.FIELD.VALUE = R.NEW(AC.STA.OVERRIDE)
    GOSUB CUST.CHECK
    Y.CUS.VAL = R.REL.CUSTOMER<EB.CUS.REL.CUSTOMER>
    OVERRIDE.FIELD.VALUE = R.NEW(AC.STA.OVERRIDE)
    GOSUB CUST.CHECK
RETURN


*-------------------
IC.CHARGE.PROCESS:
*-------------------

    Y.ACCT.NO =FIELDS(ID.NEW,'-',2)
    CALL F.READ(FN.ACCOUNT,Y.ACCT.NO,R.ACCOUNT,F.ACCOUNT,ERR.ACCOUNT)
    Y.DBT.CUS = R.ACCOUNT<AC.CUSTOMER>
    Y.CUS.VAL = R.ACCOUNT<AC.JOINT.HOLDER>
    IF Y.DBT.CUS THEN
        LOCATE Y.DBT.CUS IN Y.EMP.REL.CUS<1,1> SETTING EMP.POS THEN
            OVERRIDE.FIELD.VALUE = R.NEW(AC.STA.OVERRIDE)
            CURR.NO = DCOUNT(OVERRIDE.FIELD.VALUE,@VM) + 1
            CURR.NO=1
            TEXT = 'USER.REL.TO.EMP'
            CALL STORE.OVERRIDE(CURR.NO)
        END
    END
    CALL F.READ(FN.REDO.EMPLOYEE.ACCOUNTS,Y.CUS.VAL,R.REDO.EMPLOYEE.ACCOUNTS,F.REDO.EMPLOYEE.ACCOUNTS,ERR.REDO.EMPLOYEE.ACCOUNTS)
    CALL F.READ(FN.CUSTOMER,Y.CUS.VAL,R.REL.CUSTOMER,F.CUSTOMER,CUS.ERR)
    AF = '' ; AV = '' ; AS = '' ;****
    GOSUB SEND.ERROR
    CALL F.READ(FN.REDO.EMPLOYEE.ACCOUNTS,Y.DBT.CUS,R.REDO.EMPLOYEE.ACCOUNTS,F.REDO.EMPLOYEE.ACCOUNTS,ERR.REDO.EMPLOYEE.ACCOUNTS)
    CALL F.READ(FN.CUSTOMER,Y.DBT.CUS,R.REL.CUSTOMER,F.CUSTOMER,CUS.ERR)
    AF = '' ; AV = '' ; AS = '' ;****
    GOSUB SEND.ERROR
    OVERRIDE.FIELD.VALUE = R.NEW(AC.STA.OVERRIDE)
    GOSUB CUST.CHECK
    Y.CUS.VAL = R.REL.CUSTOMER<EB.CUS.REL.CUSTOMER>
    OVERRIDE.FIELD.VALUE = R.NEW(AC.STA.OVERRIDE)
    GOSUB CUST.CHECK
RETURN
*------------------------
AC.LOCKED.EVENTS.PROCESS:
*------------------------

    Y.ACCT.NO=R.NEW(AC.LCK.ACCOUNT.NUMBER)
    CALL F.READ(FN.ACCOUNT,Y.ACCT.NO,R.ACCOUNT,F.ACCOUNT,ERR.ACCOUNT)
    Y.DBT.CUS = R.ACCOUNT<AC.CUSTOMER>
    Y.CUS.VAL = R.ACCOUNT<AC.JOINT.HOLDER>
    IF Y.DBT.CUS THEN
        LOCATE Y.DBT.CUS IN Y.EMP.REL.CUS<1,1> SETTING EMP.POS THEN
            OVERRIDE.FIELD.VALUE = R.NEW(AC.LCK.OVERRIDE)
            CURR.NO = DCOUNT(OVERRIDE.FIELD.VALUE,@VM) + 1
            TEXT = 'USER.REL.TO.EMP'
            CALL STORE.OVERRIDE(CURR.NO)
        END
    END
    CALL F.READ(FN.REDO.EMPLOYEE.ACCOUNTS,Y.CUS.VAL,R.REDO.EMPLOYEE.ACCOUNTS,F.REDO.EMPLOYEE.ACCOUNTS,ERR.REDO.EMPLOYEE.ACCOUNTS)
    CALL F.READ(FN.CUSTOMER,Y.CUS.VAL,R.REL.CUSTOMER,F.CUSTOMER,CUS.ERR)
    AF = AC.LCK.ACCOUNT.NUMBER
    GOSUB SEND.ERROR
*    Y.USER.ID = R.REDO.EMPLOYEE.ACCOUNTS<REDO.EMP.USER.ID>
*    IF Y.USER.ID EQ OPERATOR THEN
*        GOSUB UPDATE.LOG
*        E = 'EB-USER.NOT.ALLOW'
*        CALL STORE.END.ERROR
*    END


    CALL F.READ(FN.REDO.EMPLOYEE.ACCOUNTS,Y.DBT.CUS,R.REDO.EMPLOYEE.ACCOUNTS,F.REDO.EMPLOYEE.ACCOUNTS,ERR.REDO.EMPLOYEE.ACCOUNTS)
    CALL F.READ(FN.CUSTOMER,Y.DBT.CUS,R.REL.CUSTOMER,F.CUSTOMER,CUS.ERR)
    AF = AC.LCK.ACCOUNT.NUMBER
    GOSUB SEND.ERROR
    OVERRIDE.FIELD.VALUE = R.NEW(AC.LCK.OVERRIDE)
    GOSUB CUST.CHECK
    Y.CUS.VAL = R.REL.CUSTOMER<EB.CUS.REL.CUSTOMER>
    OVERRIDE.FIELD.VALUE = R.NEW(AC.LCK.OVERRIDE)
    GOSUB CUST.CHECK
RETURN

*    Y.USER.ID = R.REDO.EMPLOYEE.ACCOUNTS<REDO.EMP.USER.ID>
*   IF Y.USER.ID EQ OPERATOR THEN
*       GOSUB UPDATE.LOG
*       E = 'EB-USER.NOT.ALLOW'
*       CALL STORE.END.ERROR
*END

*-----------
AZ.PROCESS:
*-----------
    Y.ACC.NO=ID.NEW
*CR.18-S************
    Y.DBT.AC=R.NEW(AZ.CUSTOMER)
    IF Y.DBT.AC THEN
        LOCATE Y.DBT.AC IN Y.EMP.REL.CUS<1,1> SETTING EMP.POS THEN
            OVERRIDE.FIELD.VALUE = R.NEW(AZ.OVERRIDE)
            CURR.NO = DCOUNT(OVERRIDE.FIELD.VALUE,@VM) + 1
            TEXT = 'USER.REL.TO.EMP'
            CALL STORE.OVERRIDE(CURR.NO)
        END
    END

    CALL F.READ(FN.REDO.EMPLOYEE.ACCOUNTS,Y.DBT.AC,R.REDO.EMPLOYEE.ACCOUNTS,F.REDO.EMPLOYEE.ACCOUNTS,ERR.REDO.EMPLOYEE.ACCOUNTS)
*CR.18-E***************
    CALL F.READ(FN.CUSTOMER,Y.DBT.AC,R.REL.CUSTOMER,F.CUSTOMER,CUS.ERR)
    AF = '' ; AV = '' ; AS = ''
    GOSUB SEND.ERROR
    OVERRIDE.FIELD.VALUE = R.NEW(AZ.OVERRIDE)
    GOSUB CUST.CHECK
    Y.CUS.VAL = R.REL.CUSTOMER<EB.CUS.REL.CUSTOMER>
    OVERRIDE.FIELD.VALUE = R.NEW(AZ.OVERRIDE)
    GOSUB CUST.CHECK
RETURN
*-----------
AC.PROCESS:
*-----------

    Y.DBT.ACCT.NO=ID.NEW
    Y.DBT.CUS=R.NEW(AC.CUSTOMER)
    Y.CUS.VAL = R.NEW(AC.JOINT.HOLDER)
    IF Y.DBT.CUS THEN
        LOCATE Y.DBT.CUS IN Y.EMP.REL.CUS<1,1> SETTING EMP.POS THEN
            OVERRIDE.FIELD.VALUE = R.NEW(AC.OVERRIDE)
            CURR.NO = DCOUNT(OVERRIDE.FIELD.VALUE,@VM) + 1
            TEXT = 'USER.REL.TO.EMP'
            CALL STORE.OVERRIDE(CURR.NO)
            Y.FLAG = 1
        END
    END
    CALL F.READ(FN.REDO.EMPLOYEE.ACCOUNTS,Y.CUS.VAL,R.REDO.EMPLOYEE.ACCOUNTS,F.REDO.EMPLOYEE.ACCOUNTS,ERR.REDO.EMPLOYEE.ACCOUNTS)
    CALL F.READ(FN.CUSTOMER,Y.CUS.VAL,R.REL.CUSTOMER,F.CUSTOMER,CUS.ERR)
    AF = '' ; AV = '' ; AS = ''
    GOSUB SEND.ERROR

    CALL F.READ(FN.REDO.EMPLOYEE.ACCOUNTS,Y.DBT.CUS,R.REDO.EMPLOYEE.ACCOUNTS,F.REDO.EMPLOYEE.ACCOUNTS,ERR.REDO.EMPLOYEE.ACCOUNTS)
    CALL F.READ(FN.CUSTOMER,Y.DBT.CUS,R.REL.CUSTOMER,F.CUSTOMER,CUS.ERR)
    AF = '' ; AV = '' ; AS = ''
    GOSUB SEND.ERROR
    OVERRIDE.FIELD.VALUE = R.NEW(AC.OVERRIDE)
    GOSUB CUST.CHECK
    Y.CUS.VAL = R.REL.CUSTOMER<EB.CUS.REL.CUSTOMER>
    OVERRIDE.FIELD.VALUE = R.NEW(AC.OVERRIDE)
    GOSUB CUST.CHECK
RETURN
*-------------
AA.PROCESS:
*-------------

    Y.CUS.DC=R.NEW(AA.ARR.ACT.CUSTOMER)
    Y.ACC.DC=R.NEW(AA.ARR.ACT.ARRANGEMENT)
    Y.ARR.ID=Y.ACC.DC
    CALL F.READ(FN.AA.ARRANGEMENT,Y.ACC.DC,R.AA.ARRANGEMENT,F.AA.ARRANGEMENT,Y.ARR.ERR)
    Y.ACC.DC=R.AA.ARRANGEMENT<AA.ARR.LINKED.APPL.ID>
    IF Y.CUS.DC THEN
        LOCATE Y.CUS.DC IN Y.EMP.REL.CUS<1,1> SETTING EMP.POS THEN
            OVERRIDE.FIELD.VALUE = R.NEW(FT.OVERRIDE)
            CURR.NO = DCOUNT(OVERRIDE.FIELD.VALUE,@VM) + 1
            TEXT = 'USER.REL.TO.EMP'
            CALL STORE.OVERRIDE(CURR.NO)
        END
    END

    CALL F.READ(FN.REDO.EMPLOYEE.ACCOUNTS,Y.CUS.DC,R.REDO.EMPLOYEE.ACCOUNTS,F.REDO.EMPLOYEE.ACCOUNTS,ERR.REDO.EMPLOYEE.ACCOUNTS)
    CALL F.READ(FN.CUSTOMER,Y.CUS.DC,R.REL.CUSTOMER,F.CUSTOMER,CUS.ERR)
    AF = AA.ARR.ACT.ARRANGEMENT
    GOSUB SEND.ERROR

    CALL AA.GET.ARRANGEMENT.CONDITIONS(Y.ARR.ID,'CUSTOMER', '','',RET.IDS, ACC.COND, RET.ERR)
    ACC.COND = RAISE(ACC.COND)
    Y.CUS.VAL.JOIN=ACC.COND<AA.CUS.OTHER.PARTY>

*    CALL F.READ(FN.ACCOUNT,Y.ACC.DC,R.ACCOUNT,F.ACCOUNT,ERR.ACCOUNT)
*    Y.CUS.VAL = R.ACCOUNT<AC.JOINT.HOLDER>

    Y.CUS.VAL  =R.REL.CUSTOMER<EB.CUS.REL.CUSTOMER>
    OVERRIDE.FIELD.VALUE = R.NEW(AA.ARR.ACT.OVERRIDE)

    GOSUB CUST.CHECK

    CALL F.READ(FN.REDO.EMPLOYEE.ACCOUNTS,Y.CUS.VAL.JOIN,R.REDO.EMPLOYEE.ACCOUNTS,F.REDO.EMPLOYEE.ACCOUNTS,ERR.REDO.EMPLOYEE.ACCOUNTS)
    CALL F.READ(FN.CUSTOMER,Y.CUS.VAL.JOIN,R.REL.CUSTOMER,F.CUSTOMER,CUS.ERR)
    AF = AA.ARR.ACT.ARRANGEMENT
    GOSUB SEND.ERROR

    Y.CUS.VAL = R.REL.CUSTOMER<EB.CUS.REL.CUSTOMER>
    OVERRIDE.FIELD.VALUE = R.NEW(AA.ARR.ACT.OVERRIDE)
    GOSUB CUST.CHECK
RETURN
*-------------
AA.API.PROCESS:
*-------------

    Y.CUS.VAL= R.NEW(AA.CUS.OTHER.PARTY)


    CALL F.READ(FN.REDO.EMPLOYEE.ACCOUNTS,Y.CUS.VAL,R.REDO.EMPLOYEE.ACCOUNTS,F.REDO.EMPLOYEE.ACCOUNTS,ERR.REDO.EMPLOYEE)
    CALL F.READ(FN.CUSTOMER,Y.CUS.VAL,R.REL.CUSTOMER,F.CUSTOMER,CUS.ERR)
    AF = '' ; AV = '' ; AS = ''
    GOSUB SEND.ERROR

    Y.CUS.VAL = R.REL.CUSTOMER<EB.CUS.REL.CUSTOMER>
    OVERRIDE.FIELD.VALUE = R.NEW(AA.CUS.OVERRIDE)
    GOSUB CUST.CHECK

RETURN

*------------
DC.PROCESS:
*------------------------------
* Data Capture is checked here
*------------------------------
    Y.ACC.DC = R.NEW(DC.DC.ACCOUNT.NUMBER)
    Y.CUS.DC = R.NEW(DC.DC.CUSTOMER.ID)
    IF Y.CUS.DC THEN
        LOCATE Y.CUS.DC IN Y.EMP.REL.CUS<1,1> SETTING EMP.POS THEN
            OVERRIDE.FIELD.VALUE = R.NEW(FT.OVERRIDE)
            CURR.NO = DCOUNT(OVERRIDE.FIELD.VALUE,@VM) + 1
            TEXT = 'USER.REL.TO.EMP'
            CALL STORE.OVERRIDE(CURR.NO)
        END
    END

    CALL F.READ(FN.REDO.EMPLOYEE.ACCOUNTS,Y.CUS.DC,R.REDO.EMPLOYEE.ACCOUNTS,F.REDO.EMPLOYEE.ACCOUNTS,ERR.REDO.EMPLOYEE.ACCOUNTS)
    CALL F.READ(FN.CUSTOMER,Y.CUS.DC,R.REL.CUSTOMER,F.CUSTOMER,CUS.ERR)
    AF = DC.DC.ACCOUNT.NUMBER
    GOSUB SEND.ERROR
    CALL F.READ(FN.ACCOUNT,Y.ACC.DC,R.ACCOUNT,F.ACCOUNT,ERR.ACCOUNT)
    Y.CUS.VAL = R.ACCOUNT<AC.JOINT.HOLDER>
    OVERRIDE.FIELD.VALUE = R.NEW(DC.DC.OVERRIDE)
    GOSUB CUST.CHECK
    Y.CUS.VAL = R.REL.CUSTOMER<EB.CUS.REL.CUSTOMER>
    OVERRIDE.FIELD.VALUE = R.NEW(DC.DC.OVERRIDE)
    GOSUB CUST.CHECK
RETURN

*-------------------
ACC.CLOSURE.PROCESS:
*-------------------
*Account Closure is processed here
*----------------------------------
    Y.AC.ACL.NO = ID.NEW
    CALL F.READ(FN.ACCOUNT,Y.AC.ACL.NO,R.ACCOUNT,F.ACCOUNT,ERR.ACCOUNT)
    Y.AC.ACL.CUS = R.ACCOUNT<AC.CUSTOMER>
    Y.CUS.VAL = R.ACCOUNT<AC.JOINT.HOLDER>
    IF Y.AC.ACL.CUS THEN
        LOCATE Y.AC.ACL.CUS IN Y.EMP.REL.CUS<1,1> SETTING EMP.POS THEN
            OVERRIDE.FIELD.VALUE = R.NEW(AC.ACL.OVERRIDE)
            CURR.NO = DCOUNT(OVERRIDE.FIELD.VALUE,@VM) + 1
            TEXT = 'USER.REL.TO.EMP'
            CALL STORE.OVERRIDE(CURR.NO)
        END
    END
***************CR20****

    CALL F.READ(FN.REDO.EMPLOYEE.ACCOUNTS,Y.CUS.VAL,R.REDO.EMPLOYEE.ACCOUNTS,F.REDO.EMPLOYEE.ACCOUNTS,ERR.REDO.EMPLOYEE.ACCOUNTS)
    AF = '' ; AV = '' ; AS = ''
    GOSUB SEND.ERROR
******************************************
    CALL F.READ(FN.REDO.EMPLOYEE.ACCOUNTS,Y.AC.ACL.CUS,R.REDO.EMPLOYEE.ACCOUNTS,F.REDO.EMPLOYEE.ACCOUNTS,ERR.REDO.EMPLOYEE.ACCOUNTS)
    CALL F.READ(FN.CUSTOMER,Y.AC.ACL.CUS,R.REL.CUSTOMER,F.CUSTOMER,CUS.ERR)
    AF = '' ; AV = '' ; AS = ''
    GOSUB SEND.ERROR
    CALL F.READ(FN.ACCOUNT,Y.AC.ACL.NO,R.ACCOUNT,F.ACCOUNT,ERR.ACCOUNT)
    Y.CUS.VAL = R.ACCOUNT<AC.JOINT.HOLDER>
    OVERRIDE.FIELD.VALUE = R.NEW(AC.ACL.OVERRIDE)
    GOSUB CUST.CHECK
    Y.CUS.VAL = R.REL.CUSTOMER<EB.CUS.REL.CUSTOMER>
    OVERRIDE.FIELD.VALUE = R.NEW(AC.ACL.OVERRIDE)
    GOSUB CUST.CHECK
RETURN

*-----------
SEND.ERROR:
*-----------
* Error message is defined here
*--------------------------------
    Y.USER.ID = R.REDO.EMPLOYEE.ACCOUNTS<REDO.EMP.USER.ID>
    IF Y.USER.ID EQ OPERATOR THEN
        GOSUB UPDATE.LOG
        ETEXT = 'EB-USER.NOT.ALLOW'
        CALL STORE.END.ERROR
        GOSUB PROG.END
    END
RETURN
*------------
CUST.CHECK:
*----------------------------------------------
* Reading the customer table to fetch the data
*------------------------------------------------

    Y.JOINT.CNT = DCOUNT(Y.CUS.VAL,@VM)
    Y.VAR=1
    LOOP
    WHILE Y.VAR LE Y.JOINT.CNT
        Y.CUS.ID=Y.CUS.VAL<1,Y.VAR>
        CALL F.READ(FN.CUSTOMER,Y.CUS.ID,R.CUSTOMER,F.CUSTOMER,ERR.CUSTOMER)
        Y.FAX = R.CUSTOMER<EB.CUS.FAX.1>
        IF Y.FAX EQ OPERATOR THEN
            GOSUB SEND.ERROR
*           GOSUB UPDATE.LOG
*           ETEXT = 'EB-USER.NOT.ALLOW'
*           CALL STORE.END.ERROR
*           GOSUB PROG.END
*            CURR.NO = DCOUNT(OVERRIDE.FIELD.VALUE,VM) + 1
*            TEXT = 'USER.REL.TO.EMP'
*            CALL STORE.OVERRIDE(CURR.NO)
        END

        IF APPLICATION NE 'REDO.CREATE.ARRANGEMENT' THEN
            LOCATE Y.CUS.ID IN Y.EMP.REL.CUS<1,1> SETTING REL.POS THEN
                IF NOT(Y.FLAG) THEN
                    CURR.NO = DCOUNT(OVERRIDE.FIELD.VALUE,@VM) + 1
                    TEXT = 'USER.REL.TO.EMP'
                    CALL STORE.OVERRIDE(CURR.NO)
                END
            END
        END

        Y.VAR++
    REPEAT
RETURN
*-------------
UPDATE.LOG:
*-------------
* Updating the staff log table
*-------------------------------
    CALL ALLOCATE.UNIQUE.TIME(CURRTIME)
    Y.LOG.ID = 'STAFF.':CURRTIME
    R.REDO.UNAUTH.STAFF.LOG = ''
    R.REDO.UNAUTH.STAFF.LOG<REDO.LOG.USER.ID> = OPERATOR
    R.REDO.UNAUTH.STAFF.LOG<REDO.LOG.ACTIVITY.DATE> = TODAY
    R.REDO.UNAUTH.STAFF.LOG<REDO.LOG.ACTIVITY.TIME> = OCONV(TIME(), "MTS")
    R.REDO.UNAUTH.STAFF.LOG<REDO.LOG.APPLICATION> = APPLICATION
    R.REDO.UNAUTH.STAFF.LOG<REDO.LOG.RECORD.ID> = ID.NEW

    WRITE R.REDO.UNAUTH.STAFF.LOG TO F.REDO.UNAUTH.STAFF.LOG,Y.LOG.ID ON ERROR
        RETURN
    END

RETURN
*-------------------------------
GET.LOGGEDIN.CUSTOMER:
*-------------------------------
    SEL.CMD = 'SELECT ':FN.REDO.EMPLOYEE.ACCOUNTS:' WITH USER.ID EQ ':OPERATOR
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',SEL.NOR,SEL.RET)
    Y.EMP.CUS.ID = SEL.LIST<1>  ;* There will be only one record for each employee
    CALL F.READ(FN.REDO.EMPLOYEE.ACCOUNTS,Y.EMP.CUS.ID,R.EMPLOYEE.RECORD,F.REDO.EMPLOYEE.ACCOUNTS,EMP.ERR)
*Y.EMP.REL.CUS = R.EMPLOYEE.RECORD<REDO.EMP.REL.CUSTOMER>
*Y.EMP.ACCOUNT = R.EMPLOYEE.RECORD<REDO.EMP.ACCOUNT>

    GOSUB GET.REL.CUS.AND.ACC

RETURN
*-------------------------------
GET.REL.CUS.AND.ACC:
*-------------------------------
    CALL CACHE.READ(FN.RELATION.CUSTOMER,Y.EMP.CUS.ID,R.RELATION.CUSTOMER,F.REL.CUS)
    Y.EMP.REL.CUS = R.RELATION.CUSTOMER<EB.RCU.OF.CUSTOMER>
    Y.EMP.ACCOUNT = ''
    Y.REL.CUS.CNT = DCOUNT(Y.EMP.REL.CUS,@VM)
    Y.VAR1 = 1
    LOOP
    WHILE Y.VAR1 LE Y.REL.CUS.CNT
        Y.CUS.ACC.ID = Y.EMP.REL.CUS<1,Y.VAR1>
        CALL F.READ(FN.CUSTOMER.ACCOUNT,Y.CUS.ACC.ID,R.CUSTOMER.ACCOUNT,F.CUSTOMER.ACCOUNT,ERR.CUS.ACC)
        Y.EMP.ACCOUNT<1,-1> = R.CUSTOMER.ACCOUNT
        Y.VAR1++
    REPEAT
RETURN
***********
PROG.END:
***********
END
