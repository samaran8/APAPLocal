* @ValidationCode : MjoxODY3MTQwOTgzOkNwMTI1MjoxNjgyNDEyMzQxMTIyOkhhcmlzaHZpa3JhbUM6LTE6LTE6MDoxOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 25 Apr 2023 14:15:41
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.V.AUTH.VERIFY.GAR.STAFF

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
*  DATE          NAME                    Refernce                        DESCRIPTION
*10-04-2023     Conversion Tool        R22 Auto Code conversion          VM TO @VM,++ TO +=1
*10-04-2023      Samaran T             R22 Manual Code Conversion        No Changes
 
*------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_F.ACCOUNT
    $INSERT I_F.TELLER
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.DATA.CAPTURE
    $INSERT I_F.REDO.EMPLOYEE.ACCOUNTS
    $INSERT I_F.REDO.UNAUTH.STAFF.LOG
    $INSERT I_F.APAP.H.GARNISH.DETAILS
    $INSERT I_F.AC.LOCKED.EVENTS
    $INSERT I_GTS.COMMON

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

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)


    FN.REDO.UNAUTH.STAFF.LOG = 'F.REDO.UNAUTH.STAFF.LOG'
    F.REDO.UNAUTH.STAFF.LOG = ''
    CALL OPF(FN.REDO.UNAUTH.STAFF.LOG,F.REDO.UNAUTH.STAFF.LOG)


    FN.REDO.CUST.EMPLOYEE='F.REDO.CUST.EMPLOYEE'
    F.REDO.CUST.EMPLOYEE=''
    CALL OPF(FN.REDO.CUST.EMPLOYEE,F.REDO.CUST.EMPLOYEE)

RETURN
*-----------
PROCESS:
*-----------
* Process are carried out for various applications
*---------------------------------

    GOSUB GET.LOGGEDIN.CUSTOMER
    BEGIN CASE
        CASE APPLICATION EQ "APAP.H.GARNISH.DETAILS"
            GOSUB AHG.PROCESS
    END CASE

RETURN
*-----------
ALE.PROCESS:
*-----------
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


AHG.PROCESS:
*-----------
    Y.ACCT.LIST=R.NEW(APAP.GAR.ACCOUNT.NO)
    Y.ACCT.CNT =1
    Y.ACCT.SIZE=DCOUNT(Y.ACCT.LIST,@VM)
    LOOP
        Y.ACCT.NO=Y.ACCT.LIST<1,Y.ACCT.CNT>
        GOSUB ALE.PROCESS
    WHILE Y.ACCT.CNT LE Y.ACCT.SIZE
        Y.ACCT.CNT += 1
    REPEAT
RETURN


*-----------
SEND.ERROR:
*-----------
* Error message is defined here
*--------------------------------
    Y.USER.ID = R.REDO.EMPLOYEE.ACCOUNTS<REDO.EMP.USER.ID>
    IF Y.USER.ID EQ OPERATOR THEN
        GOSUB UPDATE.LOG
        E= 'EB-USER.NOT.ALLOW'
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
        END

        LOCATE Y.CUS.ID IN Y.EMP.REL.CUS<1,1> SETTING REL.POS THEN

            CURR.NO = DCOUNT(OVERRIDE.FIELD.VALUE,@VM) + 1
            TEXT = 'USER.REL.TO.EMP'
            CALL STORE.OVERRIDE(CURR.NO)

        END

        Y.VAR += 1
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
    CALL F.READ(FN.REDO.CUST.EMPLOYEE,OPERATOR,R.REDO.CUST.EMPLOYEE,F.REDO.CUST.EMPLOYEE,ERR)
    Y.EMP.CUS.ID = R.REDO.CUST.EMPLOYEE
    CALL F.READ(FN.REDO.EMPLOYEE.ACCOUNTS,Y.EMP.CUS.ID,R.EMPLOYEE.RECORD,F.REDO.EMPLOYEE.ACCOUNTS,EMP.ERR)
    Y.EMP.REL.CUS = R.EMPLOYEE.RECORD<REDO.EMP.REL.CUSTOMER>
    Y.EMP.ACCOUNT = R.EMPLOYEE.RECORD<REDO.EMP.ACCOUNT>
RETURN
***********
PROG.END:
***********
END
