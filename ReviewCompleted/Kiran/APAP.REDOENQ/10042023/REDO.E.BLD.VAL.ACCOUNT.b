$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.E.BLD.VAL.ACCOUNT(ENQ.DATA)

****************************************************************
*-------------------------------------------------------------------------
* Company Name  : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By  : H GANESH
* Program Name  : REDO.E.BLD.VAL.ACCOUNT
*-------------------------------------------------------------------------
* Description: This routine is a build routine attached to all enquiries
* which have account no as selection field to restrict unauthorised access
*----------------------------------------------------------
* Linked with: All enquiries with Customer no as selection field
* In parameter : ENQ.DATA
* out parameter : None
*------------------------------------------------------------------------
* MODIFICATION HISTORY
*--------------------------------------------
*   DATE              ODR                             DESCRIPTION
* 01-09-10          ODR-2010-08-0031              Routine to validate Account
* 10-APRIL-2023      Conversion Tool       R22 Auto Conversion  - VM to @VM , FM to @FM and ++ to +=1 
* 10-APRIL-2023      Harsha                R22 Manual Conversion - No changes 
*------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_F.ACCOUNT
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.REDO.UNAUTH.STAFF.LOG
    $INSERT I_F.REDO.EMPLOYEE.ACCOUNTS

    GOSUB OPENFILES
    GOSUB PROCESS

RETURN

*---------
OPENFILES:
*---------

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    FN.REDO.EMPLOYEE.ACCOUNTS = 'F.REDO.EMPLOYEE.ACCOUNTS'
    F.REDO.EMPLOYEE.ACCOUNTS = ''
    CALL OPF(FN.REDO.EMPLOYEE.ACCOUNTS,F.REDO.EMPLOYEE.ACCOUNTS)

    FN.REDO.UNAUTH.STAFF.LOG = 'F.REDO.UNAUTH.STAFF.LOG'
    F.REDO.UNAUTH.STAFF.LOG = ''
    CALL OPF(FN.REDO.UNAUTH.STAFF.LOG,F.REDO.UNAUTH.STAFF.LOG)

    FN.ACCOUNT='F.ACCOUNT'
    F.ACCOUNT=''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

RETURN

*-----------------------------------------
PROCESS:
*-----------------------------------------
* Main enquiry process is carried on here
*-----------------------------------------

    R.REDO.UNAUTH.STAFF.LOG = ''
    R.REDO.EMPLOYEE.ACCOUNTS = ''
    Y.CUS=''

    IF ENQ.DATA<1,1> EQ 'REDO.ACCOUNT.DETAILS' THEN
        LOCATE 'ACCOUNT.NO' IN ENQ.DATA<2,1> SETTING POS1 THEN
            Y.ACCOUNT=ENQ.DATA<4,POS1>
        END ELSE
            POS1=''
        END
        CALL F.READ(FN.ACCOUNT,Y.ACCOUNT,R.ACCOUNT,F.ACCOUNT,ACCOUNT.ERR)
    END
    IF ENQ.DATA<1,1> EQ 'REDO.OVERDRAWN.ACCOUNTS' OR ENQ.DATA<1,1> EQ 'REDO.AZ.DEP.SCH.NAU' OR ENQ.DATA<1,1> EQ 'REDO.AZ.DEPOSIT.SCHEDULES' THEN
        LOCATE '@ID' IN ENQ.DATA<2,1> SETTING POS1 THEN
            Y.ACCOUNT=ENQ.DATA<4,POS1>
        END ELSE
            POS1=''
        END
        CALL F.READ(FN.ACCOUNT,Y.ACCOUNT,R.ACCOUNT,F.ACCOUNT,ACCOUNT.ERR)
    END
    IF ENQ.DATA<1,1> EQ 'REDO.AZ.ACCOUNT' THEN
        LOCATE 'MASTER.ACCOUNT' IN ENQ.DATA<2,1> SETTING POS1 THEN
            Y.ACCOUNT=ENQ.DATA<4,POS1>
        END ELSE
            POS1=''
        END
        CALL F.READ(FN.ACCOUNT,Y.ACCOUNT,R.ACCOUNT,F.ACCOUNT,ACCOUNT.ERR)
    END
    IF ENQ.DATA<1,1> EQ 'REDO.ACCT.BAL.TODAY' OR ENQ.DATA<1,1> EQ 'REDO.STMT.ENT.TODAY' OR ENQ.DATA<1,1> EQ 'REDO.ACCOUNT.LIST' OR ENQ.DATA<1,1> EQ 'REDO.ACCT.BAL' OR ENQ.DATA<1,1> EQ 'REDO.AZ.ACCOUNT.DEP' THEN
        LOCATE 'ACCOUNT.NUMBER' IN ENQ.DATA<2,1> SETTING POS1 THEN
            Y.ACCOUNT=ENQ.DATA<4,POS1>
        END ELSE
            POS1=''
        END
        CALL F.READ(FN.ACCOUNT,Y.ACCOUNT,R.ACCOUNT,F.ACCOUNT,ACCOUNT.ERR)
    END
    IF ENQ.DATA<1,1> EQ 'REDO.STMT.ENT.LAST' OR ENQ.DATA<1,1> EQ 'REDO.STMT.ENT.BOOK' THEN
        LOCATE 'ACCOUNT' IN ENQ.DATA<2,1> SETTING POS1 THEN
            Y.ACCOUNT=ENQ.DATA<4,POS1>
        END ELSE
            POS1=''
        END
        CALL F.READ(FN.ACCOUNT,Y.ACCOUNT,R.ACCOUNT,F.ACCOUNT,ACCOUNT.ERR)
    END
    IF ENQ.DATA<1,1> EQ 'REDO.NOSTRO.FWD.BAL' THEN
        LOCATE 'ACCOUNT.ID' IN ENQ.DATA<2,1> SETTING POS1 THEN
            Y.ACCOUNT=ENQ.DATA<4,POS1>
        END ELSE
            POS1=''
        END
        CALL F.READ(FN.ACCOUNT,Y.ACCOUNT,R.ACCOUNT,F.ACCOUNT,ACCOUNT.ERR)
    END
    Y.CUS=R.ACCOUNT<AC.CUSTOMER>
    IF Y.CUS NE '' THEN
        CALL F.READ(FN.CUSTOMER,Y.CUS,R.CUSTOMER,F.CUSTOMER,ERR.CUSTOMER)
        IF R.CUSTOMER THEN
            FAX.COUNT= DCOUNT(R.CUSTOMER<EB.CUS.FAX.1>,@VM)
            VAR3=1
            LOOP
            WHILE VAR3 LE FAX.COUNT
                Y.FAX = R.CUSTOMER<EB.CUS.FAX.1,VAR3>
                GOSUB CHECK.TABLE
                VAR3 += 1
            REPEAT
        END
    END
*-------------------------------------------------------------------
* Editing the Selection Creteria for Employee accounts
*-------------------------------------------------------------------
    SEL.CMD1='SELECT ':FN.CUSTOMER:' WITH FAX.1 EQ ':OPERATOR
    CALL EB.READLIST(SEL.CMD1,SEL.LIST1,'',SEL.NOR1,SEL.RET1)
    SEL.CMD='SELECT ':FN.REDO.EMPLOYEE.ACCOUNTS
    VAR1=1
    LOOP
    WHILE VAR1 LE SEL.NOR1
        IF VAR1 EQ 1 THEN
            SEL.CMD=SEL.CMD:' WITH @ID NE ':SEL.LIST1<VAR1>
        END ELSE
            SEL.CMD=SEL.CMD:' AND @ID NE ':SEL.LIST1<VAR1>
        END
        VAR1 += 1
    REPEAT
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',SEL.NOR,SEL.RET)
    CHANGE @FM TO ' ' IN SEL.LIST
    IF ENQ.DATA<1,1> EQ 'REDO.STMT.ENT.TODAY' OR ENQ.DATA<1,1> EQ 'REDO.STMT.ENT.LAST' THEN
        ENQ.DATA<2,-1>='CUSTOMER.ID'
        ENQ.DATA<3,-1>='NE'
        ENQ.DATA<4,-1>=SEL.LIST
    END
    IF ENQ.DATA<1,1> EQ 'REDO.ACCT.BAL.TODAY' OR ENQ.DATA<1,1> EQ 'REDO.ACCT.BAL' THEN
        ENQ.DATA<2,-1>='CUSTOMER.CODE'
        ENQ.DATA<3,-1>='NE'
        ENQ.DATA<4,-1>=SEL.LIST
    END
    IF ENQ.DATA<1,1> EQ 'REDO.ACCOUNT.DETAILS' OR ENQ.DATA<1,1> EQ 'REDO.ACCOUNT.LIST' THEN
        ENQ.DATA<2,-1>='CUSTOMER.NO'
        ENQ.DATA<3,-1>='NE'
        ENQ.DATA<4,-1>=SEL.LIST
    END
    IF ENQ.DATA<1,1> EQ 'REDO.OVERDRAWN.ACCOUNTS' OR ENQ.DATA<1,1> EQ 'REDO.AZ.ACCOUNT.DEP' OR ENQ.DATA<1,1> EQ 'REDO.AZ.ACCOUNT' THEN
        ENQ.DATA<2,-1>='CUSTOMER'
        ENQ.DATA<3,-1>='NE'
        ENQ.DATA<4,-1>=SEL.LIST
    END

RETURN
*-----------
CHECK.TABLE:
*--------------------------------------
* Employee table is being checked here
*--------------------------------------
    CALL F.READ(FN.REDO.EMPLOYEE.ACCOUNTS,Y.CUS,R.REDO.EMPLOYEE.ACCOUNTS,F.REDO.EMPLOYEE.ACCOUNTS,ERR.REDO.EMPLOYEE.ACCOUNTS)
    IF R.REDO.EMPLOYEE.ACCOUNTS THEN
        Y.USER.ID = R.REDO.EMPLOYEE.ACCOUNTS<REDO.EMP.USER.ID>
        IF Y.USER.ID NE OPERATOR THEN
            ENQ.ERROR = 'EB-NO.ACCESS.USER'
            CALL STORE.END.ERROR
            GOSUB UPDATE.LOG
            GOSUB END1
        END
    END
RETURN
*----------
UPDATE.LOG:
*---------------------------------------
* The log Table is being updated here
*---------------------------------------
    CALL ALLOCATE.UNIQUE.TIME(CURRTIME)
    Y.LOG.ID = 'STAFF.':CURRTIME
    R.REDO.UNAUTH.STAFF.LOG<REDO.LOG.USER.ID> = OPERATOR
    R.REDO.UNAUTH.STAFF.LOG<REDO.LOG.ACTIVITY.DATE> = TODAY
    R.REDO.UNAUTH.STAFF.LOG<REDO.LOG.ACTIVITY.TIME> = OCONV(TIME(), "MTS")
    R.REDO.UNAUTH.STAFF.LOG<REDO.LOG.APPLICATION> = APPLICATION
    R.REDO.UNAUTH.STAFF.LOG<REDO.LOG.RECORD.ID> = ID.NEW
    CALL F.WRITE(FN.REDO.UNAUTH.STAFF.LOG,Y.LOG.ID,R.REDO.UNAUTH.STAFF.LOG)
    CALL JOURNAL.UPDATE(Y.LOG.ID)
RETURN
END1:
END
