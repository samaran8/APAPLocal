*-----------------------------------------------------------------------------
* <Rating>69</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE REMOVE.ACCT.STMT

    $INSERT I_COMMON
    $INSERT I_EQUATE

    EXECUTE "COMO ON REMOVE.ACCT.STMT-":TODAY

    GOSUB INITIALISE
    GOSUB SELECT.RECORDS
    EXECUTE "COMO OFF REMOVE.ACCT.STMT-":TODAY
    RETURN

INITIALISE:
**********
    FN.ACC = 'F.ACCOUNT'
    F.ACC = ''
    CALL OPF(FN.ACC,F.ACC)
    FN.ACCT.STMT = 'F.ACCOUNT.STATEMENT'
    F.ACCT.STMT = ''
    CALL OPF(FN.ACCT.STMT,F.ACCT.STMT)

    FN.EB.CONTRACT.BALANCES = 'F.EB.CONTRACT.BALANCES'
    F.EB.CONTRACT.BALANCES = ''
    CALL OPF(FN.EB.CONTRACT.BALANCES,F.EB.CONTRACT.BALANCES)

     FN.SAVEDLISTS = '&SAVEDLISTS&'
     F.SAVEDLISTS = ''
     CALL OPF(FN.SAVEDLISTS,F.SAVEDLISTS)

    SEL.LIST = ''
    NO.OF.REC = ''
    RET.CODE = ''
    AC.REC = ''
    ECB.REC = ''

    RETURN
SELECT.RECORDS:
**************
   
   *READ SEL.LIST FROM F.SAVEDLISTS,'PROB.AS.LIST' ELSE
   *SEL.LIST = ''
    *END 
SEL.LIST<-1>="1023488817"
SEL.LIST<-1>="1022641891"
SEL.LIST<-1>="1022250906"
SEL.LIST<-1>="1024523276"
SEL.LIST<-1>="1023848554"
SEL.LIST<-1>="1023850427"
SEL.LIST<-1>="1023357763"
SEL.LIST<-1>="1024091244"
SEL.LIST<-1>="1023326094"
SEL.LIST<-1>="1023532549"
    CNT = DCOUNT(SEL.LIST,@FM)
    FOR X = 1 TO CNT
        IF SEL.LIST<X> THEN
            ACC.ID = SEL.LIST<X>
            GOSUB PROCESS
        END
    NEXT X
    RETURN

PROCESS:
********

    READ AC.REC FROM F.ACC,ACC.ID ELSE AC.REC = ''
    IF AC.REC EQ '' THEN
        CRT "Deleting account statement ": ACC.ID
        DELETE F.ACCT.STMT,ACC.ID
    END
    RETURN
*--------------------------------------------------------------------
*Final End
END
