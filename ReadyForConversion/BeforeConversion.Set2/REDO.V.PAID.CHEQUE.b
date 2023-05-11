*-----------------------------------------------------------------------------
* <Rating>-159</Rating>
*-----------------------------------------------------------------------------
  SUBROUTINE REDO.V.PAID.CHEQUE
*-----------------------------------------------------------------------------
*DESCRIPTION:
*------------
* This subroutine will show an override message, which would deliver
* the user a message if the option for the field, STOPPAYMENT.STATUS has been
* selected as Non-Confirmed or Confirmed
*
* Input/Output:
*--------------
* IN : -NA-
* OUT : -NA-
*
* Dependencies:
*---------------
* CALLS : -NA-
* CALLED BY : -NA-
*
* Revision History:
*------------------------------------------------------------------------------------------
*   Date           who              Reference          Description
*   25-Nov-2009    SHANKAR RAJU                        Initial Creation
*   28-MAR-2012                                        to cater Multiple values CHEQUE.ISSUE.ACCOUNT
*   28-JUN-2012    VICTOR NAVA                         Execution routine moved to INPUT event
*   27-FEB-2013    Pradeep S       PACS00251820        AF value changed
*------------------------------------------------------------------------------------------

$INSERT I_COMMON
$INSERT I_EQUATE
*
$INSERT I_GTS.COMMON
*
$INSERT I_F.TELLER
$INSERT I_F.ACCOUNT
$INSERT I_F.TELLER.TRANSACTION
$INSERT I_F.TRANSACTION
$INSERT I_F.CHEQUE.ISSUE
$INSERT I_F.PAYMENT.STOP
*$INSERT I_F.CHEQUES.STOPPED ;*Tus Start
$INSERT I_F.CHEQUE.REGISTER.SUPPLEMENT ;*Tus End
$INSERT I_F.CHEQUE.TYPE.ACCOUNT
*
$INSERT I_F.REDO.CHEQUE.STOP.PARA
*
*------------------------------MAIN------------------------------------------
*
*
  TEMP.AF = AF
  GOSUB INIT
  GOSUB OPEN.FILES
  GOSUB CHECK.PRELIM.CONDITIONS
  IF PROCESS.GOAHEAD THEN
    GOSUB PROCESS
  END
  AF = TEMP.AF
*
  RETURN
*
* ======
PROCESS:
* ======
*
  Y.FLG.FND  = ""
  CURR.NO    = DCOUNT(R.NEW(TT.TE.OVERRIDE),VM)
*
  CALL F.READ(FN.CHEQUE.ISSUE.ACCOUNT,WCHEQUE.ACCT,R.CHEQUE.ISSUE.ACCOUNT,F.CHEQUE.ISSUE.ACCOUNT,Y.ERR.PER)
  IF R.CHEQUE.ISSUE.ACCOUNT THEN
    GOSUB VALIDATE.CHEQUE
  END ELSE
    GOSUB OVE.ISSUED
  END
*
  IF NOT(Y.FLG.FND) THEN
    GOSUB OVE.ISSUED
  END
*

  CALL F.READ(FN.PAYMENT.STOP,WCHEQUE.ACCT,R.PAYMENT.STOP,F.PAYMENT.STOP,ERR.PS)
  FIRST.CHEQUE.NOS = R.PAYMENT.STOP<AC.PAY.FIRST.CHEQUE.NO>
  LAST.CHEQUE.NOS  = R.PAYMENT.STOP<AC.PAY.LAST.CHEQUE.NO>
*
  COUNTER.LOOP   = 1
  COUNT.PAY.STOP = DCOUNT(FIRST.CHEQUE.NOS,VM)
*
  LOOP
  WHILE COUNTER.LOOP LE COUNT.PAY.STOP
    FIRST.CHQ.NO = FIRST.CHEQUE.NOS<1,COUNTER.LOOP>
    LAST.CHQ.NO  = LAST.CHEQUE.NOS<1,COUNTER.LOOP>
    IF CHEQUE.NO1 GE FIRST.CHQ.NO AND CHEQUE.NO1 LE LAST.CHQ.NO THEN
      GOSUB DISPLAY.MESSAGE.SP
    END
    COUNTER.LOOP = COUNTER.LOOP + 1
  REPEAT

*This is for REDO.CHEQUE.STOP.PARA

  Y.FLAG    = 1
  Y.CUS     = ''
  Y.AMT.FRM = ''
  Y.AMT.TO  = ''

  FIRST.CHQ.NO = TRIM(CHEQUE.NO1,'0','L')
* CHEQ.STOP.ID = WCHEQUE.ACCT : "*" : FIRST.CHQ.NO ;*Tus Start

*  CALL F.READ(FN.CHEQ.STOP,CHEQ.STOP.ID,R.CHEQ.STOP,F.CHEQ.STOP,Y.ERR.CH)
       FN.CHEQUE.REGISTER.SUPPLEMENT="F.CHEQUE.REGISTER.SUPPLEMENT" 
       F.CHEQUE.REGISTER.SUPPLEMENT="" 
       CALL OPF(FN.CHEQUE.REGISTER.SUPPLEMENT,F.CHEQUE.REGISTER.SUPPLEMENT) 
      
       FN.CHEQUE.TYPE.ACCOUNT = "F.CHEQUE.TYPE.ACCOUNT" 
       F.CHEQUE.TYPE.ACCOUNT = "" 
       CALL OPF(FN.CHEQUE.TYPE.ACCOUNT,F.CHEQUE.TYPE.ACCOUNT) 
      
       CALL F.READ (FN.CHEQUE.TYPE.ACCOUNT,WCHEQUE.ACCT, REC.CHEQUE.TYPE.ACCOUNT,F.CHEQUE.TYPE.ACCOUNT,CHQ.TYPE.ERR) 
       CHQ.TYPE = REC.CHEQUE.TYPE.ACCOUNT<CHQ.TYP.CHEQUE.TYPE,1> 
       
       CHEQ.STOP.ID = CHQ.TYPE:".":WCHEQUE.ACCT:".":FIRST.CHQ.NO
       CALL F.READ(FN.CHEQUE.REGISTER.SUPPLEMENT,CHEQ.STOP.ID,R.CHEQUE.REGISTER.SUPPLEMENT,F.CHEQUE.REGISTER.SUPPLEMENT,ERR.CH.STOPPED)  
       CHQ.STATUS = R.CHEQUE.REGISTER.SUPPLEMENT<CC.CRS.STATUS> 
       IF CHQ.STATUS EQ 'STOPPED' THEN 
        Y.AMT.FRM = R.CHEQUE.REGISTER.SUPPLEMENT<CC.CRS.AMOUNT.FROM>
        Y.AMT.TO  = R.CHEQUE.REGISTER.SUPPLEMENT<CC.CRS.AMOUNT.TO>
       END 

*  Y.AMT.FRM = R.CHEQ.STOP<CHQ.STP.AMOUNT.FROM>
*  Y.AMT.TO  = R.CHEQ.STOP<CHQ.STP.AMOUNT.TO>;*Tus End
  R.REDO.CHEQUE.STOP.PARA = ""
  CALL F.READ(FN.REDO.CHEQUE.STOP.PARA,CHEQ.STOP.ID,R.REDO.CHEQUE.STOP.PARA,F.REDO.CHEQUE.STOP.PARA,Y.ERR)
  IF R.REDO.CHEQUE.STOP.PARA THEN
    GOSUB OVER.DIS.CHECK
  END
*
  RETURN
*
* ==============
VALIDATE.CHEQUE:
* ==============
*
  II          = 1
  CNT.CB      = DCOUNT(R.CHEQUE.ISSUE.ACCOUNT,@FM)
*
  LOOP
  WHILE II LE CNT.CB AND NOT(Y.FLG.FND)
    WCHEQUE.ISSUE.ACCT.ID = R.CHEQUE.ISSUE.ACCOUNT<II>
    R.CHEQUE.ISSUE = "" ; Y.ERR.CI = "" ;
    CALL F.READ(FN.CHEQUE.ISSUE,WCHEQUE.ISSUE.ACCT.ID,R.CHEQUE.ISSUE,F.CHEQUE.ISSUE,Y.ERR.CI)
    IF R.CHEQUE.ISSUE THEN
      GOSUB CHK.ISS.RG
    END ELSE
      GOSUB OVE.ISSUED
    END

    II += 1
  REPEAT
*
  RETURN
*
*----------
CHK.ISS.RG:
*----------
*
  Y.IS.CHQ.ST = R.CHEQUE.ISSUE<CHEQUE.IS.CHQ.NO.START>
  Y.IS.NUM.IS = R.CHEQUE.ISSUE<CHEQUE.IS.NUMBER.ISSUED>
  Y.IS.END    = Y.IS.CHQ.ST + Y.IS.NUM.IS
*
  IF CHEQUE.NO1 GE Y.IS.CHQ.ST AND CHEQUE.NO1 LE Y.IS.END THEN
    Y.FLG.FND            = '1'
    WCHEQUE.TYPE         = FIELD(WCHEQUE.ISSUE.ACCT.ID,".",1)
    WCHEQUE.PRESENTED.ID = WCHEQUE.TYPE : "." : WCHEQUE.ACCT : "-" : CHEQUE.NO1
    CALL F.READ(FN.CHEQ.PRE,WCHEQUE.PRESENTED.ID,R.CHEQ.PRE,F.CHEQ.PRE,Y.ERR.CH.P)
    IF R.CHEQ.PRE THEN
      TEXT = 'CHEQUE.PAID.ISSUE' : FM : CHEQUE.NO1
      CALL STORE.OVERRIDE(CURR.NO+1)
    END
  END
*
  RETURN
*
*----------
OVE.ISSUED:
*----------
*
  TEXT = 'CHEQUE.NOT.ISSUE' : FM : CHEQUE.NO1
  CALL STORE.OVERRIDE(CURR.NO+1)
*
  RETURN
*
* =============
OVER.DIS.CHECK:
* =============
*
  IF Y.AMT EQ "" THEN
    COMI = ""
    ETEXT           = "TT-ENTER.AMOUNT"
    AF              = TT.TE.AMOUNT.LOCAL.1
    CALL STORE.END.ERROR
    RETURN
  END
*
  IF Y.AMT.FRM THEN
    IF Y.AMT.FRM GT Y.AMT THEN
      Y.FLAG = ''
    END
    IF Y.AMT.TO AND Y.AMT.TO LT Y.AMT THEN
      Y.FLAG = ''
    END
  END
  IF Y.FLAG EQ '1' THEN
    Y.STATUS = R.REDO.CHEQUE.STOP.PARA<CHQ.STOP.STATUS>
    GOSUB DISPLAY.MESSAGE
  END
*
  RETURN
*
* ===============
DISPLAY.ERROR.SP:
* ===============
*
  AF = TT.TE.CHEQUE.NUMBER    ;* PACS00251820 - S/E
  ETEXT = 'EB-DESC.CHEQUE.STATUS' : FM : CHEQUE.NO1
  CALL STORE.END.ERROR

  RETURN
*
* ============
DISPLAY.ERROR:
* ============
*
  AF = TT.TE.CHEQUE.NUMBER    ;* PACS00251820 - S/E
  ETEXT = 'EB-DESC.CHEQUE.STATUS' : FM : FIRST.CHQ.NO
  CALL STORE.END.ERROR
*
  RETURN
*
* ==================
DISPLAY.OVERRIDE.SP:
* ==================
*
  TEXT = 'STATUS.OF.CHEQUE' : FM : CHEQUE.NO1
  CALL STORE.OVERRIDE(CURR.NO+1)
*
  RETURN
*
* ===============
DISPLAY.OVERRIDE:
* ===============
*
  TEXT = 'STATUS.OF.CHEQUE' : FM : FIRST.CHQ.NO
  CALL STORE.OVERRIDE(CURR.NO+1)
*
  RETURN
*
* =================
DISPLAY.MESSAGE.SP:
* =================
*
  CURR.NO = DCOUNT(R.NEW(TT.TE.OVERRIDE),VM)

  IF R.PAYMENT.STOP<AC.PAY.LOCAL.REF,POS.STOPPAYMENT.STATUS,COUNTER.LOOP> EQ 'NONCONFIRMED' THEN
    GOSUB DISPLAY.OVERRIDE.SP
  END ELSE
    IF R.PAYMENT.STOP<AC.PAY.LOCAL.REF,POS.STOPPAYMENT.STATUS,COUNTER.LOOP> EQ 'CONFIRMED' THEN
      GOSUB DISPLAY.ERROR.SP
    END
  END
*
  RETURN
*
* ==============
DISPLAY.MESSAGE:
* ==============
*
  CURR.NO = DCOUNT(R.NEW(TT.TE.OVERRIDE),VM)
  IF Y.STATUS EQ 'NONCONFIRMED' THEN
    GOSUB DISPLAY.OVERRIDE
  END ELSE
    IF Y.STATUS EQ 'CONFIRMED' THEN
      GOSUB DISPLAY.ERROR
    END
  END
*
  RETURN
*
* ======================
VALIDATE.IF.CHECK.TRANS:
* ======================
*
  CALL F.READ(FN.TRANSACTION,WTR.CODE1,R.TRANSACTION,F.TRANSACTION,ERR.TRANS)
  IF NOT(R.TRANSACTION<AC.TRA.CHEQUE.IND>) THEN
    CALL F.READ(FN.TRANSACTION,WTR.CODE2,R.TRANSACTION,F.TRANSACTION,ERR.TRANS)
    IF NOT(R.TRANSACTION<AC.TRA.CHEQUE.IND>) THEN
      PROCESS.GOAHEAD = ""
    END ELSE
      WCHEQUE.ACCT = R.NEW(TT.TE.ACCOUNT.2)
      Y.AMT        = R.NEW(TT.TE.AMOUNT.LOCAL.2)
    END
  END ELSE
    WCHEQUE.ACCT = R.NEW(TT.TE.ACCOUNT.1)
    Y.AMT        = R.NEW(TT.TE.AMOUNT.LOCAL.1)
  END
*
  RETURN
*
*------------------------------INIT------------------------------------------
*
INIT:
*
  PROCESS.GOAHEAD   = 1
  WCHEQUE.ACCT      = ''
  R.PAYMENT.STOP    = ''
  CHEQUE.NO1        = ''
  POS               = ''
  CURR.NO           = ''
  CHEQUE.NO1        = ''
  VAR.OFS.OPERATION = OFS$OPERATION
*
  FN.CHEQ.PRE = 'F.CHEQUES.PRESENTED'
  F.CHEQ.PRE = ''
*
  FN.CHEQUE.ISSUE.ACCOUNT = 'F.CHEQUE.ISSUE.ACCOUNT'
  F.CHEQUE.ISSUE.ACCOUNT = ''
*
  FN.ACCOUNT = 'F.ACCOUNT'
  F.ACCOUNT = ''
*
  FN.CHEQUE.ISSUE = 'F.CHEQUE.ISSUE'
  F.CHEQUE.ISSUE  = ''
*
  FN.TELLER.TRANSACTION = "F.TELLER.TRANSACTION"
  F.TELLER.TRANSACTION  = ""
*
  FN.TRANSACTION = "F.TRANSACTION"
  F.TRANSACTION  = ""
*
  FN.PAYMENT.STOP='F.PAYMENT.STOP'
  F.PAYMENT.STOP=''
*
  FN.REDO.CHEQUE.STOP.PARA = 'F.REDO.CHEQUE.STOP.PARA'
  F.REDO.CHEQUE.STOP.PARA = ''
*
  *FN.CHEQ.STOP = 'F.CHEQUES.STOPPED' ;*Tus Start
  *F.CHEQ.STOP = '' ;*Tus End
*
  CALL MULTI.GET.LOC.REF('PAYMENT.STOP','L.PS.STP.PMT.ST',POS)
  POS.STOPPAYMENT.STATUS = POS
*
  IF VAR.OFS.OPERATION EQ "VALIDATE" THEN
    PROCESS.GOAHEAD = ""
  END
*
  RETURN
*
* =========
OPEN.FILES:
* =========
*
  CALL OPF(FN.ACCOUNT,F.ACCOUNT)
  CALL OPF(FN.CHEQ.PRE,F.CHEQ.PRE)
  CALL OPF(FN.CHEQUE.ISSUE.ACCOUNT,F.CHEQUE.ISSUE.ACCOUNT)
  CALL OPF(FN.CHEQUE.ISSUE,F.CHEQUE.ISSUE)
  CALL OPF(FN.REDO.CHEQUE.STOP.PARA,F.REDO.CHEQUE.STOP.PARA)
 *CALL OPF(FN.CHEQ.STOP,F.CHEQ.STOP) ;*Tus (S/E)
  CALL OPF(FN.PAYMENT.STOP,F.PAYMENT.STOP)
*
  RETURN
*
*-----------------------
CHECK.PRELIM.CONDITIONS:
*-----------------------
*
  LOOP.CNT  = 1   ;   MAX.LOOPS = 3
*
  LOOP
  WHILE LOOP.CNT LE MAX.LOOPS AND PROCESS.GOAHEAD DO
    BEGIN CASE
    CASE LOOP.CNT EQ 1
      WTTRAN.ID = R.NEW(TT.TE.TRANSACTION.CODE)
      CALL F.READ(FN.TELLER.TRANSACTION,WTTRAN.ID,R.TELLER.TRANSACTION,F.TELLER.TRANSACTION,ERR.TTRAN)

      IF R.TELLER.TRANSACTION THEN
        WTR.CODE1 = R.TELLER.TRANSACTION<TT.TR.TRANSACTION.CODE.1>
        WTR.CODE2 = R.TELLER.TRANSACTION<TT.TR.TRANSACTION.CODE.2>
      END ELSE
        ETEXT           = "TT-TELLER.TRANSACTION.MISS"
        PROCESS.GOAHEAD = ""
        AF              = TT.TE.TRANSACTION.CODE
        CALL STORE.END.ERROR
      END

    CASE LOOP.CNT EQ 2
      GOSUB VALIDATE.IF.CHECK.TRANS

    CASE LOOP.CNT EQ 3
*  PACS00247880 - S
      IF VAR.OFS.OPERATION EQ "PROCESS" THEN
        CHEQUE.NO1 = R.NEW(TT.TE.CHEQUE.NUMBER)
      END
*
      IF AF EQ TT.TE.CHEQUE.NUMBER THEN
        CHEQUE.NO1 = COMI
      END
*
      IF CHEQUE.NO1 EQ "" THEN
        ETEXT           = "TT-INPUT.CHEQUE.NUMBER"
        PROCESS.GOAHEAD = ""
        AF              = TT.TE.CHEQUE.NUMBER
        CALL STORE.END.ERROR
      END
* PACS00247880 - E
    END CASE
*       Increase
    LOOP.CNT += 1
*
  REPEAT
*
  RETURN
*

END
*-----------------------------------------------------------------------------------------------------
