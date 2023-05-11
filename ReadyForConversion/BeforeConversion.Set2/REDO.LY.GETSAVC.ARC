*-----------------------------------------------------------------------------
* <Rating>-60</Rating>
*-----------------------------------------------------------------------------
  SUBROUTINE  REDO.LY.GETSAVC.ARC(R.DATA)
*----------------------------------------------------------------------------------------------------
*DESCRIPTION : This routine is used to get the savings account available for Customer in Usage Txn.
*              Routine used for ARC-IB functionality only.
*-----------------------------------------------------------------------------------------------------
*-----------------------------------------------------------------------------------------------------
* * Input / Output
* --------------
* IN     : CUSTOMER.ID
* OUT    : R.DATA
*-----------------------------------------------------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : RMONDRAGON
* PROGRAM NAME : REDO.LY.GETSAVC.ARC
*-----------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
*DATE             WHO                REFERENCE         DESCRIPTION
*31.07.2012    RMONDRAGON         ODR-2011-06-0243     FIRST VERSION
* -----------------------------------------------------------------------------------------------------

$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_ENQUIRY.COMMON
$INSERT I_F.CUSTOMER.ACCOUNT
$INSERT I_F.ACCOUNT
$INSERT I_GTS.COMMON
$INSERT I_System

$INSERT I_F.AI.REDO.ARCIB.PARAMETER

  GOSUB INIT
  GOSUB OPEN.FILES
  GOSUB GET.CATEGORIES
  GOSUB PROCESS

  RETURN

*----
INIT:
*----

  LREF.APP = 'ACCOUNT'
  LREF.FIELDS = 'L.AC.STATUS1':VM:'L.AC.STATUS2':VM:'L.AC.NOTIFY.1'
  LREF.POS = ''
  CALL MULTI.GET.LOC.REF(LREF.APP,LREF.FIELDS,LREF.POS)
  L.AC.STATUS1.POS = LREF.POS<1,1>
  L.AC.STATUS2.POS = LREF.POS<1,2>
  L.AC.NOTIFY.POS = LREF.POS<1,3>
  R.DATA = ''
  RETURN

*----------
OPEN.FILES:
*----------

  FN.AI.REDO.ARCIB.PARAMETER = 'F.AI.REDO.ARCIB.PARAMETER'
  F.AI.REDO.ARCIB.PARAMETER = ''
  CALL OPF(FN.AI.REDO.ARCIB.PARAMETER,F.AI.REDO.ARCIB.PARAMETER)

  FN.CUSTOMER.ACCOUNT = 'F.CUSTOMER.ACCOUNT'
  F.CUSTOMER.ACCOUNT = ''
  CALL OPF(FN.CUSTOMER.ACCOUNT,F.CUSTOMER.ACCOUNT)

  FN.ACCOUNT = 'F.ACCOUNT'
  F.ACCOUNT = ''
  CALL OPF(FN.ACCOUNT,F.ACCOUNT)

  RETURN

*--------------
GET.CATEGORIES:
*--------------

  R.ARCIB.PARAM = ''; ARCIB.ERR = ''
  CALL CACHE.READ(FN.AI.REDO.ARCIB.PARAMETER,'SYSTEM',R.ARCIB.PARAM,ARCIB.ERR)
  Y.ALL.ACCOUNT.TYPE = R.ARCIB.PARAM<AI.PARAM.ACCOUNT.TYPE>
  Y.ALL.CATEG.START = R.ARCIB.PARAM<AI.PARAM.CATEG.START>
  Y.ALL.CATEG.END = R.ARCIB.PARAM<AI.PARAM.CATEG.END>

  Y.TOT.ACCOUNT.TYPE = DCOUNT(Y.ALL.ACCOUNT.TYPE,VM)
  Y.ACCT.TYPE.CNT = 1
  LOOP
  WHILE Y.ACCT.TYPE.CNT LE Y.TOT.ACCOUNT.TYPE
    Y.ACCONT.TYPE = FIELD(Y.ALL.ACCOUNT.TYPE,VM,Y.ACCT.TYPE.CNT)
    IF Y.ACCONT.TYPE EQ 'SAVINGS' THEN
      Y.CATEG.START := FIELD(Y.ALL.CATEG.START,VM,Y.ACCT.TYPE.CNT):'*'
      Y.CATEG.END := FIELD(Y.ALL.CATEG.END,VM,Y.ACCT.TYPE.CNT):'*'
    END
    Y.ACCT.TYPE.CNT++
  REPEAT

  Y.TOT.RANGES = DCOUNT(Y.CATEG.START,'*') - 1

  RETURN

*-------
PROCESS:
*-------

  Y.US.CUSTOMER = System.getVariable("EXT.SMS.CUSTOMERS")
  Y.CU.ACCT.ER=''
  CALL F.READ(FN.CUSTOMER.ACCOUNT,Y.US.CUSTOMER,ACCT.LST,F.CUSTOMER.ACCOUNT,Y.CU.ACCT.ER)
  IF Y.CU.ACCT.ER THEN
    RETURN
  END

  Y.ACCTS.CNT = DCOUNT(ACCT.LST,FM)
  Y.CNT = 1
  LOOP
  WHILE Y.CNT LE Y.ACCTS.CNT
    Y.ACCT = FIELD(ACCT.LST,FM,Y.CNT)
    Y.AV.ACCT = 'N'
    GOSUB CHECK.ACCOUNT
    IF Y.AV.ACCT EQ 'Y' THEN
      R.DATA<-1> = Y.ACCT
    END
    Y.CNT++
  REPEAT

  RETURN

*-------------
CHECK.ACCOUNT:
*-------------

  R.ACCOUNT = '' ; ACC.ERR = ''
  CALL F.READ(FN.ACCOUNT,Y.ACCT,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
  IF R.ACCOUNT THEN
    Y.ACCT.CAT = R.ACCOUNT<AC.CATEGORY>
    Y.CHECK.CNT = 1
    LOOP
    WHILE Y.CHECK.CNT LE Y.TOT.RANGES
      Y.LIMIT.1 = FIELD(Y.CATEG.START,'*',Y.CHECK.CNT)
      Y.LIMIT.2 = FIELD(Y.CATEG.END,'*',Y.CHECK.CNT)
      IF Y.ACCT.CAT GE Y.LIMIT.1 AND Y.ACCT.CAT LE Y.LIMIT.2 THEN
        Y.AV.ACCT = 'Y'
        GOSUB CHECK.ACCT.STATUS
        RETURN
      END
      Y.CHECK.CNT++
    REPEAT
  END

  RETURN

*-----------------
CHECK.ACCT.STATUS:
*-----------------

  Y.ACC.STATUS1 = R.ACCOUNT<AC.LOCAL.REF><1,L.AC.STATUS1.POS>

  IF Y.ACC.STATUS1 NE 'ACTIVE' THEN
    Y.AV.ACCT = 'N'
    RETURN
  END

  Y.ACC.STATUS2 = R.ACCOUNT<AC.LOCAL.REF><1,L.AC.STATUS2.POS>

  IF Y.ACC.STATUS2 EQ 'DECEASED' THEN
    Y.AV.ACCT = 'N'
    RETURN
  END

  Y.PR.ID = R.ACCOUNT<AC.POSTING.RESTRICT>

  IF Y.PR.ID EQ '2' OR Y.PR.ID EQ '3' THEN
    Y.AV.ACCT = 'N'
    RETURN
  END

  Y.ACC.NOTIF = R.ACCOUNT<AC.LOCAL.REF><1,L.AC.NOTIFY.POS>

  IF Y.ACC.NOTIF EQ 'NOTIFY.MGMT.MONEY.LAUNDRY.PREV' OR Y.ACC.NOTIF EQ 'NOTIFY.OFFICER' THEN
    Y.AV.ACCT = 'N'
    RETURN
  END

  RETURN

*----------------------------------------------------------------------------------
END
