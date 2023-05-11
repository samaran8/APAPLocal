*-----------------------------------------------------------------------------

* <Rating>3</Rating>

*-----------------------------------------------------------------------------

  SUBROUTINE REDO.FC.S.PICTORACION

*

* ====================================================================================

*

*


* ====================================================================================

*

* Subroutine Type :

* Attached to     :

* Attached as     :

* Primary Purpose :

*

*

* Incoming:

* ---------

*

*

*

* Outgoing:



* ---------

*

*

*-----------------------------------------------------------------------------------

* Modification History:

* ====================

* Development for : Asociacion Popular de Ahorros y Prestamos

* Development by  : Bryan Torres (btorxresalbornoz@temenos.com) - TAM Latin America

* Date            : Septiembre 2011

* Development by  : JP - Code Review

* Date            : 18 Octubre 2011

* Edited by       : Jorge Valarezo - PACS00169926

* Date            : 2 Abril 2012

*=======================================================================



$INSERT I_COMMON

$INSERT I_EQUATE

$INSERT I_F.AA.ARRANGEMENT.ACTIVITY

$INSERT I_F.COLLATERAL

$INSERT I_F.ACCOUNT

$INSERT I_F.REDO.COLLATERAL.REA

$INSERT I_F.AC.LOCKED.EVENTS

$INSERT I_F.REDO.CREATE.ARRANGEMENT

*

*************************************************************************

*

  GOSUB INITIALISE

  GOSUB OPEN.FILES



  IF PROCESS.GOAHEAD THEN

    GOSUB PROCESS

  END



*

  RETURN

*

*========

PROCESS:

*========



  GOSUB GET.COLLATERAL.TYPE



  RETURN



*====================

GET.COLLATERAL.TYPE:

*====================

  Y.TXN.ID = R.NEW(AA.ARR.ACT.LOCAL.REF)<1,TXN.REF.ID.POS>

  Y.ARRANGEMENT.ID = R.NEW(AA.ARR.ACT.ARRANGEMENT)



  CALL F.READ(FN.REDO.CREATE.ARRANGEMENT,Y.TXN.ID,R.REDO.CREATE.ARRANGEMENT,F.REDO.CREATE.ARRANGEMENT,REDO.CREATE.ARRANGEMENT.ERR)

  IF REDO.CREATE.ARRANGEMENT.ERR THEN

    ETEXT = "EB-FC-READ.ERROR" : FM : FN.REDO.CREATE.ARRANGEMENT

    CALL STORE.END.ERROR

    RETURN

  END



* Recupera el numero de garantias asociadas a este prestamo

  Y.COLL.COUNT =  DCOUNT(R.REDO.CREATE.ARRANGEMENT<REDO.FC.SEC.NO.STATE.DI>,VM)



  FOR I = 1 TO Y.COLL.COUNT

    IF NOT (R.REDO.CREATE.ARRANGEMENT<REDO.FC.COLL.RE.USED.DI,I>) THEN          ;* ADDED TO AVOID THAT TWO OR MORE TIMES WILL BE LOCKEDS AMOUNTS JV02042012

      Y.COLL.ID.DI=R.REDO.CREATE.ARRANGEMENT<REDO.FC.SEC.NO.STATE.DI,I>

      Y.NUM.INS=R.REDO.CREATE.ARRANGEMENT<REDO.FC.NUM.INST.COLL.DI,I>



      CALL F.READ(FN.COLLATERAL,Y.COLL.ID.DI,R.COLLATERAL,F.COLLATERAL,ERR.MSJ)

      IF ERR.MSJ THEN

        ETEXT = "EB-FC-READ.ERROR" : FM : FN.COLLATERAL

        CALL STORE.END.ERROR

        RETURN

      END

      IF R.COLLATERAL THEN

        Y.COLLATERAL.CODE = R.COLLATERAL<COLL.COLLATERAL.CODE>

        Y.COLLATERAL.TYPE = R.COLLATERAL<COLL.COLLATERAL.TYPE>

        Y.NOMINAL.VALUE = R.COLLATERAL<COLL.NOMINAL.VALUE>

        IF Y.COLLATERAL.CODE EQ "150" THEN

          CALL F.READ(FN.REDO.COLLATERAL.REA,Y.COLLATERAL.TYPE,R.REDO.COLLATERAL.REA,F.REDO.COLLATERAL.REA,ERR.MSJ)

          IF ERR.MSJ THEN

            ETEXT = "EB-FC-READ.ERROR" : FM : FN.REDO.COLLATERAL.REA

            CALL STORE.END.ERROR

            RETURN

          END



          YRESULT = Y.NOMINAL.VALUE

          GOSUB REGISTER.LOCKED.EV

*                  GOSUB READ.ACCOUNT

*                  GOSUB AFECT.BAL

*                  GOSUB WRITE.ACCOUNT



        END



      END

    END

  NEXT



  RETURN



*

* =========

REGISTER.LOCKED.EV:

* =========

*



  Y.TO.DATE=""

  Y.FROM.DATE=R.REDO.CREATE.ARRANGEMENT<REDO.FC.EFFECT.DATE>

  Y.ACCOUNT.NO = Y.NUM.INS

  Y.LOCKED.AMOUNT = YRESULT
* PACS00260025 - S
  OFS.STR  = 'AC.LOCKED.EVENTS,APAP/I/PROCESS,/,,TRANSACTION.REF:1:1:=':',ACCOUNT.NUMBER:1:1:=':Y.ACCOUNT.NO:',DESCRIPTION:1:1:=':Y.COLL.ID.DI:',FROM.DATE:1:1:=':Y.FROM.DATE:',TO.DATE:1:1:=':Y.TO.DATE:',LOCKED.AMOUNT:1:1:=':Y.LOCKED.AMOUNT
* PACS00260025 - E
  OFS.STR := ',L.AC.LOCKE.TYPE:=GUARANTEE.STATUS,L.AC.STATUS2:=GUARANTEE.STATUS,L.AC.LK.COL.ID:=':Y.ARRANGEMENT.ID

  OFS.SRC= 'FC.OFS'

  OFS.MSG.ID = ''

  OPTIONS = ''

  CALL OFS.POST.MESSAGE(OFS.STR,OFS.MSG.ID,OFS.SRC,OPTIONS)



  RETURN



*************

READ.ACCOUNT:

*************

  R.ACCOUNT  = ''

  ACCOUNT.ER = ''

  CALL F.READ(FN.ACCOUNT,Y.NUM.INS,R.ACCOUNT,F.ACCOUNT,ACCOUNT.ER)

  IF ACCOUNT.ER THEN

    ETEXT = "EB-FC-READ.ERROR" : FM : FN.ACCOUNT

    CALL STORE.END.ERROR

    RETURN

  END



  RETURN

*------------------------------------------------------------------------

*=================

AFECT.BAL:

*=================

  Y.AC.AV.BAL = 0

  Y.AMT = R.ACCOUNT<AC.LOCAL.REF,LOC.L.AC.AV.BAL.POS>

  Y.AC.AV.BAL = Y.AMT - YRESULT

  R.ACCOUNT<AC.LOCAL.REF,LOC.L.AC.AV.BAL.POS> = Y.AC.AV.BAL



  RETURN



**************

WRITE.ACCOUNT:

**************

  CALL F.WRITE(FN.ACCOUNT,Y.NUM.INS,R.ACCOUNT)



  RETURN



**********************

FIND.MULTI.LOCAL.REF:

**********************

  APPL.ARRAY = 'ACCOUNT' : FM : 'AA.ARRANGEMENT.ACTIVITY'

  FLD.ARRAY  = 'L.AC.AV.BAL' : FM : 'TXN.REF.ID'

  FLD.POS    = ''



  CALL MULTI.GET.LOC.REF(APPL.ARRAY,FLD.ARRAY,FLD.POS)



  LOC.L.AC.AV.BAL.POS =  FLD.POS<1,1>

  TXN.REF.ID.POS = FLD.POS<2,1>



  RETURN





* =========

OPEN.FILES:

* =========

*

  CALL OPF(FN.REDO.CREATE.ARRANGEMENT,F.REDO.CREATE.ARRANGEMENT)

  CALL OPF(FN.REDO.COLLATERAL.REA,F.REDO.COLLATERAL.REA)

  CALL OPF(FN.COLLATERAL,F.COLLATERAL)

  CALL OPF(FN.ACCOUNT,F.ACCOUNT)

  RETURN

*

* =========

INITIALISE:

* =========

*

  PROCESS.GOAHEAD = 1



  FN.COLLATERAL= "F.COLLATERAL"

  F.COLLATERAL=""



  FN.REDO.CREATE.ARRANGEMENT = 'F.REDO.CREATE.ARRANGEMENT'

  F.REDO.CREATE.ARRANGEMENT = ''



  FN.REDO.COLLATERAL.REA="F.REDO.COLLATERAL.REA"

  F.REDO.COLLATERAL.REA=""



  FN.AC.LOCKED.EVENTS="F.AC.LOCKED.EVENTS"

  F.AC.LOCKED.EVENTS =""



  FN.ACCOUNT = 'F.ACCOUNT'

  F.ACCOUNT = ''



  GOSUB FIND.MULTI.LOCAL.REF



  RETURN

*



END
