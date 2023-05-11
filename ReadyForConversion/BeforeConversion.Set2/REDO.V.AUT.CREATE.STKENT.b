*-----------------------------------------------------------------------------
* <Rating>-57</Rating>
*-----------------------------------------------------------------------------
  SUBROUTINE REDO.V.AUT.CREATE.STKENT
***********************************************************
*----------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : KAVITHA
* PROGRAM NAME : REDO.V.AUT.CREATE.STKENT
*----------------------------------------------------------


* DESCRIPTION : This routine is a validation routine attached to version
* REDO.CARD.REQUEST,SUPPLY , REDO.CARD.REQUEST,BRANCH to create STOCK.ENTRY
*------------------------------------------------------------

*    IN PARAMETER: NONE
*    OUT PARAMETER: NONE

*----------------------------------------------------------------------
* Modification History :
*-----------------------
*DATE             WHO                    REFERENCE         DESCRIPTION
*20-1-2011       KAVITHA                 ODR-2010-03-0400  INITIAL CREATION
*05-4-2011       KAVITHA                 PACS00036008      ISSUE FIX
*19 MAY 2011     JEEVA T                 ODR-2010-03-0400  Mapping Stock Entry Validation to Local Template
*----------------------------------------------------------------------


*-------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.REDO.STOCK.ENTRY
$INSERT I_F.COMPANY
$INSERT I_F.REDO.CARD.REQUEST
$INSERT I_F.REDO.CARD.SERIES.PARAM
$INSERT I_F.LOCKING



  GOSUB INIT
  GOSUB OPENFILES
  GOSUB PROCESS
  RETURN

*-------------------------------------------------------------
INIT:
*Initialising
*-------------------------------------------------------------

  R.REDO.STOCK.ENTRY = ''



  RETURN

*-------------------------------------------------------------
OPENFILES:
*Opening File

  FN.LOCKING='F.LOCKING'
  F.LOCKING = ''
  CALL OPF(FN.LOCKING,F.LOCKING)

  FN.REDO.STOCK.ENTRY = 'F.REDO.STOCK.ENTRY'
  F.REDO.STOCK.ENTRY = ''
  CALL OPF(FN.REDO.STOCK.ENTRY,F.REDO.STOCK.ENTRY)

  FN.REDO.CARD.REQ = 'F.REDO.CARD.REQUEST'
  F.REDO.CARD.REQ = ''
  CALL OPF(FN.REDO.CARD.REQ,F.REDO.CARD.REQ)

  FN.REDO.SER.PARAM = 'F.REDO.CARD.SERIES.PARAM'
  F.REDO.SER.PARAM = ''
  CALL OPF(FN.REDO.SER.PARAM,F.REDO.SER.PARAM)

  RETURN
*-------------------------------------------------------------
PROCESS:


*    CALL F.READ(FN.REDO.SER.PARAM,'SYSTEM',R.REDO.SER.PARAM,F.REDO.SER.PARAM,SER.ERR)

  CALL CACHE.READ(FN.REDO.SER.PARAM,'SYSTEM',R.REDO.SER.PARAM,PROD.ERR)

  IF R.REDO.SER.PARAM THEN
    EMBOSS.DEPT.CODE = R.REDO.SER.PARAM<REDO.CARD.SERIES.PARAM.EMBOSS.DEPT.CODE>
    RECEIVE.DEPT.CODE = R.REDO.SER.PARAM<REDO.CARD.SERIES.PARAM.RECEIVE.DEPT.CODE>
    VIRGIN.DEPT.CODE = R.REDO.SER.PARAM<REDO.CARD.SERIES.PARAM.VIRGIN.DEPT.CODE>
    TRANSIT.DEPT.CODE = R.REDO.SER.PARAM<REDO.CARD.SERIES.PARAM.TRANSIT.DEPT.CODE>
  END



  FINANCIAL.COMP = R.COMPANY(EB.COM.FINANCIAL.COM)

  AGENCY = R.NEW(REDO.CARD.REQ.AGENCY)

  IF PGM.VERSION EQ ',PRINTING' THEN
    GOSUB COMMON.ARRAY

*PACS00036008-S
    R.REDO.STOCK.ENTRY<STK.TO.REGISTER> = 'CARD.':ID.COMPANY:'-':EMBOSS.DEPT.CODE
*PACS00036008-E
*        R.REDO.STOCK.ENTRY<STO.ENT.LOCAL.REF,AGENCY.POS> =  ID.COMPANY
    R.REDO.STOCK.ENTRY<STK.NOTES> = "PRINTING"
  END

  IF PGM.VERSION EQ ',BRANCH' THEN
    GOSUB COMMON.ARRAY
    R.REDO.STOCK.ENTRY<STK.FROM.REGISTER>= 'CARD.':ID.COMPANY:'-':EMBOSS.DEPT.CODE
    R.REDO.STOCK.ENTRY<STK.TO.REGISTER>= 'CARD.':ID.COMPANY:'-':TRANSIT.DEPT.CODE
*        R.STOCK.ENTRY<STO.ENT.LOCAL.REF,AGENCY.POS> = AGENCY
    R.REDO.STOCK.ENTRY<STK.NOTES> = "DELIVERY TO TRANSIT"
  END

  IF PGM.VERSION EQ ',BRANCH.RECEIVED' THEN
    GOSUB COMMON.ARRAY

    R.REDO.STOCK.ENTRY<STK.FROM.REGISTER>= 'CARD.':FINANCIAL.COMP:'-':TRANSIT.DEPT.CODE
    R.REDO.STOCK.ENTRY<STK.TO.REGISTER>= 'CARD.':ID.COMPANY:'-':RECEIVE.DEPT.CODE
*       R.STOCK.ENTRY<STO.ENT.LOCAL.REF,AGENCY.POS> = AGENCY
    R.REDO.STOCK.ENTRY<STK.NOTES> = "DELIVERY TO BRANCH"
  END

  GOSUB CALL.OFS.PROCESS

  RETURN

*--------------
COMMON.ARRAY:

  R.REDO.STOCK.ENTRY<STK.IN.OUT.DATE> = TODAY
  R.REDO.STOCK.ENTRY<STK.BATCH.NO> = ID.NEW
*    Y.REQ.TOT.COUNT= DCOUNT(R.NEW(REDO.CARD.REQ.CARD.START.NO),VM)
  Y.REQ.TOT.COUNT = DCOUNT(R.NEW(REDO.CARD.REQ.REGOFF.ACCEPTQTY),VM)
  Y.REQ.COUNT = 1
  LOOP
  WHILE Y.REQ.COUNT LE Y.REQ.TOT.COUNT
    R.REDO.STOCK.ENTRY<STK.STOCK.QUANTITY,Y.REQ.COUNT> = R.NEW(REDO.CARD.REQ.REGOFF.ACCEPTQTY)<1,Y.REQ.COUNT>
*R.STOCK.ENTRY<STO.ENT.STOCK.START.NO,Y.REQ.COUNT> = R.NEW(REDO.CARD.REQ.CARD.START.NO)<1,Y.REQ.COUNT>

*        Y.NEW.STK.SERIES = R.NEW(REDO.CARD.REQ.CARD.SERIES.ID)<1,Y.REQ.COUNT>
    Y.NEW.STK.SERIES = R.NEW(REDO.CARD.REQ.CARD.TYPE)<1,Y.REQ.COUNT>

    R.REDO.STOCK.ENTRY<STK.STOCK.SERIES,Y.REQ.COUNT> = Y.NEW.STK.SERIES
    Y.REQ.COUNT=Y.REQ.COUNT+1
  REPEAT


  RETURN
*-------------------
CALL.OFS.PROCESS:



  Y.NEW.ID = ''

  Y.LOCKING.ID = 'F.REDO.STOCK.ENTRY'

  R.LOCKING   = ''
  LOCKING.ERR = ''
  CALL F.READU(FN.LOCKING,Y.LOCKING.ID,R.LOCKING,F.LOCKING,LOCKING.ERR,'P')
  IF R.LOCKING THEN
    Y.NEW.ID = R.LOCKING<EB.LOK.CONTENT>
    FETCH.NEXT.ID = Y.NEW.ID[3,10]
    FETCH.NEXT.ID += 1
    Y.NEW.ID = 'SE':FETCH.NEXT.ID
    R.LOCKING<EB.LOK.CONTENT> = Y.NEW.ID
  END

  CALL F.WRITE(FN.LOCKING,Y.LOCKING.ID,R.LOCKING)

  APP.NAME = 'REDO.STOCK.ENTRY'
  OFSFUNCT = 'I'
  PROCESS  = 'PROCESS'
  OFSVERSION = 'REDO.STOCK.ENTRY,REDO.CARD.MV'
  GTSMODE = ''
  NO.OF.AUTH = '0'
  TRANSACTION.ID = Y.NEW.ID
  OFSRECORD = ''
  OFS.MSG.ID =''
  OFS.SOURCE.ID = 'DEBIT.CARD'
  OFS.ERR = ''

  CALL OFS.BUILD.RECORD(APP.NAME,OFSFUNCT,PROCESS,OFSVERSION,GTSMODE,NO.OF.AUTH,TRANSACTION.ID,R.REDO.STOCK.ENTRY,OFSRECORD)
  CALL OFS.POST.MESSAGE(OFSRECORD,OFS.MSG.ID,OFS.SOURCE.ID,OFS.ERR)


  RETURN
*-----------------------

END
