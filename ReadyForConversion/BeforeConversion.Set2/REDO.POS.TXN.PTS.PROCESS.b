*-----------------------------------------------------------------------------
* <Rating>-45</Rating>
*-----------------------------------------------------------------------------
  SUBROUTINE REDO.POS.TXN.PTS.PROCESS


* this routine will validate the transaction amount with available monetry value for the available points
* will reduce the avl pts and amount with transaction amount and its equivalent points
* creates a new record in REDO.LY.POINTS.US

$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.FUNDS.TRANSFER
$INSERT I_F.USER
$INSERT I_F.REDO.LY.POINTS.TOT
$INSERT I_F.REDO.LY.POINTS.US
$INSERT I_AT.ISO.COMMON
$INSERT I_F.REDO.LY.MASTERPRGDR
$INSERT I_F.REDO.LY.PROGRAM

  GOSUB OPEN.FILES
  GOSUB GET.LOCAL.REF

  IF NOT(R.REDO.LY.POINTS.TOT ) THEN

    GOSUB REDO.CALL.ERROR
    RETURN
  END

  GOSUB PROCESS
  RETURN

***********
OPEN.FILES:
***********

  FN.REDO.LY.POINTS.TOT='F.REDO.LY.POINTS.TOT'
  F.REDO.LY.POINTS.TOT=''
  CALL OPF(FN.REDO.LY.POINTS.TOT,F.REDO.LY.POINTS.TOT)

  FN.REDO.LY.POINTS.US='F.REDO.LY.POINTS.US'
  F.REDO.LY.POINTS.US=''
  CALL OPF(FN.REDO.LY.POINTS.US,F.REDO.LY.POINTS.US)

  FN.REDO.LY.MASTERPRGDR='F.REDO.LY.MASTERPRGDR'
  F.REDO.LY.MASTERPRGDR =''
  CALL OPF(FN.REDO.LY.MASTERPRGDR,F.REDO.LY.MASTERPRGDR)

*  CALL F.READ(FN.REDO.LY.MASTERPRGDR,'SYSTEM',R.REDO.LY.MASTERPRGDR,F.REDO.LY.MASTERPRGDR,ERR.LM) ;*Tus Start 
  CALL CACHE.READ(FN.REDO.LY.MASTERPRGDR,'SYSTEM',R.REDO.LY.MASTERPRGDR,ERR.LM) ; * Tus End
  Y.MASTER.PGM=R.REDO.LY.MASTERPRGDR<REDO.MASPRG.MASTER.PRG>

  FN.REDO.LY.PROGRAM='F.REDO.LY.PROGRAM'
  F.REDO.LY.PROGRAM =''
  CALL OPF(FN.REDO.LY.PROGRAM,F.REDO.LY.PROGRAM)
  CALL F.READ(FN.REDO.LY.PROGRAM,Y.MASTER.PGM,R.REDO.LY.PROGRAM,F.REDO.LY.PROGRAM,ERR)
  Y.PT.TO.VAL=R.REDO.LY.PROGRAM<REDO.PROG.POINT.VALUE>

  R.REDO.LY.POINTS.US=''
  Y.LY.PTS.TOT=Y.CARD.CUST.ID : "ONLINEDEB"
  CALL F.READU (FN.REDO.LY.POINTS.TOT,Y.LY.PTS.TOT,R.REDO.LY.POINTS.TOT,F.REDO.LY.POINTS.TOT,'',LY.ERR)
  Y.AVL.PTS=  R.REDO.LY.POINTS.TOT<REDO.PT.T.TOT.AVAIL.POINTS>
  Y.AVL.MON.VAL= R.REDO.LY.POINTS.TOT<REDO.PT.T.TOT.AVAIL.VALUE>
  Y.ID.CERITOS=''
  RETURN

**************
GET.LOCAL.REF:
**************

  LOC.APP=APPLICATION

  LOC.FLDS="AT.UNIQUE.ID"

  LOC.POS=''

  CALL MULTI.GET.LOC.REF(LOC.APP,LOC.FLDS,LOC.POS)

  RETURN

****************
REDO.CALL.ERROR:
****************

  ETEXT='EB-AVAIL.BAL.CHECK'
  CALL STORE.END.ERROR

  RETURN


********
PROCESS:
********

  Y.TXN.AMT.CR=R.NEW(FT.CREDIT.AMOUNT)

  IF Y.AVL.MON.VAL LT Y.TXN.AMT.CR THEN
    GOSUB REDO.CALL.ERROR
    RETURN

  END

*    Y.TXN.PTS= Y.TXN.AMT.CR * (Y.AVL.PTS / Y.AVL.MON.VAL )  ;* step to identify point equivalent for the transaction amount

  Y.TXN.AMT.CR=DROUND(Y.TXN.AMT.CR,0)
  Y.TXN.PTS=Y.TXN.AMT.CR/Y.PT.TO.VAL
  Y.TXN.PTS=DROUND(Y.TXN.PTS,0)
  R.REDO.LY.POINTS.TOT<REDO.PT.T.TOT.AVAIL.POINTS>=Y.AVL.PTS-Y.TXN.PTS
  R.REDO.LY.POINTS.TOT<REDO.PT.T.TOT.AVAIL.VALUE>=Y.AVL.MON.VAL-Y.TXN.AMT.CR


  R.REDO.LY.POINTS.TOT<REDO.PT.T.TOT.USED.POINTS>=R.REDO.LY.POINTS.TOT<REDO.PT.T.TOT.USED.POINTS> + Y.TXN.PTS
  R.REDO.LY.POINTS.TOT<REDO.PT.T.TOT.USED.VALUE>=R.REDO.LY.POINTS.TOT<REDO.PT.T.TOT.USED.VALUE> + Y.TXN.AMT.CR

  CALL F.WRITE(FN.REDO.LY.POINTS.TOT,Y.LY.PTS.TOT,R.REDO.LY.POINTS.TOT)

*this section will create a new record in REDO.LY.POINTS.US

  R.REDO.LY.POINTS.US<REDO.PT.US.CUSTOMER.NO>=Y.CARD.CUST.ID
  R.REDO.LY.POINTS.US<REDO.PT.US.DATE>=TODAY
  R.REDO.LY.POINTS.US<REDO.PT.US.QTYORVAL>="MONTO"
  R.REDO.LY.POINTS.US<REDO.PT.US.QTYORVAL.TO.US>=Y.TXN.AMT.CR
  R.REDO.LY.POINTS.US<REDO.PT.US.STATUS.US>=2


  TEMPTIME = OCONV(TIME(),"MTS")
  TEMPTIME = TEMPTIME[1,5]
  CHANGE ':' TO '' IN TEMPTIME
  CHECK.DATE = DATE()
  R.REDO.LY.POINTS.US<REDO.PT.US.RECORD.STATUS>=''
  R.REDO.LY.POINTS.US<REDO.PT.US.DATE.TIME>=OCONV(CHECK.DATE,"DY2"):FMT(OCONV(CHECK.DATE,"DM"),"R%2"):FMT(OCONV(CHECK.DATE,"DD"),"R%2"):TEMPTIME
  R.REDO.LY.POINTS.US<REDO.PT.US.CURR.NO>=R.REDO.LY.POINTS.US<REDO.PT.US.CURR.NO>+1
  R.REDO.LY.POINTS.US<REDO.PT.US.INPUTTER>=TNO:'_':OPERATOR
  R.REDO.LY.POINTS.US<REDO.PT.US.AUTHORISER>=TNO:'_':OPERATOR
  R.REDO.LY.POINTS.US<REDO.PT.US.DEPT.CODE>=R.USER<EB.USE.DEPARTMENT.CODE>
  R.REDO.LY.POINTS.US<REDO.PT.US.CO.CODE>=ID.COMPANY

  Y.PREV.ID.NEW=ID.NEW
  Y.ID.COMPANY=ID.COMPANY
  Y.APPLICATION=APPLICATION
  APPLICATION='REDO.LY.POINTS.US'
  Y.PREV.ID.NEW.LAST=ID.NEW.LAST
  CALL LOAD.COMPANY(Y.ID.COMPANY)
  FULL.FNAME ='F.REDO.LY.POINTS.US'
  ID.T  = 'A'
  ID.N ='20'
  ID.CONCATFILE = ''
  COMI = ''
  PGM.TYPE = '.IDA'
  ID.NEW = ''
  V$FUNCTION = 'I'
  ID.NEW.LAST=''
  Y.ID.NEW.LAST = ''
  CALL GET.NEXT.ID(Y.ID.NEW.LAST,'F')
  Y.ID.CERITOS= COMI

  ID.NEW.LAST=Y.PREV.ID.NEW.LAST
  ID.NEW=Y.PREV.ID.NEW
  APPLICATION=Y.APPLICATION
  CALL F.WRITE(FN.REDO.LY.POINTS.US,Y.ID.CERITOS,R.REDO.LY.POINTS.US)
  RETURN

END
