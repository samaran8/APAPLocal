*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
  SUBROUTINE REDO.GET.TOTAL.OUTSTANDING.BYPROP(Y.AA.ID,Y.PROPERTY.LIST,Y.BALANCE.TYPE,Y.TOTAL.AMOUNT)
*-----------------------------------------------------------------
* Description: This routine is to calculate the outstanding balance of Loan by property and balance types.
*-----------------------------------------------------------------
* InComing Arg: ARR.ID         -> Arrangement ID.
*               Y.PROPERTY.LIST-> Property List seperated by FM marker.
*               Y.BALANCE.TYPE -> Balance prefix like ACC,DUE etc seperated by FM marker.
* Outgoing Arg: Y.TOTAL.AMOUNT -> Sum of the amounts.
*-----------------------------------------------------------------

$INSERT I_COMMON
$INSERT I_EQUATE


  GOSUB INIT
  GOSUB PROCESS

  RETURN
*-----------------------------------------------------------------
INIT:
*-----------------------------------------------------------------
  Y.TOTAL.AMOUNT= 0
  IN.ACC.ID     = ''
  Y.ACC.ID      = ''
  CALL REDO.CONVERT.ACCOUNT(IN.ACC.ID,Y.AA.ID,Y.ACC.ID,ERR.TEXT)

  RETURN
*-----------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------

  Y.BALANCE = 0
  Y.PROPERTY.CNT = DCOUNT(Y.PROPERTY.LIST,FM)
  Y.BALANCE.CNT  = DCOUNT(Y.BALANCE.TYPE,FM)
  Y.LOOP1 = 1
  LOOP
  WHILE Y.LOOP1 LE Y.PROPERTY.CNT
    Y.LOOP2 = 1
    LOOP
    WHILE Y.LOOP2 LE Y.BALANCE.CNT

      BALANCE.TO.CHECK = Y.BALANCE.TYPE<Y.LOOP2>:Y.PROPERTY.LIST<Y.LOOP1>
      BALANCE.AMOUNT=''
      CALL AA.GET.ECB.BALANCE.AMOUNT(Y.ACC.ID,BALANCE.TO.CHECK,TODAY,BALANCE.AMOUNT,RET.ERROR)
      Y.BALANCE += ABS(BALANCE.AMOUNT)
      Y.LOOP2++
    REPEAT
    Y.LOOP1++
  REPEAT
  Y.TOTAL.AMOUNT = Y.BALANCE
  RETURN
END
