*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
  SUBROUTINE AT.CALC.AVAIL.BALANCE(R.ACCT,WRK.BAL,AVAIL.BAL)
*/rtn to calculate the available balance to return
*/AVAIL.BAL = WORK.BAL - LOCKED.AMT - TXN.AMT
  $INSERT I_COMMON
  $INSERT I_EQUATE
  $INSERT I_F.ACCOUNT
  $INSERT I_F.ACCOUNT.CLASS
  $INSERT I_F.CURRENCY



  GOSUB INITIALISE
  GOSUB PROCESS
  RETURN


*
INITIALISE:
*--------*
  FN.ACCOUNT.CLASS = 'F.ACCOUNT.CLASS'
  CALL OPF(FN.ACCOUNT.CLASS,F.ACCOUNT.CLASS)

  RETURN

*
PROCESS:
*____________*

  IF R.ACCT<AC.FROM.DATE> THEN
    U.LOCK.AMT =0
    U.CTR = DCOUNT(R.ACCT<AC.FROM.DATE>, @VM)
    FOR U.I = 1 TO U.CTR

      IF TODAY GE R.ACCT<AC.FROM.DATE, U.I> THEN

        U.LOCK.AMT = R.ACCT<AC.LOCKED.AMOUNT,U.I>
      END
    NEXT U.I

    IF WRK.BAL LT 0 THEN
      AVAIL.BAL = ABS(WRK.BAL) + U.LOCK.AMT
      AVAIL.BAL = AVAIL.BAL * -1
    END ELSE
      AVAIL.BAL = WRK.BAL - U.LOCK.AMT
    END
  END ELSE

    AVAIL.BAL = WRK.BAL

  END
  RETURN
