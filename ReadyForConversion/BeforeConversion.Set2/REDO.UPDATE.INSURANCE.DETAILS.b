*-----------------------------------------------------------------------------
* <Rating>-10</Rating>
*-----------------------------------------------------------------------------
  SUBROUTINE REDO.UPDATE.INSURANCE.DETAILS(INSURANCE.ID)

*------------------------------------------------------------------------
* Description: This routine is to update the insurance details as cancelled.
*------------------------------------------------------------------------


$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.APAP.H.INSURANCE.DETAILS
$INSERT I_REDO.B.LOAN.CLOSURE.COMMON

  GOSUB PROCESS
  RETURN
*------------------------------------------------------------------------
PROCESS:
*------------------------------------------------------------------------
  SEL.CMD = 'SELECT ':FN.APAP.H.INSURANCE.DETAILS:' WITH POLICY.NUMBER EQ ':INSURANCE.ID
  SEL.LIST = ''
  CALL EB.READLIST(SEL.CMD,SEL.LIST,'',SEL.NOR,SEL.RET)

  Y.INSURANCE.ID = SEL.LIST<1>
  CALL F.READ(FN.APAP.H.INSURANCE.DETAILS,Y.INSURANCE.ID,R.INSURANCE.DETAILS,F.APAP.H.INSURANCE.DETAILS,INSURANCE.ERR)

  IF R.INSURANCE.DETAILS THEN
    R.INSURANCE.DETAILS<INS.DET.POLICY.STATUS> = 'CANCELADA'
*CALL F.WRITE(FN.APAP.H.INSURANCE.DETAILS,Y.INSURANCE.ID,R.INSURANCE.DETAILS)
    WRITE R.INSURANCE.DETAILS TO F.APAP.H.INSURANCE.DETAILS,Y.INSURANCE.ID
  END
  RETURN
END
