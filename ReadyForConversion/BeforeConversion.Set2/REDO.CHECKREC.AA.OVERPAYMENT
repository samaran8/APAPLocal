*-----------------------------------------------------------------------------
* <Rating>-10</Rating>
*-----------------------------------------------------------------------------
  SUBROUTINE REDO.CHECKREC.AA.OVERPAYMENT
*-------------------------------------------------
*Description: This routine is to throw the validation error messages.
*-------------------------------------------------


$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.REDO.AA.OVERPAYMENT


  IF V$FUNCTION EQ 'I' THEN
    GOSUB PROCESS
  END

  RETURN
*-------------------------------------------------
PROCESS:
*-------------------------------------------------


  IF R.NEW(REDO.OVER.STATUS) NE 'PENDIENTE' THEN
    E     = 'EB-REDO.NOT.ALLOWED'
  END ELSE
    R.NEW(REDO.OVER.STATUS) = "REVERSADO"
  END

  RETURN

END

