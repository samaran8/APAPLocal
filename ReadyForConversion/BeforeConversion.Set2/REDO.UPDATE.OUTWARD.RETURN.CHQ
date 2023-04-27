*-----------------------------------------------------------------------------
* <Rating>-10</Rating>
*-----------------------------------------------------------------------------
  SUBROUTINE REDO.UPDATE.OUTWARD.RETURN.CHQ(ACC.NO,Y.CHQ.ID)
*------------------------------------------------------------------------
*Description: This routine is to update the return chq of each account.
*------------------------------------------------------------------------

$INSERT I_COMMON
$INSERT I_EQUATE

  GOSUB PROCESS
  RETURN
*------------------------------------------------------------------------
PROCESS:
*------------------------------------------------------------------------
  IF ACC.NO EQ '' OR Y.CHQ.ID EQ '' THEN
    RETURN
  END

  FN.REDO.OUTWARD.RETURN.CHQ = 'F.REDO.OUTWARD.RETURN.CHQ'
  F.REDO.OUTWARD.RETURN.CHQ  = ''
  CALL OPF(FN.REDO.OUTWARD.RETURN.CHQ,F.REDO.OUTWARD.RETURN.CHQ)

  CALL F.READU(FN.REDO.OUTWARD.RETURN.CHQ,ACC.NO,R.REDO.OUTWARD.RETURN.CHQ,F.REDO.OUTWARD.RETURN.CHQ,RET.ERR,'')
  LOCATE Y.CHQ.ID IN R.REDO.OUTWARD.RETURN.CHQ<1> SETTING POS1 ELSE
    R.REDO.OUTWARD.RETURN.CHQ<-1> = Y.CHQ.ID
    CALL F.WRITE(FN.REDO.OUTWARD.RETURN.CHQ,ACC.NO,R.REDO.OUTWARD.RETURN.CHQ)
  END
  CALL F.RELEASE(FN.REDO.OUTWARD.RETURN.CHQ,ACC.NO,F.REDO.OUTWARD.RETURN.CHQ)

  RETURN
END
