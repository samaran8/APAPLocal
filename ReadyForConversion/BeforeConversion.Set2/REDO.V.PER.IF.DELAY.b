*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
  SUBROUTINE  REDO.V.PER.IF.DELAY
*----------------------------------------------------------------------------------------------------
*DESCRIPTION : This routine is used to validate the REDO.LY.PROGRAM table fields
*-----------------------------------------------------------------------------------------------------
*-----------------------------------------------------------------------------------------------------
* * Input / Output
* --------------
* IN     : -NA-
* OUT    : -NA-
*-----------------------------------------------------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : RMONDRAGON
* PROGRAM NAME : REDO.V.PER.IF.DELAY
*-----------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
*DATE             WHO                REFERENCE         DESCRIPTION
*19.07.2011    RMONDRAGON         ODR-2011-06-0243       UPDATE
* -----------------------------------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.REDO.LY.PROGRAM
$INSERT I_GTS.COMMON

  GOSUB INIT
  GOSUB PROCESS
  RETURN

*---
INIT:
*---
  FN.REDO.LY.PROGRAM='F.REDO.LY.PROGRAM'
  F.REDO.LY.PROGRAM=''
  CALL OPF(FN.REDO.LY.PROGRAM,F.REDO.LY.PROGRAM)
  RETURN
*-------
PROCESS:
*-------
  VAR.PER.IF.DELAY = R.NEW(REDO.PROG.PER.IF.DELAY)

  IF VAR.PER.IF.DELAY NE '' AND VAR.PER.IF.DELAY LT '0' OR VAR.PER.IF.DELAY GT '100' THEN
    AF = REDO.PROG.PER.IF.DELAY
    ETEXT = 'EB-LY.PERIFDELAY'
    CALL STORE.END.ERROR
  END

  RETURN
*------------------------------------------------------------------------------------
END
