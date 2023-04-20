*-----------------------------------------------------------------------------
* <Rating>-52</Rating>
*-----------------------------------------------------------------------------

  SUBROUTINE REDO.FC.S.VALID.COLL

*------------------------------------------------------------------------------------------------------------------
* Developer    : mgudino@temenos.com
* Date         : 2011-06-13
* Description  : Manage the execute way of Validations Rutines
*
*------------------------------------------------------------------------------------------------------------------
* Input/Output:
* -------------
* In  :
* Out :
*------------------------------------------------------------------------------------------------------------------
* Revision History:
* -----------------
* Version          Date          Name              Description
* -------          ----          ----              ------------
* 1.0              2011-06-13    Marcelo Gudi First Version
*------------------------------------------------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.REDO.FC.BH.VALIDATIONS
*
  GOSUB INITIALISE
  GOSUB OPENFILES
  GOSUB PROCESS

  RETURN

*----------
PROCESS:
*----------
*
  GOSUB PROCESS.VERSION
  RETURN

*-----------------
PROCESS.VERSION:
*-----------------
*
  Y.VERSION.NAME = SUBSTRINGS (Y.VERSION.NAME, 2, Y.COUNT.VERSION)
  CALL CACHE.READ(FN.REDO.FC.BH.VALIDATIONS, Y.VERSION.NAME, R.REDO.FC.BH.VALIDATIONS, Y.ERR)

  IF Y.ERR THEN
    ETEXT = "EB-FC-READ.ERROR" : FM : FN.REDO.FC.BH.VALIDATIONS
    CALL STORE.END.ERROR
  END ELSE
    Y.COUNT.VAL = DCOUNT(R.REDO.FC.BH.VALIDATIONS<REDO.BH.NAME.RUTINE>,VM)
* To Iterate in R.REDO.FC.BH.VALIDATIONS records
    FOR Y.I = 1 TO Y.COUNT.VAL
      Y.RUTINE.NAME = R.REDO.FC.BH.VALIDATIONS<REDO.BH.NAME.RUTINE,Y.I>
      IF Y.RUTINE.NAME THEN
        GOSUB PROCESS.ROUTINE
      END
    NEXT
  END

  RETURN

*-----------------
PROCESS.ROUTINE:
*-----------------
*
  CALL @Y.RUTINE.NAME

  RETURN

*------------
INITIALISE:
*------------
*
  YERR = ''
  Y.APP.NAME = ''
  Y.VERSION.NAME = PGM.VERSION
  Y.COUNT.VERSION = LEN(Y.VERSION.NAME)
  Y.RUTINE.NAME = ''
  Y.CONT.VAL = 0

  FN.REDO.FC.BH.VALIDATIONS = 'F.REDO.FC.BH.VALIDATIONS'
  F.REDO.FC.BH.VALIDATIONS = ''


  RETURN
*------------
OPENFILES:
*------------
*   Paragraph that open files
*

  RETURN
*------------
END
