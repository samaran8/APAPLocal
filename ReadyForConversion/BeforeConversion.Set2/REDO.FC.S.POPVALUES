*-----------------------------------------------------------------------------
* <Rating>-31</Rating>
*-----------------------------------------------------------------------------
  SUBROUTINE REDO.FC.S.POPVALUES(ID.PARAMS.IN)

*
* Subroutine Type : ROUTINE
* Attached to     : ROUTINE REDO.CREATE.ARRANGEMENT.RECORD
* Attached as     : ROUTINE
* Primary Purpose : Get defaults values from REDO.FC.PARAMS and populate them in R.NEW
*
* Incoming:
* ---------
*
*
* Outgoing:
* ---------
*
*
* Error Variables:
*
*-----------------------------------------------------------------------------------
* Modification History:
*
* Development for : Asociacion Popular de Ahorros y Prestamos
* Development by  : Juan Pablo Armas - TAM Latin America
* Date            : 30 Jun 2011
*
*-----------------------------------------------------------------------------------

$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.REDO.CREATE.ARRANGEMENT
$INSERT I_F.REDO.FC.PARAMS

  GOSUB INITIALISE
  GOSUB OPEN.FILES

  IF PROCESS.GOAHEAD THEN
    GOSUB PROCESS
  END

  RETURN  ;* Program RETURN
*-----------------------------------------------------------------------------------
PROCESS:
*============

  CALL CACHE.READ (FN.REDO.FC.PARAMS,ID.PARAMS,R.REDO.FC.PARAMS, YERR)
  IF R.REDO.FC.PARAMS THEN
    YNRO = DCOUNT(R.REDO.FC.PARAMS<FC.PR.FIELD.NAME>,VM)
    FOR I = 1 TO YNRO
      YFIELD = R.REDO.FC.PARAMS<FC.PR.FIELD.NAME,I>
      YVALUE = R.REDO.FC.PARAMS<FC.PR.FIELD.VALUE,I>
      IF YVALUE EQ "TODAY" THEN
        YVALUE = TODAY
      END
      CALL EB.FIND.FIELD.NO(Y.APLICACION, YFIELD)
      IF NOT(R.NEW(YFIELD)) THEN
        R.NEW(YFIELD)= YVALUE
      END
    NEXT
  END

  RETURN

*------------------------
INITIALISE:
*=========

  FN.REDO.FC.PARAMS = 'F.REDO.FC.PARAMS'
  ID.PARAMS = ID.PARAMS.IN
  R.REDO.FC.PARAMS = ''
  YERR = ''
  Y.APLICACION = "REDO.CREATE.ARRANGEMENT"

  PROCESS.GOAHEAD = 1
  RETURN

*------------------------
OPEN.FILES:
*=========

  RETURN
*------------
END
