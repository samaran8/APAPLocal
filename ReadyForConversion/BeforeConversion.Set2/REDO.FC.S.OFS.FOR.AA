*-----------------------------------------------------------------------------
* <Rating>-82</Rating>
*-----------------------------------------------------------------------------
  SUBROUTINE REDO.FC.S.OFS.FOR.AA (ID.APP.MAPPING, OFS.STR)

*
* Subroutine Type : ROUTINE
* Attached to     : ROUTINE REDO.CREATE.ARRANGEMENT.AUTHORISE
* Attached as     : ROUTINE
* Primary Purpose : Build OFS for AA
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
* Date            : 01 Julio 2011
*
*-----------------------------------------------------------------------------------

$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.REDO.CREATE.ARRANGEMENT
$INSERT I_F.REDO.APP.MAPPING

  GOSUB INITIALISE
  GOSUB OPEN.FILES

  IF PROCESS.GOAHEAD THEN
    GOSUB PROCESS.MAIN
  END

  RETURN  ;* Program RETURN
*-----------------------------------------------------------------------------------
PROCESS.MAIN:
*=============
  CALL CACHE.READ(FN.REDO.APP.MAPPING, Y.IDAPP, R.REDO.APP.MAPPING, YERR)
  Y.APLICACION.FROM = R.REDO.APP.MAPPING<REDO.APP.APP.FROM>
  Y.APLICACION.TO = R.REDO.APP.MAPPING<REDO.APP.APP.TO>
  NRO.FIELDS = DCOUNT(R.REDO.APP.MAPPING<REDO.APP.FIELD.FROM>,VM)

  GOSUB SET.OFS.HEADER
  FOR I =1 TO NRO.FIELDS
    YFIELDFROM = R.REDO.APP.MAPPING<REDO.APP.FIELD.FROM,I>
    YFIELDTO =  R.REDO.APP.MAPPING<REDO.APP.FIELD.TO,I>
    GOSUB SET.YFIELDFROM
    GOSUB SET.OFS.BODY
  NEXT
  OFS.STR = OFS.STR.HEAD:OFS.STR.BODY
  RETURN

*------------------------
SET.YFIELDFROM:
*================
  YOP = FIELD(YFIELDFROM,"*",1)
  BEGIN CASE
  CASE YOP EQ "VAR"
    YREST = FIELD(YFIELDFROM,"*",2)
  CASE YOP EQ "STR"
    YREST = FIELD(YFIELDFROM,"*",2)
    YFIELDFROM = YREST
  CASE 1
    GOSUB SET.VALUE.FLD
  END CASE

  RETURN
*------------------
SET.VALUE.FLD:
*===============
  GOSUB GETPOSVALFLD
  YFIELDFROM = R.NEW(Y.FIELD.NO)
  RETURN
*----------------------
GETPOSVALFLD:
*===============
*Campos de la aplicacion
  Y.FIELD.NO = YFIELDFROM
  CALL EB.FIND.FIELD.NO(Y.APLICACION.FROM, Y.FIELD.NO)
  RETURN
*------------------------
SET.OFS.HEADER:
*=============
  OFS.STR.HEAD  = Y.APLICACION.TO
  OFS.STR.HEAD := ",AA.APAP/I/PROCESS//0"
  OFS.STR.HEAD := ",/"
  OFS.STR.HEAD := ","
  RETURN
*------------------------
SET.OFS.BODY:
*=============
  OFS.STR.BODY := ","
  OFS.STR.BODY := YFIELDTO
  OFS.STR.BODY := ":1:1="
  OFS.STR.BODY := YFIELDFROM
  RETURN

*------------------------
INITIALISE:
*=========

  FN.REDO.APP.MAPPING = 'F.REDO.APP.MAPPING'
  R.REDO.APP.MAPPING  = ''

  Y.IDAPP = ID.APP.MAPPING
  OFS.STR.BODY  = ''
  YERR = ''
  PROCESS.GOAHEAD = 1
  RETURN

*------------------------
OPEN.FILES:
*=========

  RETURN
*------------
END
