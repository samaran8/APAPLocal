*-----------------------------------------------------------------------------
* <Rating>-22</Rating>
*-----------------------------------------------------------------------------
  SUBROUTINE REDO.SYS.DATE.FORMAT(Y.OUT)
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :Sudharsanan S
*Program   Name    :REDO.SYS.DATE.FORMAT
*---------------------------------------------------------------------------------

*DESCRIPTION       :This program is used to get the date and Convert the date into
*                   dd mon yy (e.g. 01 JAN 09)
*LINKED WITH       :
* ----------------------------------------------------------------------------------
*----------------------------------------------------------------------------------
* Date             Author             Reference                   Description
* 14-MAR-2012    V�ctor Panchi        PACS00172914              Print month in Spanish
*-----------------------------------------------------------------------------

$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.TELLER

  GOSUB PROCESS
  RETURN

PROCESS:
  Y.DATE = R.NEW(TT.TE.VALUE.DATE.1)

* S - PACS00172914
* VAR.DATE=ICONV(Y.DATE,"D2")
* Y.OUT=OCONV(VAR.DATE,"D4")
  Y.MONTH = Y.DATE[5,2]
  GOSUB GET.MONTH.SPANISH
* E - PACS00172914
  RETURN

GET.MONTH.SPANISH:
  BEGIN CASE
  CASE Y.MONTH EQ "01"
    Y.MONTH = "ENE"
  CASE Y.MONTH EQ "02"
    Y.MONTH = "FEB"
  CASE Y.MONTH EQ "03"
    Y.MONTH = "MAR"
  CASE Y.MONTH EQ "04"
    Y.MONTH = "ABR"
  CASE Y.MONTH EQ "05"
    Y.MONTH = "MAY"
  CASE Y.MONTH EQ "06"
    Y.MONTH = "JUN"
  CASE Y.MONTH EQ "07"
    Y.MONTH = "JUL"
  CASE Y.MONTH EQ "08"
    Y.MONTH = "AGO"
  CASE Y.MONTH EQ "09"
    Y.MONTH = "SEP"
  CASE Y.MONTH EQ "10"
    Y.MONTH = "OCT"
  CASE Y.MONTH EQ "11"
    Y.MONTH = "NOV"
  CASE Y.MONTH EQ "12"
    Y.MONTH = "DIC"
  END CASE

  Y.OUT = Y.DATE[7,2]: " " : Y.MONTH : " " : Y.DATE[1,4]

  RETURN
END
