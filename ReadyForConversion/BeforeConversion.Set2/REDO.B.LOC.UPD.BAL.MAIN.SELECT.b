*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
  SUBROUTINE REDO.B.LOC.UPD.BAL.MAIN.SELECT
*---------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :Marimuthu S
*Program   Name    :REDO.B.LOC.UPD.BAL.MAIN.SELECT
*----------------------------------------------------------------------------------
*DESCRIPTION       : This is multi-threaded cob routine will fetch the arrangement id from
*                    local template and make charge with scheduled payment for the loan
*---------------------------------------------------------------------------------
*Modification Details:
*=====================
*   Date               who             Reference            Description
* 26-JAN-2012       MARIMUTHU S       PACS00170057         Initial Creation
*---------------------------------------------------------------------------------

$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_REDO.B.LOC.UPD.BAL.MAIN.COMMON


  SEL.CMD = 'SELECT ':FN.REDO.CONC.NEXT.ARR.BILL
  CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,SEL.ERR)
  CALL BATCH.BUILD.LIST('',SEL.LIST)

  RETURN

END
