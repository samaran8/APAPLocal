*-----------------------------------------------------------------------------
* <Rating>-11</Rating>
*-----------------------------------------------------------------------------
  SUBROUTINE REDO.PRE.PRIN.INT.PENAL.INT.SELECT
*-----------------------------------------------------------------------------

*DESCRIPTION:
*------------
* This is the COB routine for the B16 development and this is Select routine
*
* Input/Output:
*--------------
* IN : -NA-
* OUT : -NA-
*---------------
*-----------------------------------------------------------------------------
* Modification History :
*   Date            Who             Reference            Description
* 02-JUL-2010    Kishore.SP      ODR-2009-10-0325      Initial Creation
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_GTS.COMMON
$INSERT I_BATCH.FILES
$INSERT I_REDO.PRE.PRIN.INT.PENAL.INT.COMMON
*-----------------------------------------------------------------------------
!
  GOSUB PROCESS
  RETURN
!
PROCESS:
*-------
* Select the Arrangements
!

  SELECT.CMD = "SELECT ":FN.AA.ARRANGEMENT:" WITH ARR.STATUS EQ AUTH OR ARR.STATUS EQ CURRENT "
  CALL EB.READLIST(SELECT.CMD,SEL.LIST,'',NO.REC,PGM.ERR)
  CALL BATCH.BUILD.LIST('',SEL.LIST)
  RETURN
END
