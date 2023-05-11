SUBROUTINE REDO.FC.S.CL.OVERDS
*------------------------------------------------------------------------------------------------------------------
* Developer    : jvalarezoulloa@temenos.com
* Date         : 2011-11-23
* Description  : This routine validate if all Mandatory doc were recived
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
* 1.1              2011-11-23    Jorge Valarezo   First Version
*------------------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE

*
*
    $INSERT I_F.REDO.CREATE.ARRANGEMENT


    GOSUB INITIALISE
    GOSUB OPENFILES
    GOSUB PROCESS

RETURN
*==================
INITIALISE:
*==================

RETURN

*==================
OPENFILES:
*==================

RETURN
*==================
PROCESS:
*==================
    R.NEW(REDO.FC.OVERRIDE)=""
RETURN

END
