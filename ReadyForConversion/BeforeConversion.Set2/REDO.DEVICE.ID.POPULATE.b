*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
  SUBROUTINE REDO.DEVICE.ID.POPULATE
*-----------------------------------------------------------------------------
*DESCRIPTION:
*------------
*This routine is used to populate the value in the field Device ID based on the ID of ATM.BRANCH
*------------------------------------------------------------------------------------------
* Input/Output:
*--------------
* IN  : -NA-
* OUT : -NA-
*
* Dependencies:
*---------------
* CALLS : -NA-
* CALLED BY : -NA-
*
* Revision History:
*------------------
*   Date               who           Reference            Description
* 11-05-2011           Prabhu.N      PACS00054730        Initial Creation
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.ATM.BRANCH
  R.NEW(ATM.BR.DEVICE.ID)=ID.NEW[7,2]
  RETURN
END
