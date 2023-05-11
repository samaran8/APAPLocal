*-----------------------------------------------------------------------------
* <Rating>-27</Rating>
*-----------------------------------------------------------------------------
  SUBROUTINE REDO.S.CHK.AC.NOTIF.OVR(Y.NOTIFY.LIST, Y.NOTIFICATION.FLAG)
*-------------------------------------------------------------------------
* Company Name  : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By  : Deepak Kumar K
* Program Name  : REDO.S.CHK.AC.NOTIF.OVR
****-------------------------------------------------------------------------
* Description: Check whether notification in account contains override class or not.
*
***----------------------------------------------------------
* Linked with:
* In parameter :
* out parameter : None
*------------------------------------------------------------------------
* MODIFICATION HISTORY
*--------------------------------------------
*   DATE              ODR                             DESCRIPTION
* 24.08.2013         Mantis 4817                 Initial Creation
*------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.LOOKUP
$INSERT I_F.OVERRIDE
$INSERT I_F.REDO.NOTIFY.STATUS.MESSAGE

* Variables
  Y.LOOKUP.PREFIX = 'L.AC.NOTIFY.1'
  Y.NOTIFICATION.FLAG = 'FALSE'

* Open files
  FN.EB.LOOKUP = 'F.EB.LOOKUP'
  F.EB.LOOKUP = ''
  CALL OPF(FN.EB.LOOKUP , F.EB.LOOKUP)

  FN.REDO.NOTIFY.STATUS.MESSAGE = 'F.REDO.NOTIFY.STATUS.MESSAGE'
  F.REDO.NOTIFY.STATUS.MESSAGE = ''
  CALL OPF(FN.REDO.NOTIFY.STATUS.MESSAGE, F.REDO.NOTIFY.STATUS.MESSAGE)

  FN.OVERRIDE = 'F.OVERRIDE'
  F.OVERRIDE = ''
  CALL OPF(FN.OVERRIDE, F.OVERRIDE)

* Read parameter
  Y.PARAM.ID = 'ACCOUNT'
  R.PARAM = ''
  Y.READ.ERR = ''
  CALL CACHE.READ(FN.REDO.NOTIFY.STATUS.MESSAGE, Y.PARAM.ID, R.PARAM, Y.READ.ERR)

  Y.OVR.MSG.LIST = R.PARAM<REDO.NOTIF.OVERRIDE.MSG>
  Y.NOTIFY.MSG.LIST = R.PARAM<REDO.NOTIF.NOTIFY.MSG>

* check each notification set in the account
  LOOP
    REMOVE Y.NOTIFY.ID FROM Y.NOTIFY.LIST SETTING Y.NOTIFY.POS
  WHILE Y.NOTIFY.ID:Y.NOTIFY.POS

* Form Eb.lookup id and read
    Y.LOOKUP.ID = Y.LOOKUP.PREFIX:'*':Y.NOTIFY.ID

*        Y.READ.ERR =  ''
*        R.LOOKUP.REC = ''
*        CALL F.READ(FN.EB.LOOKUP , Y.LOOKUP.ID, R.LOOKUP.REC, F.EB.LOOKUP, Y.READ.ERR)

* If unreadable skip to next id
*        IF R.LOOKUP.REC THEN
    GOSUB LOOKUP.PROCESS
*        END

  REPEAT

  RETURN
*----------------------------------------------------
LOOKUP.PROCESS:
*--------------

* Get Description from Eb.Lookup
*    Y.LOOKUP.DESC = R.LOOKUP.REC<EB.LU.DESCRIPTION, LNGG>
*
*    IF Y.LOOKUP.DESC ELSE
*        Y.LOOKUP.DESC = R.LOOKUP.REC<EB.LU.DESCRIPTION, 1>
*    END

* Check whether this LOOKUP ID is present in parameter else return
  LOCATE Y.NOTIFY.ID IN Y.NOTIFY.MSG.LIST<1,1> SETTING Y.PARAM.POS THEN

* If found get override id.
    Y.OVERRIDE.ID = Y.OVR.MSG.LIST<1, Y.PARAM.POS>

    Y.READ.ERR= ''
    R.OVERRIDE = ''
    CALL F.READ(FN.OVERRIDE, Y.OVERRIDE.ID, R.OVERRIDE, F.OVERRIDE, Y.READ.ERR)

    LOCATE 'ACCOUNT' IN R.OVERRIDE<EB.OR.APPLICATION, 1> SETTING Y.OVR.POS THEN
      Y.NOTIFICATION.FLAG = 'YES'
    END
  END


  RETURN
*----------------------------------------------------
* End of Program
END
