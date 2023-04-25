*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
  SUBROUTINE E.ISO.RET.SPF.STATUS(Y.ID.LIST)
*Modification History
*  Date       Who             Reference       Description
* 24 Aug 2011 Balagurunathan ODR-2010-08-0469 added the value to Unique transaction code for issue PACS00084788
*-------------------------------------------------------------------------
  $INSERT I_COMMON
  $INSERT I_EQUATE
  $INSERT I_ENQUIRY.COMMON
  $INSERT I_F.SPF
$INSERT I_AT.ISO.COMMON
$INSERT I_ATM.BAL.ENQ.COMMON
  Y.ID.LIST = "STATUS:1:1=":R.SPF.SYSTEM<SPF.OP.MODE>
  Y.ID.LIST:=',UNIQUE.TXN.CODE:1:1=1'
  Y.ID.LIST := ',Y.ISO.RESPONSE:1:1='

  IF AT$INCOMING.ISO.REQ(3)[1,2]='94' THEN
    Y.UNIQUE.ID=AT$INCOMING.ISO.REQ(38)
    CALL V.FT.UPD.ENQ.ATM.KEY.ID

  END
  IF R.SPF.SYSTEM<SPF.OP.MODE> EQ 'O' THEN
    Y.ID.LIST := '00'
  END ELSE
    Y.ID.LIST := '01'
  END

  RETURN
END
