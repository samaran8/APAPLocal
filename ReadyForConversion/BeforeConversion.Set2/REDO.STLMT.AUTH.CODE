*-------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* <Rating>-10</Rating>
*-----------------------------------------------------------------------------
  SUBROUTINE  REDO.STLMT.AUTH.CODE
*--------------------------------------------------------------------------
*Company Name      : APAP Bank
*Developed By      : Temenos Application Management
*Program Name      : REDO.STLMT.AUTH.CODE
*Date              : 23.11.2010
*-------------------------------------------------------------------------
* Incoming/Outgoing Parameters
*-------------------------------
* In  : --N/A--
* Out : --N/A--
*-----------------------------------------------------------------------------
* Revision History:
* -----------------
* Date                   Name                   Reference               Version
* -------                ----                   ----------              --------
*23/11/2010      saktharrasool@temenos.com   ODR-2010-08-0469       Initial Version
*------------------------------------------------------------------------------------

$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_REDO.VISA.STLMT.FILE.PROCESS.COMMON
$INSERT I_F.LATAM.CARD.ORDER
$INSERT I_F.REDO.CARD.BIN



  IF ERROR.MESSAGE NE '' THEN
    RETURN
  END


  GOSUB INIT
  RETURN

*------------------------------------------------------------------------------------
INIT:
*------------------------------------------------------------------------------------


  CALL REDO.STLMT.VAL.DELAY
  Y.FIELD.VALUE=TRIM(Y.FIELD.VALUE,' ','B')
  IF Y.FIELD.VALUE EQ '' THEN
    ERROR.MESSAGE="NO.AUTH.CODE"
  END ELSE
    AUTH.CODE=Y.FIELD.VALUE
  END
  RETURN
END
