*-----------------------------------------------------------------------------
* <Rating>90</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE L.APAP.VAL.ADITIONAL.TD

$INSERT T24.BP I_COMMON
$INSERT T24.BP I_EQUATE
$INSERT TAM.BP I_F.LATAM.CARD.ORDER

*--------------------------------------------------------------------------------------------------------
*--------------------------------------------------------------------------------------------------------
*Description  : 
*Linked With  : LATAM.CARD.ORDER,REDO.ADDITIONAL
*In Parameter : N/A
*Out Parameter: N/A
*--------------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
*    Date            Who                  Reference               Description
*   ------         ------               -------------            -------------
* 22 06 2020    Estalin Valerio           Proyecto Digitalizacion   Initial Creation
*--------------------------------------------------------------------------------------------------------


GOSUB PROCESS
RETURN

PROCESS:
*--------
  
  IF R.NEW(CARD.IS.TYPE.OF.CARD) EQ "ADICIONAL" AND LEN(R.NEW(CARD.IS.PROSPECT.ID)) EQ 0 THEN
  
      *AF=CARD.IS.TYPE.OF.CARD
      *ETEXT='EB-NO.ADIT.CARD'
      *CALL STORE.END.ERROR
      
      TEXT="L.APAP.VAL.TD.ADI"
      CURR.NO=1
      CALL STORE.OVERRIDE(CURR.NO)
      RETURN

  END 
  ELSE IF R.NEW(CARD.IS.TYPE.OF.CARD) EQ "PRINCIPAL" AND LEN(R.NEW(CARD.IS.PROSPECT.ID)) NE 0 THEN
      TEXT="L.APAP.VAL.TD.PRIN"
      CURR.NO=1
      CALL STORE.OVERRIDE(CURR.NO)
      RETURN
  END

RETURN
END
