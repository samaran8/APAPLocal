* Version 2 03/04/00  GLOBUS Release No. G10.2.01 25/02/00
*-----------------------------------------------------------------------------
* <Rating>51</Rating>
*-----------------------------------------------------------------------------
      SUBROUTINE ATM.PARAMETER.OVERRIDE
************************************************************************
*
* Routine to process the overrides for a XXXX contract
*
************************************************************************
* XX/XX/XX - GBXXXXXXX
*            Pif Description
*
************************************************************************

************************************************************************
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.ATM.PARAMETER
$INSERT I_F.INTERCO.PARAMETER
************************************************************************
************************************************************************

      GOSUB INITIALISE

      GOSUB PROCESS.OVERRIDES

      RETURN
*
************************************************************************
*
PROCESS.OVERRIDES:

*
* Place Overrides Here
*
* Set text to be the key to the override you
* want to use form the OVERRIDE file
*
* Set AF/AV/AS to be the Field you wish GLOBUS
* to return to if the user rejects the override
*
* AF = XX.RETURN.FIELD
* TEXT = "SAMPLE.OVERRIDE.KEY"
* GOSUB DO.OVERRIDE
*
      Y.ACCT.NO.LENGHT = 0
      CALL DBR('INTERCO.PARAMETER':FM:ST.ICP.ACCOUNT.NO.LENGTH,'SYSTEM',Y.ACCT.NO.LENGTH)
      IF ETEXT EQ '' AND Y.ACCT.NO.LENGTH NE R.NEW(ATM.PARA.ACCT.NO.LEN) THEN
         AF = ATM.PARA.ACCT.NO.LEN
         TEXT = 'ACCT.NO.LENGTH DEFFERS FROM THAT DEFINED IN INTERCO.PARAMETER'
         GOSUB DO.OVERRIDE
      END
      RETURN
*
************************************************************************
*
DO.OVERRIDE:
      CALL STORE.OVERRIDE(CURR.NO)
      IF TEXT = 'NO' THEN
         GOTO PROGRAM.ABORT
      END
      RETURN
*
*************************************************************************

*************************************************************************
*
INITIALISE:

      CURR.NO = 0
      CALL STORE.OVERRIDE(CURR.NO)

      RETURN
*
*************************************************************************
*
* If the user said no, get the hell out..
*
PROGRAM.ABORT:

      RETURN TO PROGRAM.ABORT
      RETURN
*
*************************************************************************
*
   END
