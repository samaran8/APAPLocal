*-----------------------------------------------------------------------------
* <Rating>-10</Rating>
*-----------------------------------------------------------------------------
     SUBROUTINE AT.ISO.CHG.AMT(IN.ARG,OUT.ARG)
 *Developed by s.anitha
 *accept the amount from field 28 and format it to send only the ccy and the amount

*     $INCLUDE T24.BP I_COMMON	;*/ TUS START
*     $INCLUDE T24.BP I_EQUATE
*     $INCLUDE T24.BP I_F.NUMERIC.CURRENCY
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.NUMERIC.CURRENCY	;*/  TUS END

     GOSUB PROCESS

     RETURN
 *-------------------------------------------------
 PROCESS:
 *---------
     FN.NUMERIC.CURRENCY='F.NUMERIC.CURRENCY'
     F.NUMERIC.CURRENCY=''
     CALL OPF(FN.NUMERIC.CURRENCY,F.NUMERIC.CURRENCY)
     CCY.CDE=FIELD(IN.ARG,'%',1)
     CHG.AMT=FIELD(IN.ARG,'%',2)


     CALL F.READ(FN.NUMERIC.CURRENCY,CCY.CDE,R.NUMERIC.CURRENCY,F.NUMERIC.CURRENCY,E.NUMERIC.CURRENCY)

     ACTUAL.CCY=R.NUMERIC.CURRENCY<EB.NCN.CURRENCY.CODE>

 CHG.AMT=CHG.AMT/100


 IF CHG.AMT NE '0' THEN
     OUT.ARG=ACTUAL.CCY:CHG.AMT
 END
     RETURN
 *----------------------------------------
 END
