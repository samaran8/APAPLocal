*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE L.APAP.FORMAT.AMT(Y.INP.DEAL)
$INCLUDE T24.BP I_COMMON 
$INCLUDE T24.BP I_EQUATE
    
    Y.AMOUNT = FMT(Y.INP.DEAL, "L2,")
    Y.INP.DEAL = FMT( "RD$":Y.AMOUNT, "15R")
    
    RETURN 
END
