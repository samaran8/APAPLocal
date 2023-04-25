*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE L.APAP.OP.CANALES.SF.DELETEF
$INCLUDE T24.BP I_F.DATES
EXECUTE 'COPY FROM ../interface/FLAT.INTERFACE/TRANSF.OP.CANALES.SF TRANSF.OP.CANALES.SF.TXT TO ../interface/FLAT.INTERFACE/TRANSF.OP.CANALES.SF/TEMP OVERWRITING DELETING'
RETURN
END
 
