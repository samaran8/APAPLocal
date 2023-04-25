*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.PAGOS.CXP.DELETEF
  
    EXECUTE 'COPY FROM ../interface/FLAT.INTERFACE/PAGOS.CXP PAGOS.CXP.TXT TO ../interface/FLAT.INTERFACE/PAGOS.CXP/TEMP OVERWRITING DELETING'

    RETURN

END
