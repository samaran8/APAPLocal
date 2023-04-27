*-----------------------------------------------------------------------------
* <Rating>100</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.ARC.CUST.REST.RT
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.CUSTOMER
    FN.AC = "F.CUSTOMER"
    F.AC = ""
    FN.CUS.ARC = "FBNK.CUSTOMER$ARC"

    P.CUSTOMER.ID = ID.NEW.LAST         ;*COMI
    *DEBUG
    SEL.CMD = 'SELECT ': FN.CUS.ARC :' WITH @ID LIKE ' : P.CUSTOMER.ID  : ';... '
    EXECUTE SEL.CMD CAPTURING OUTPUT
    READLIST REC.LIST ELSE REC.LIST = ''

    IF REC.LIST <> '' THEN
        FOR REC.IDX = 1 TO DCOUNT(REC.LIST,@FM)
            EXECUTE 'COPY FROM FBNK.CUSTOMER$ARC TO FBNK.CUSTOMER$HIS ' : REC.LIST<REC.IDX> : ' OVERWRITING'
        NEXT REC.IDX
    END

    RETURN
END
