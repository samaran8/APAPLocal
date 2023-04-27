*-----------------------------------------------------------------------------
* <Rating>100</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.ARC.ACCT.REST.RT
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.ACCOUNT
    FN.AC = "F.ACCOUNT"
    F.AC = ""
    FN.AC.ARC = "FBNK.ACCOUNT$ARC"

    P.ACCOUNT.ID = ID.NEW.LAST          ;*COMI
**DEBUG
    SEL.CMD = 'SELECT ': FN.AC.ARC :' WITH @ID LIKE ' : P.ACCOUNT.ID  : ';... '
    EXECUTE SEL.CMD CAPTURING OUTPUT
    READLIST REC.LIST ELSE REC.LIST = ''

    IF REC.LIST <> '' THEN
        FOR REC.IDX = 1 TO DCOUNT(REC.LIST,@FM)
            EXECUTE 'COPY FROM FBNK.ACCOUNT$ARC TO FBNK.ACCOUNT$HIS ' : REC.LIST<REC.IDX> : ' OVERWRITING'
        NEXT REC.IDX
    END

    RETURN
END
