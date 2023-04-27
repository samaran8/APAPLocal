*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.VAL.REINV.CANC
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.AZ.ACCOUNT

    VAR.ID  = ID.NEW

    FN.AZ.ACCOUNT = 'F.AZ.ACCOUNT'
    F.AZ.ACCOUNT = ''
    CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)

    CALL F.READ(FN.AZ.ACCOUNT,VAR.ID,R.AZ.ACCOUNT,F.AZ.ACCOUNT,YERR)

    VAR.CATEG  =  R.AZ.ACCOUNT<AZ.CATEGORY>
    VAR.ROLLOVER = R.AZ.ACCOUNT<AZ.ROLLOVER.DATE>

    IF  VAR.CATEG EQ 6614 OR VAR.CATEG  EQ 6615  THEN

        IF VAR.ROLLOVER EQ '' THEN

            TEXT="L.APAP.REINV"

            CURR.NO=1

            CALL STORE.OVERRIDE(CURR.NO)

        END


    END

    RETURN
