*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.AVOID.GARNISHMENT.FLD

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT TAM.BP I_F.APAP.H.GARNISH.DETAILS


    FN.GARNISH ="F.APAP.H.GARNISH.DETAILS"
    F.GARNISH = ""

    CALL OPF(FN.GARNISH,F.GARNISH)

    GRAMT = R.NEW(APAP.GAR.GARNISH.AMT)

    M = DCOUNT(GRAMT,@VM)
    FOR A = 1 TO M STEP 1
        IF GRAMT ! '' THEN

            R.NEW(APAP.GAR.GARNISH.AMT)<1,A> = 0.00
            R.NEW(APAP.GAR.AMOUNT.LOCKED)<1,A> = 0
        END
    NEXT A

END
