*========================================================================
SUBROUTINE LAPAP.DEL.CUS.BEN.LIST
*========================================================================
* Technical report:
* =================
* Company Name   : APAP
* Program Name   : LAPAP.DEL.CUS.BEN.LIST
* Date           : 2019-07-24
* Item ID        : --------------
*========================================================================
* Brief description :
* -------------------
* Delete from "FBNK.CUS.BEN.LIST" table X relationated beneficiary with
* the customer's record.
*========================================================================
* Modification History :
* ======================
* Date           Author            Modification Description
* -------------  -----------       ---------------------------
* 2018-07-24     Richard HC         Initial Development
*========================================================================
* Content summary :
* =================
* Table name     :CUSTOMER
* Auto Increment :N/A
* Views/versions :ALL VERSION TO REQUIRED IT
* EB record      :LAPAP.DEL.CUS.BEN.LIST
* Routine        :LAPAP.DEL.CUS.BEN.LIST
*========================================================================


    $INSERT I_COMMON
    $INSERT I_EQUATE

    $INSERT I_F.BENEFICIARY

    FN.BENEFICIARY = "F.BENEFICIARY"
    F.BENEFICIARY = ""
    CALL OPF(FN.BENEFICIARY,F.BENEFICIARY)

    FN.BEN.LIST = "FBNK.CUS.BEN.LIST"
    F.BEN.LIST = ""
    CALL OPF(FN.BEN.LIST,F.BEN.LIST)

    TYPE = R.NEW(ARC.BEN.TRANSACTION.TYPE)
    ID = R.NEW(ARC.BEN.OWNING.CUSTOMER)
    VERSION.BENEDICIARY = ID.NEW


*   TYPE = "AC14"

    IF TYPE EQ "AC14" THEN
        ID = ID:"-OWN"
    END ELSE
        ID = ID:"-OTHER"
    END

    CALL F.READ(FN.BEN.LIST,ID,R.BEN.LIST,F.BEN.LIST,ERR)

    CHANGE '^' TO @FM IN R.BEN.LIST

    M.VAR = DCOUNT(R.BEN.LIST,@FM)

    FOR A = 1 TO M.VAR STEP 1

        RECORD = R.BEN.LIST<A>
        CHANGE '*' TO @VM IN RECORD
        MN = DCOUNT(RECORD,@VM)
        BENEFICIARY = RECORD<1,MN>

        IF BENEFICIARY EQ VERSION.BENEDICIARY THEN

            RECORD = ""

        END ELSE
            ARR.BENEFICIARY<-1> = RECORD
        END

        CHANGE @VM TO '*' IN ARR.BENEFICIARY


    NEXT A

*   DEBUG
    CALL F.WRITE(FN.BEN.LIST,ID,ARR.BENEFICIARY)

END
