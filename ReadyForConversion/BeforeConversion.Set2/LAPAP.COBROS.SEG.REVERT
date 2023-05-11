*========================================================================
*-----------------------------------------------------------------------------
* <Rating>-2</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.COBROS.SEG.REVERT
*========================================================================
* Technical report:
* =================
* Company Name   : APAP
* Program Name   : LAPAP.COBROS.SEG.REVERT
* Date           : 2018-07-03
* Item ID        : --------------
*========================================================================
* Brief description :
* -------------------
* This program allow revers record with pending status in FUNDS.TRANSFER
*========================================================================
* Modification History :
* ======================
* Date           Author            Modification Description
* -------------  -----------       ---------------------------
* 2018-07-03     Richard HC         Initial Development
*========================================================================
* Content summary :
* =================
* Table name     :F.FUNDS.TRANSFER$NAU
* Auto Increment :N/A
* Views/versions :LAPAP.ENQ.COBROS.SEG.REVERT
* EB record      :N/A
* Routine        :LAPAP.COBROS.SEG.REVERT
*========================================================================


    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.FUNDS.TRANSFER

    FN.FUND = "F.FUNDS.TRANSFER$NAU"
    F.FUND = ""
    CALL OPF(FN.FUND,F.FUND)

    QUERY = "SELECT FBNK.FUNDS.TRANSFER$NAU WITH ORDERING.BANK EQ 'COBRO SEGUROS'"

    CALL EB.READLIST(QUERY,SEL.LIST,'',NO.OF.REC,RET.CODE)
    LOOP
        REMOVE Y.TEMP.ID FROM SEL.LIST SETTING TEMP.POS
    WHILE Y.TEMP.ID

        APP = "FUNDS.TRANSFER"
        ID = Y.TEMP.ID
        Y.FUNC = "D"
        *RSS<FT.CREDIT.AMOUNT> = 1234
        RSS = ""
        CALL LAPAP.BUILD.OFS.LOAD(APP,Y.FUNC,ID,RSS)

    REPEAT

    RETURN

END
