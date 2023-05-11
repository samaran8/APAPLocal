* @ValidationCode : MjoxMjQ3MDA3MjY4OkNwMTI1MjoxNjgyMDgwMzQzMjI0OkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 21 Apr 2023 18:02:23
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.LAPAP
SUBROUTINE L.APAP.GET.BEN.ID.RT
*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
*
* DATE              WHO                REFERENCE                 DESCRIPTION
* 21-APR-2023     Conversion tool    R22 Auto conversion       FM TO @FM, VM to @VM, SM to @SM
* 21-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.BENEFICIARY

    Y.NUM.TXN = COMI

    GOSUB INIT
    GOSUB PROCESS

    COMI = Y.CED.FINAL

RETURN

*---------------
INIT:
*---------------
    Y.CED.FINAL = ''

    APPLS = 'FUNDS.TRANSFER':@FM:'BENEFICIARY'
    F.FIELDS = 'L.ACH.PART.ID':@FM:'L.BEN.CEDULA'

    CALL MULTI.GET.LOC.REF(APPLS,F.FIELDS,POS.VAL)

    POS.ACH.PART.ID = POS.VAL<1,1>
    POS.BEN.CEDULA = POS.VAL<2,1>

    FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER'; F.FUNDS.TRANSFER = ''
    CALL OPF(FN.FUNDS.TRANSFER, F.FUNDS.TRANSFER)

    FN.BENEFICIARY = 'F.BENEFICIARY'; F.BENEFICIARY = ''
    CALL OPF(FN.BENEFICIARY, F.BENEFICIARY)

RETURN

*---------------
PROCESS:
*---------------
    CALL F.READ(FN.FUNDS.TRANSFER, Y.NUM.TXN, R.FUNDS.TRANSFER, F.FUNDS.TRANSFER, ERR.FT)

    IF NOT(ERR.FT) THEN
        Y.ACH.PART.ID = R.FUNDS.TRANSFER<FT.LOCAL.REF,POS.ACH.PART.ID>
        Y.CREDIT.THEIR.REF = R.FUNDS.TRANSFER<FT.CREDIT.THEIR.REF>
        Y.BEN.CUSTOMER = R.FUNDS.TRANSFER<FT.BEN.CUSTOMER>
        Y.BEN.CUSTOMER = CHANGE(Y.BEN.CUSTOMER,@VM,@FM)
        Y.BEN.CUSTOMER = CHANGE(Y.BEN.CUSTOMER,@SM,@FM)
        Y.BEN.CUSTOMER = Y.BEN.CUSTOMER<2>

        IF Y.CREDIT.THEIR.REF THEN
            CALL CACHE.READ(FN.BENEFICIARY, Y.CREDIT.THEIR.REF, R.BENEFICIARY, ERR.BEN) ;*R22 Auto conversion

            IF NOT(ERR.BEN) THEN
                Y.CED.BEN = R.BENEFICIARY<ARC.BEN.LOCAL.REF,POS.BEN.CEDULA>
            END
        END

        IF Y.CED.BEN THEN
            Y.CED.FINAL = Y.CED.BEN
        END ELSE
            IF Y.ACH.PART.ID THEN
                Y.CED.FINAL = Y.ACH.PART.ID
            END ELSE
                IF Y.BEN.CUSTOMER THEN
                    Y.CED.FINAL = Y.BEN.CUSTOMER
                END
            END
        END

    END

RETURN
END
