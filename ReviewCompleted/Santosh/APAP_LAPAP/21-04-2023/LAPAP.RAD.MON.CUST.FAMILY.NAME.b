* @ValidationCode : MjotMTg0MjM2NTI1MjpDcDEyNTI6MTY4MjA4MDU2Nzk4MzpBZG1pbjotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 21 Apr 2023 18:06:07
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : Admin
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.LAPAP
SUBROUTINE LAPAP.RAD.MON.CUST.FAMILY.NAME
*-----------------------------------------------------------------------------

*MODIFICATION HISTORY:

*

* DATE              WHO                 REFERENCE               DESCRIPTION

* 21-APR-2023    Conversion tool      R22 Auto conversion     BP is removed in Insert File, INCLUDE to INSERT
* 21-APR-2023    Narmadha V           R22 Manual Conversion    No Changes
*-----------------------------------------------------------------------------
    $INSERT I_COMMON ;*R22 Auto conversion - START
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER ;*R22 Auto conversion - END

    FN.CUSTOMER = 'F.CUSTOMER'; F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    Y.CUST.CODE = COMI
    CALL F.READ(FN.CUSTOMER, Y.CUST.CODE, R.CUSTOMER, F.CUSTOMER, ERR.CUS)

    IF NOT(ERR.CUS) THEN

        Y.FAMILY.NAME = ''

        IF R.CUSTOMER<EB.CUS.FAMILY.NAME> THEN
            Y.FAMILY.NAME = R.CUSTOMER<EB.CUS.FAMILY.NAME>
        END ELSE
            Y.FAMILY.NAME = R.CUSTOMER<EB.CUS.NAME.2>
        END

        COMI = Y.FAMILY.NAME
    END

RETURN
END
