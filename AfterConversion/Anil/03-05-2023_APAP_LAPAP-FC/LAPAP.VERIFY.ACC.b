* @ValidationCode : Mjo0MDg3OTg4Mjc6Q3AxMjUyOjE2ODMwOTU0NTk3NjY6SVRTUzotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 03 May 2023 12:00:59
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.LAPAP
SUBROUTINE LAPAP.VERIFY.ACC(ACC,RES)
*--------------------------------------------------------------------------------------------
*Modification History
*DATE                WHO                         REFERENCE                DESCRIPTION
*24-04-2023       Conversion Tool        R22 Auto Code conversion          INSERT FILE MODIFIED
*24-04-2023       Samaran T               R22 Manual Code Conversion       No Changes
*----------------------------------------------------------------------------------------------------

    $INSERT I_COMMON     ;*R22 AUTO CODE CONVERSION.START
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT      ;*R22 AUTO CODE CONVERSION.END

    AC = ACC
    FN.ACC = "F.ACCOUNT$HIS"
    F.ACC = ""
    CALL OPF(FN.ACC,F.ACC)

    CALL F.READ.HISTORY(FN.ACC,AC,R.ACC,F.ACC,ERR);
    CALL GET.LOC.REF("ACCOUNT","L.AC.AZ.ACC.REF",POS);
    AZ.ACC.REF = R.ACC<AC.LOCAL.REF,POS>
    CATEGORY = R.ACC<AC.CATEGORY>

    IF CATEGORY GE 6010 AND CATEGORY LE 6020 THEN
        RES = AZ.ACC.REF
    END ELSE
        RES = AC[1,10]
    END

RETURN

END
