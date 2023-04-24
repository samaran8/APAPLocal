* @ValidationCode : MjoxNzY2NDI1NTE6Q3AxMjUyOjE2ODIwODAwODY3NTg6SGFyaXNodmlrcmFtQzotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 21 Apr 2023 17:58:06
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
SUBROUTINE L.APAP.GET.AUT.MON
*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
*
* DATE              WHO                REFERENCE                 DESCRIPTION
* 21-APR-2023     Conversion tool    R22 Auto conversion       BP Removed in insert file
* 21-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------
    $INSERT I_COMMON ;*R22 Auto conversion - START
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT ;*R22 Auto conversion - END

    Y.ACC.ID = COMI

    FN.ACC = "F.ACCOUNT"
    FV.ACC = ""

    CALL OPF(FN.ACC,FV.ACC)
    CALL F.READ(FN.ACC,Y.ACC.ID,R.ACC,FV.ACC,ACC.ERROR)

    COMI = R.ACC<AC.AUTHORISER>


END
