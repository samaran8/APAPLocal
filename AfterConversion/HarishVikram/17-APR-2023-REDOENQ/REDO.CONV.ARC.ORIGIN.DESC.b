* @ValidationCode : MjoxNTMwODUwODYyOkNwMTI1MjoxNjgxNzE0ODM4MzYyOkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 17 Apr 2023 12:30:38
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
$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.CONV.ARC.ORIGIN.DESC
*------------------------------------------------------------------------------------------------------------
* IN Parameter    : NA
* OUT Parameter   : NA
*--------------------------------------------------------------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : RIYAS
* PROGRAM NAME : REDO.CONV.ARC.ORIGIN.DESC
*--------------------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
* Date             Author             Reference                   Description
* 17-APR-2023     Conversion tool    R22 Auto conversion       No changes
* 17-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*---------------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.FUNDS.TRANSFER
*

    GOSUB INIT
    GOSUB PROCESS
RETURN
*****
INIT:
*****


    FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER'
    F.FUNDS.TRANSFER  = ''
    CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)

    FN.FUNDS.TRANSFER.HIST = 'F.FUNDS.TRANSFER$HIS'
    F.FUNDS.TRANSFER.HIST  =''
    CALL OPF(FN.FUNDS.TRANSFER.HIST,F.FUNDS.TRANSFER.HIST)


RETURN
********
PROCESS:
********

    IF O.DATA[1,2] EQ 'FT' THEN
        CALL F.READ(FN.FUNDS.TRANSFER,O.DATA,R.FUNDS.TRANSFER,F.FUNDS.TRANSFER,FT.ERR)
        IF R.FUNDS.TRANSFER THEN
            Y.REV.TXN.DET = R.FUNDS.TRANSFER<FT.PAYMENT.DETAILS>
        END ELSE
            CALL EB.READ.HISTORY.REC(F.FUNDS.TRANSFER.HIST,O.DATA,R.FUNDS.TRANSFER.HIST,FT.HIST.ERR)
            Y.REV.TXN.DET = R.FUNDS.TRANSFER.HIST<FT.PAYMENT.DETAILS>
        END
    END

    IF Y.REV.TXN.DET[1,12] EQ 'REVERSADO-FT' THEN
        O.DATA = Y.REV.TXN.DET
    END  ELSE
        IF Y.REV.TXN.DET[1,10] EQ 'REVERSO-FT' THEN
            O.DATA = 'REVERSADO-':FIELD(Y.REV.TXN.DET,'-',2)
        END ELSE
            O.DATA = 'N/A'
        END
    END
RETURN
END
