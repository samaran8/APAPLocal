* @ValidationCode : MjotMjA5MjQzODQxODpDcDEyNTI6MTY4MjQxMjMzMTE2MTpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MTpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 25 Apr 2023 14:15:31
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.INP.DS.REINV.AZ.CLOSE

*Modification History
*DATE                WHO                         REFERENCE                DESCRIPTION
*06-04-2023       Conversion Tool        R22 Auto Code conversion          No Changes
*06-04-2023       Samaran T               R22 Manual Code Conversion       No Changes

*------------------------------------------------------------------------------------------
*------------------------------------------------------------------------------------------
* * Input / Output
* --------------
* IN     : -NA-
* OUT    : -NA-
*------------------------------------------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : SUDHARSANAN S
*------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.ACCOUNT
    $INSERT I_F.AZ.CUSTOMER
    $INSERT I_F.ACCOUNT.CLOSURE

    GOSUB INIT
    GOSUB PROCESS
RETURN
*-----------------------------------------------------------------------------
INIT:
*-------------------------------------------------------------------------------
    FN.AZ.ACCOUNT='F.AZ.ACCOUNT'
    F.AZ.ACCOUNT = ''
    CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    LOC.REF.APP = 'ACCOUNT'
    LOC.REF.FIELD = 'L.AC.AZ.ACC.REF'
    LOC.REF.POS = ''
    CALL MULTI.GET.LOC.REF(LOC.REF.APP,LOC.REF.FIELD,LOC.REF.POS)
    POS.AZ.ACC.REF          = LOC.REF.POS<1,1>
RETURN
*-----------------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------------
* This is the main para used to check the category and update the charge related fields
    VAR.ACC.ID = ID.NEW
    CALL F.READ(FN.ACCOUNT,VAR.ACC.ID,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
    VAR.AZ.ACC.REF = R.ACCOUNT<AC.LOCAL.REF,POS.AZ.ACC.REF>
    IF VAR.AZ.ACC.REF THEN
        IF OFS$OPERATION EQ 'PROCESS' THEN
            GOSUB GENERATE.DEAL.SLIP
        END
    END
RETURN
*----------------------------------------------------------------------------------
GENERATE.DEAL.SLIP:
*----------------------------------------------------------------------------------

    Y.OVER.LIST = OFS$OVERRIDES
    IF Y.OVER.LIST THEN
        IF Y.OVER.LIST<2,1> EQ 'YES' THEN
            LOCATE 'NOANSWER' IN OFS$WARNINGS<2,1> SETTING POS ELSE
                OFS$DEAL.SLIP.PRINTING = 1
                CALL PRODUCE.DEAL.SLIP('REDO.REIN.CLO')
            END
        END
    END ELSE
        LOCATE 'NOANSWER' IN OFS$WARNINGS<2,1> SETTING POS ELSE
            OFS$DEAL.SLIP.PRINTING = 1
            CALL PRODUCE.DEAL.SLIP('REDO.REIN.CLO')
        END

    END
RETURN
*---------------------------------------------------------------------------------------------------------------
END
