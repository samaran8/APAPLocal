* @ValidationCode : MjotMTMxNjU2NjEwNjpDcDEyNTI6MTY4MTI5MDQyMTk3NzpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 12 Apr 2023 14:37:01
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
$PACKAGE APAP.REDOEB
SUBROUTINE MB.E.REMINDER.DUE.ON(REMIND.DUE.ON, SDB.ID)

*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
*
* DATE              WHO                REFERENCE                 DESCRIPTION
* 12-APR-2023     Conversion tool    R22 Auto conversion       No changes
* 12-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.MB.SDB.PARAM
    $INSERT I_F.MB.SDB.STATUS

    FN.MB.SDB.PARAM = 'F.MB.SDB.PARAM'; F.MB.SDB.PARAM = ''
    CALL OPF(FN.MB.SDB.PARAM, F.MB.SDB.PARAM)

    FN.MB.SDB.STATUS = 'F.MB.SDB.STATUS'; F.MB.SDB.STATUS = ''
    CALL OPF(FN.MB.SDB.STATUS, F.MB.SDB.STATUS)

    Y.SDB.STATUS.ID = SDB.ID; R.SDB.STATUS = ''; SDB.ERR = ''
    CALL F.READ(FN.MB.SDB.STATUS, Y.SDB.STATUS.ID, R.SDB.STATUS, F.MB.SDB.STATUS, SDB.ERR)

    IF NOT(SDB.ERR) THEN
        Y.SDB.PARAM.ID = FIELD(SDB.ID, '.', 1,1); R.SDB.PARAM = ''; SDB.ERR = ''
        CALL F.READ(FN.MB.SDB.PARAM, Y.SDB.PARAM.ID, R.SDB.PARAM, F.MB.SDB.PARAM, SDB.ERR)
        IF NOT(SDB.ERR) THEN

            REMIND.FREQ = R.SDB.PARAM<SDB.PAR.2ND.REMINDER.FREQ>
            IF REMIND.FREQ AND R.SDB.STATUS<SDB.STA.RENEWAL.DUE.ON> THEN
                REMIND.DUE.ON = R.SDB.STATUS<SDB.STA.RENEWAL.DUE.ON>
                W.FREQ = "+":REMIND.FREQ
                CALL CDT('', REMIND.DUE.ON, W.FREQ)
            END ELSE
                REMIND.DUE.ON = ''
            END

        END

    END

RETURN

END
