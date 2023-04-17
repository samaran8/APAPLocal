* @ValidationCode : MjotMTA0NjkwOTMzMjpDcDEyNTI6MTY4MTcxNDkxMzA4NDpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 17 Apr 2023 12:31:53
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
SUBROUTINE REDO.CONV.CLAIMS.EB.CHANNEL
*-------------------------------------------------------------------------
*DESCRIPTION:
*------------
* This routine is attached as a conversion routine to the enquiry
* display the field description of EB.CHANNEL instead of the ID.
*-------------------------------------------------------------------------
* HISTORY:
*---------
*   Date               who           Reference            Description

* 16-SEP-2011         RIYAS      ODR-2011-07-0162     Initial Creation

* DATE              WHO                REFERENCE                 DESCRIPTION
* 13-APR-2023     Conversion tool    R22 Auto conversion       F.READ to CACHE.READ
* 17-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.EB.CHANNEL

    GOSUB INITIALSE
    GOSUB CHECK.NOTES

RETURN
*-------------------------------------------------------------------------
INITIALSE:
*~~~~~~~~~

    FN.EB.CHANNEL = 'F.EB.CHANNEL'
    F.EB.CHANNEL  = ''
    CALL OPF(FN.EB.CHANNEL,F.EB.CHANNEL)

RETURN
*-------------------------------------------------------------------------
CHECK.NOTES:
*~~~~~~~~~~~
    Y.REC.DATA = O.DATA

    CALL CACHE.READ(FN.EB.CHANNEL, Y.REC.DATA, R.EB.CHANNEL, LOOKUP.ERR) ;*R22 Auto conversion
    IF LNGG EQ 1 THEN
        O.DATA=R.EB.CHANNEL<EB.CHAN.DESC,1>
        RETURN
    END
    IF LNGG EQ 2 THEN
        IF R.EB.CHANNEL<EB.CHAN.DESC,2> THEN
            O.DATA = R.EB.CHANNEL<EB.CHAN.DESC,2>
        END ELSE
            O.DATA = R.EB.CHANNEL<EB.CHAN.DESC,1>
        END
    END

RETURN
*-------------------------------------------------------------------------
END
