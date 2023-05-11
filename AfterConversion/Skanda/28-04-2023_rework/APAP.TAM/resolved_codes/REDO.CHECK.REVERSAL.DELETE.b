* @ValidationCode : Mjo3OTEyMDQ3NTpDcDEyNTI6MTY4MjY1Nzk5MzI0OTpJVFNTOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 28 Apr 2023 10:29:53
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
$PACKAGE APAP.TAM
*-----------------------------------------------------------------------------
* <Rating>-21</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE REDO.CHECK.REVERSAL.DELETE
*-----------------------------------------------------------------
*Description: This routine is to restrict the user from delete the RNAU TFS record when
*             underlying TT record is in RNAU. As per TFS module, in order to do reversal deletion,
*             we need to first delete the underlying RNAU TT record by setting the REVERSAL.MARK as R,
*             then it will move the TT RNAU record to LIVE. then we need to delete the TFS record.

** 05-04-2023 R22 Auto Conversion no changes
** 05-04-2023 Skanda R22 Manual Conversion removed T24.BP and USPLATFORM.BP

*$INCLUDE T24.BP I_COMMON
*$INCLUDE T24.BP I_EQUATE
*$INCLUDE T24.BP I_F.TELLER
*$INCLUDE USPLATFORM.BP I_F.T24.FUND.SERVICES
    $INCLUDE I_COMMON ;* R22 Manual Conversion removed T24.BP
    $INCLUDE I_EQUATE ;* R22 Manual Conversion removed T24.BP
    $INCLUDE I_F.TELLER ;* R22 Manual Conversion removed T24.BP
    $INCLUDE I_F.T24.FUND.SERVICES ;* R22 Manual Conversion removed USPLATFORM.BP

    IF V$FUNCTION EQ "D" THEN
        GOSUB OPEN.FILES
        GOSUB PROCESS
    END

RETURN
*-----------------------------------------------------------------
OPEN.FILES:
*-----------------------------------------------------------------

    FN.TELLER$NAU = "F.TELLER$NAU"
    F.TELLER$NAU  = ""
    CALL OPF(FN.TELLER$NAU,F.TELLER$NAU)


RETURN
*-----------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------

    Y.UNDERLYING.TXNS = R.NEW(TFS.UNDERLYING)
    Y.UNDERLYING.CNT  = DCOUNT(Y.UNDERLYING.TXNS,@VM)
    Y.VAR1 = 1
    LOOP
    WHILE Y.VAR1 LE Y.UNDERLYING.CNT
        Y.TT.ID = Y.UNDERLYING.TXNS<1,Y.VAR1>
        CALL F.READ(FN.TELLER$NAU,Y.TT.ID,R.TELLER.NAU,F.TELLER$NAU,TT.ERR)
        IF R.TELLER.NAU<TT.TE.RECORD.STATUS>[1,2] EQ "RN" THEN
            AF = TFS.UNDERLYING
            AV = Y.VAR1
            ETEXT = "EB-REDO.TFS.TT.RNAU"
            CALL STORE.END.ERROR
            Y.VAR1 = Y.UNDERLYING.CNT+1 ;* Instead of break statement.
        END
        Y.VAR1++
    REPEAT

RETURN
END
