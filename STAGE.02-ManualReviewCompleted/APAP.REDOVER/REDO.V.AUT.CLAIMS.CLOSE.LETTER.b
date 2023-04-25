* @ValidationCode : MjoxNDAxNTY0Mjk5OkNwMTI1MjoxNjgxMjg0NzY5MjgxOjkxNjM4Oi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 12 Apr 2023 13:02:49
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : 91638
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER

SUBROUTINE REDO.V.AUT.CLAIMS.CLOSE.LETTER
*-----------------------------------------------------------------------------
*----------------------------------------------------------------------------------------------------
* DESCRIPTION : A Authorisation to generate the PDF for CLOSE letter and this routine
* is attached to REDO.ISSUE.CLAIMS,NEW VERSION
*
*-----------------------------------------------------------------------------------------------------
*-----------------------------------------------------------------------------------------------------
* * Input / Output
* --------------
* IN     : -NA-
* OUT    : -NA-
*-----------------------------------------------------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : B RENUGADEVI
* PROGRAM NAME : REDO.V.AUT.CLAIMS.CLOSE.LETTER
*-----------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
* DATE              WHO                REFERENCE         DESCRIPTION
* 16.AUG.2010       BRENUGADEVI        ODR-2009-12-0283  INITIAL CREATION
* 30.MAY.2011       PRADEEP S          PACS00071941      PDF Generated during notification
*Modification history
*Date                Who               Reference                  Description
*12-04-2023      conversion tool     R22 Auto code conversion     No changes
*12-04-2023      Mohanraj R          R22 Manual code conversion   No changes

* ----------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.ISSUE.CLAIMS

    GOSUB INIT
    GOSUB PROCESS
RETURN
*****
INIT:
*****

    FN.REDO.ISSUE.CLAIMS = 'F.REDO.ISSUE.CLAIMS'
    F.REDO.ISSUE.CLAIMS  = ''
    CALL OPF(FN.REDO.ISSUE.CLAIMS,F.REDO.ISSUE.CLAIMS)
RETURN

********
PROCESS:
********
    Y.ID       = ID.NEW
    Y.STATUS   = R.NEW(ISS.CL.CLOSE.NOTIFICATION)
    TASK.NAME  = "ENQ REDO.ENQ.CLAIM.STATUS.CLOSE @ID EQ ":Y.ID

*PACS00071941 - S
*IF Y.STATUS EQ 'YES' THEN
    CALL EB.SET.NEW.TASK(TASK.NAME)
*END
*PACS00071941 - E
RETURN
END
