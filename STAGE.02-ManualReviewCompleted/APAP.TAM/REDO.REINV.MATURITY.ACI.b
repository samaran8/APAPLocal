* @ValidationCode : MjotMTQxODg4MTI5MjpDcDEyNTI6MTY4MTgxMjMyMjA3NjpzYW1hcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 18 Apr 2023 15:35:22
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : samar
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.REINV.MATURITY.ACI(Y.AZ.ID)
*--------------------------------------------------------------------------------
* Company Name : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Program Name : REDO.REINV.MATURITY.ACI
*--------------------------------------------------------------------------------
* Description: This Batch routine is to create a ACI for interest Liq account
* for the deposit with zero rate.
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
*
*  DATE             WHO                         REFERENCE                  DESCRIPTION
* 18-Jul-2011    H GANESH                   PACS00072695_N.11           INITIAL CREATION
*18-04-2023       Conversion Tool        R22 Auto Code conversion          No Changes
*18-04-2023       Samaran T               R22 Manual Code Conversion       No Changes
*----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.ACCOUNT.CREDIT.INT
    $INSERT I_REDO.REINV.MATURITY.ACI.COMMON


    GOSUB PROCESS
RETURN
*---------------------------------------------------------------------------------
PROCESS:
*---------------------------------------------------------------------------------
    R.AZ.ACCOUNT =''
    AZ.ERR = ''
    CALL F.READ(FN.AZ.ACCOUNT,Y.AZ.ID,R.AZ.ACCOUNT,F.AZ.ACCOUNT,AZ.ERR)

    IF R.AZ.ACCOUNT<AZ.MATURITY.INSTR> EQ 'AUTOMATIC ROLLOVER' THEN
        RETURN
    END ELSE
        GOSUB PROCESS.MATURITY
    END

RETURN
*---------------------------------------------------------------------------------
PROCESS.MATURITY:
*---------------------------------------------------------------------------------


    Y.INT.LIQ.ACC = R.AZ.ACCOUNT<AZ.INTEREST.LIQU.ACCT>
    IF Y.INT.LIQ.ACC EQ '' THEN
        RETURN
    END

    Y.ACI.OFS.ARRAY = ''
    Y.ACI.OFS.ARRAY<IC.ACI.INTEREST.DAY.BASIS> = 'A'
    Y.ACI.OFS.ARRAY<IC.ACI.CR.BALANCE.TYPE>    = 'DAILY'
    Y.ACI.OFS.ARRAY<IC.ACI.CR.CALCUL.TYPE>     = 'BAND'
    Y.ACI.OFS.ARRAY<IC.ACI.CR.INT.RATE>        = 0

    ACTUAL.APP.NAME1 = 'ACCOUNT.CREDIT.INT'
    OFS.FUNCTION1 = 'I'
    PROCESS1 = 'PROCESS'
    OFS.VERSION1 = ''
    GTSMODE1 = ''
    NO.OF.AUTH1 = '0'
    TRANSACTION.ID1 =  Y.INT.LIQ.ACC:'-':R.AZ.ACCOUNT<AZ.MATURITY.DATE>
    OFS.RECORD1 = ''
    VERSION1 = 'ACCOUNT.CREDIT.INT,RE'
    MSG.ID1 = ''
    OPTION1 = ''

    CALL OFS.BUILD.RECORD(ACTUAL.APP.NAME1,OFS.FUNCTION1,PROCESS1,VERSION1,GTSMODE1,NO.OF.AUTH1,TRANSACTION.ID1,Y.ACI.OFS.ARRAY,OFS.ACI)
    MSG.ID = ''
    ERR.OFS = ''
    OFS.SRC.ID = 'REINV.DEPOSIT'
    CALL OFS.POST.MESSAGE(OFS.ACI,MSG.ID,OFS.SRC.ID,ERR.OFS)


RETURN

END
