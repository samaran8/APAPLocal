* @ValidationCode : MjotMTcyOTczMDI2MzpDcDEyNTI6MTY4MTgxMjAzNTY2MzozMzNzdTotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 18 Apr 2023 15:30:35
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : 333su
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE  REDO.VAL.MODE.PARAM
*-------------------------------------------------------------------------
*DESCRIPTION:
*------------
* THIS IS AN VALIDATION ROUTINE TO DEFAULT THE ACCOUNT NUMBER BASED
* ON PAYMENT MODE SELECTED
 
* INPUT/OUTPUT:
*--------------
* IN : N/A
* OUT : N/A

* DEPENDDENCIES:
*-------------------------------------------------------------------------
* CALLED BY :
* CALLS :
* ------------------------------------------------------------------------
*   Date               who           Reference            Description
* 23-MAR-2010     SHANKAR RAJU     ODR-2009-10-0795     Initial Creation
*30-SEP-2011      JEEVA T           PACS00132257          B.34 CHANGES
*Modification History:
*DATE                 WHO                  REFERENCE                     DESCRIPTION
*18/04/2023      CONVERSION TOOL     AUTO R22 CODE CONVERSION             NOCHANGE
*18/04/2023         SURESH           MANUAL R22 CODE CONVERSION           NOCHANGE

*-------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.H.PAY.MODE.PARAM
    $INSERT I_F.ACCOUNT
    $INSERT I_GTS.COMMON

    GOSUB INITIALSE
    IF OFS$OPERATION EQ 'BUILD' THEN
        GOSUB CHECK.PAY.MODE
    END
RETURN
*----------------------------------------------------------------------
*~~~~~~~~~
INITIALSE:
*~~~~~~~~~

    FN.PAY.MODE.PARAM = 'F.REDO.H.PAY.MODE.PARAM'
    F.PAY.MODE.PARAM = ''
    R.PAY.MODE.PARAM = ''
*CALL OPF(FN.PAY.MODE.PARAM,F.PAY.MODE.PARAM) ;*Tus S/E

RETURN
*----------------------------------------------------------------------
*~~~~~~~~~~~~~~~
CHECK.PAY.MODE:
*~~~~~~~~~~~~~~~
    Y.PAY.MODE = ''
    Y.PAY.MODE = COMI
    IF Y.PAY.MODE THEN
        IF Y.PAY.MODE EQ 'Transfer' THEN
            R.NEW(AC.INTEREST.LIQU.ACCT) = ''
        END ELSE
            GOSUB UPD.INT.ACC
        END
    END ELSE
        R.NEW(AC.INTEREST.LIQU.ACCT) = ''
    END
RETURN
*------------------------------------------------------------------------------
UPD.INT.ACC:
*-------------------------------------------------------------------------------
    CALL CACHE.READ(FN.PAY.MODE.PARAM,'SYSTEM',R.PAY.MODE.PARAM,ERR.MPAR)
    Y.PAY.MODE.PAR = R.PAY.MODE.PARAM<REDO.H.PAY.PAYMENT.MODE>
    Y.CURRENCY = R.NEW(AC.CURRENCY)
    LOCATE Y.PAY.MODE IN Y.PAY.MODE.PAR<1,1> SETTING POS.PAY THEN
        VAR.CURRENCY = R.PAY.MODE.PARAM<REDO.H.PAY.CURRENCY,POS.PAY>
        LOCATE Y.CURRENCY IN VAR.CURRENCY<1,1,1> SETTING POS.CCY THEN
            Y.ACCT = R.PAY.MODE.PARAM<REDO.H.PAY.ACCOUNT.NO,POS.PAY,POS.CCY>
            R.NEW(AC.INTEREST.LIQU.ACCT) = Y.ACCT
        END
    END

RETURN
*----------------------------------------------------------------------
END
