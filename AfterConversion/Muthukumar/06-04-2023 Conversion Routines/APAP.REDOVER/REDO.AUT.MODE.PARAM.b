* @ValidationCode : MjotMTA0Nzk2MTY3NDpDcDEyNTI6MTY4MDcxODE5NTgzNTptdXRodTotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 05 Apr 2023 23:39:55
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : muthu
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.AUT.MODE.PARAM
*-------------------------------------------------------------------------
*DESCRIPTION:
*------------
* THIS IS AN AUTHORISATION ROUTINE TO DEFAULT THE ACCOUNT NUMBER BASED
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
* Date 		      who 		       Reference 	              Description
* 23-MAR-2010  SHANKAR RAJU      ODR-2009-10-0795           Initial Creation
* 05-04-2023   CONVERSION TOOL   AUTO R22 CODE CONVERSION   NO CHANGE
* 05-04-2023   MUTHUKUMAR M      MANUAL R22 CODE CONVERSION NO CHANGE

*-------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.H.PAY.MODE.PARAM
    $INSERT I_F.AZ.ACCOUNT

    GOSUB INITIALSE
    GOSUB CHECK.PAY.MODE

RETURN
*----------------------------------------------------------------------
*~~~~~~~~~
INITIALSE:
*~~~~~~~~~

    FN.PAY.MODE.PARAM = 'F.REDO.H.PAY.MODE.PARAM'
    F.PAY.MODE.PARAM = ''
    R.PAY.MODE.PARAM = ''
    CALL OPF(FN.PAY.MODE.PARAM,F.PAY.MODE.PARAM)

    FN.AZ.ACCOUNT = 'F.AZ.ACCOUNT'
    F.AZ.ACCOUNT = ''
    CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)

    CALL GET.LOC.REF('AZ.ACCOUNT','L.AZ.PAYMT.MODE',PMOD.POS)


RETURN
*----------------------------------------------------------------------
*~~~~~~~~~~~~~~~
CHECK.PAY.MODE:
*~~~~~~~~~~~~~~~

    Y.PAY.MODE = R.NEW(AZ.LOCAL.REF)<1,PMOD.POS>
    Y.INT.LIQ.ACC = R.NEW(AZ.INTEREST.LIQU.ACCT)

    IF Y.PAY.MODE AND Y.INT.LIQ.ACC EQ '' THEN

        CALL CACHE.READ(FN.PAY.MODE.PARAM,'SYSTEM',R.PAY.MODE.PARAM,ERR.MPAR)
        Y.PAY.MODE.PAR = R.PAY.MODE.PARAM<REDO.H.PAY.PAYMENT.MODE>

        IF Y.PAY.MODE EQ 'Admin check' THEN

            LOCATE 'Admin check' IN Y.PAY.MODE.PAR<1,1> SETTING POS.PAY.ADM THEN
                R.NEW(AZ.INTEREST.LIQU.ACCT) = R.PAY.MODE.PARAM<REDO.H.PAY.ACCOUNT.NO,POS.PAY.ADM>
            END

        END ELSE

            LOCATE 'Transfer via ACH' IN Y.PAY.MODE.PAR<1,1> SETTING POS.PAY.TRANS THEN
                R.NEW(AZ.INTEREST.LIQU.ACCT) = R.PAY.MODE.PARAM<REDO.H.PAY.ACCOUNT.NO,POS.PAY.TRANS>
            END

        END

    END
RETURN
*----------------------------------------------------------------------
END
