* @ValidationCode : MjoxMzEzOTYyMjE0OkNwMTI1MjoxNjgzMTA5OTUyNDY5OklUU1M6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 03 May 2023 16:02:32
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
SUBROUTINE REDO.CHK.CUSTOMER.ACCOUNT
**************************************************************************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: Ganesh R
* PROGRAM NAME: REDO.CHK.CUSTOMER.ACCOUNT
* ODR NO      : ODR-2011-03-0150
*-----------------------------------------------------------------------------------------------------
*DESCRIPTION: This routine is to get the card number and check whether it is Valid or not.
*******************************************************************************************************
*linked with :
*In parameter:
*Out parameter:
*****************************************************************************************************
*Modification History
*  Date       Who             Reference       Description
* 20 jan 2012 Ganesh R    ODR-2011-03-0150    Will Raise the Error message
*DATE                 WHO                  REFERENCE                     DESCRIPTION
*05/04/2023      CONVERSION TOOL     AUTO R22 CODE CONVERSION             NOCHANGE
*05/04/2023         SURESH           MANUAL R22 CODE CONVERSION           CALL Rtn format modified
*------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $USING APAP.REDOVER
    GOSUB PROCESS
* CALL APAP.REDOVER.REDO.V.INP.TT.AC.VAL ;*MANUAL R22 CODE CONVERSION
    CALL APAP.REDOVER.redoVInpTtAcVal()
RETURN

PROCESS:
    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT  = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    Y.APP = "ACCOUNT"
    Y.APPL.FLS = "L.AC.STATUS2"
    Y.FLD.POS = ""

    CALL MULTI.GET.LOC.REF(Y.APP,Y.APPL.FLS,Y.FLD.POS)      ;* PACS00631629
    AC2.POS = Y.FLD.POS<1,1>

    ACCT.ID = COMI
    CALL F.READ(FN.ACCOUNT,ACCT.ID,R.ACCOUNT,F.ACCOUNT,ACCT.ERR)
    CUST.ID = R.ACCOUNT<AC.CUSTOMER>
    Y.AC.STATUS = R.ACCOUNT<AC.LOCAL.REF,AC2.POS>


    IF Y.AC.STATUS EQ 'DECEASED' THEN   ;* PACS00631629 -s
        ETEXT = "EB-REDO.AC.DECEASED"
        CALL STORE.END.ERROR
        RETURN
    END   ;* PACS00631629 -e

    IF NOT(CUST.ID) THEN
        ETEXT = "TT-CUST.ACCT"
        CALL STORE.END.ERROR
    END
RETURN
