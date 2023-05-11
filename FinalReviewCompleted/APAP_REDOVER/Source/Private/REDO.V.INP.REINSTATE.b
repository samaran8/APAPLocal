* @ValidationCode : MjoyODkyNjk0NjpDcDEyNTI6MTY4MTI4NjA3NDUxNTpzYW1hcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 12 Apr 2023 13:24:34
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
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.V.INP.REINSTATE
*-----------------------------------------------------------------------------
*------------------------------------------------------------------------------------------
* DESCRIPTION : This routine will be executed at the Input level for the Following versions
* of TELLER, REINSTATE.CERTIFIED.CHEQUES. This Routine will be used for reversing the Certified
* Cheques. This Routine is used to reinstate a certified cheque by checking status as "issued"
* It will not allow to process with the Status as "PAID" or "CANCELLED"
*------------------------------------------------------------------------------------------
*------------------------------------------------------------------------------------------
* * Input / Output
* ---------------------------
* IN     : -NA-
* OUT    : -NA-
* Linked With : TELLER
*------------------------------------------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : SUDHARSANAN S
* PROGRAM NAME : REDO.V.INP.REINSTATE
*------------------------------------------------------------------------------------------
* Modification History :
*-------------------------------------
* DATE               WHO                   REFERENCE                   DESCRIPTION
* 16.03.2010      SUDHARSANAN S        ODR-2009-10-0319              INITIAL CREATION
*12-04-2023       Conversion Tool      R22 Auto Code conversion          VM TO @VM
*12-04-2023       Samaran T             R22 Manual Code Conversion       No Changes
* ----------------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_F.TELLER
    $INSERT I_F.CERTIFIED.CHEQUE.DETAILS
    GOSUB INIT
    GOSUB PROCESS
RETURN
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
INIT:
    FN.CERTIFIED.CHEQUE.DETAILS='F.CERTIFIED.CHEQUE.DETAILS'
    F.CERTIFIED.CHEQUE.DETAILS=''
    CALL OPF(FN.CERTIFIED.CHEQUE.DETAILS,F.CERTIFIED.CHEQUE.DETAILS)
    LREF.APP='TELLER'
    LREF.FIELD='CERT.CHEQUE.NO':@VM:'L.COMMENTS'
    LREF.POS=''
    CALL MULTI.GET.LOC.REF(LREF.APP,LREF.FIELD,LREF.POS)
    POS.CERT.CHEQUE = LREF.POS<1,1>
    POS.COMMENTS = LREF.POS<1,2>
RETURN
*---------------------------------------------------------------------------------
PROCESS:
* Allowed the cheque status other than paid and cancelled
    Y.CERT.CHEQ.NO = R.NEW(TT.TE.LOCAL.REF)<1,POS.CERT.CHEQUE>
    R.NEW(TT.TE.LOCAL.REF)<1,POS.COMMENTS> = 'REINSTATE.CHEQUE'
    CALL F.READ(FN.CERTIFIED.CHEQUE.DETAILS,Y.CERT.CHEQ.NO,R.CERT.CHEQ.DET,F.CERTIFIED.CHEQUE.DETAILS,DET.ERR)
    Y.STATUS = R.CERT.CHEQ.DET<CERT.DET.STATUS>
    IF Y.STATUS EQ 'PAID' OR Y.STATUS EQ 'CANCELLED' OR Y.STATUS EQ 'REINSTATED' THEN
        CURR.NO=''
        CURR.NO=DCOUNT(R.NEW(TT.TE.OVERRIDE),@VM) + 1
        TEXT='TT.CERT.CHEQUE.REINSTATE'
        CALL STORE.OVERRIDE(CURR.NO)
    END
RETURN
*-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
END
