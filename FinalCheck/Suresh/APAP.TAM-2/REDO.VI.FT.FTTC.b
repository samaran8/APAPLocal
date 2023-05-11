* @ValidationCode : MjotMTUzNjE3MzQ5NzpDcDEyNTI6MTY4MTgxNDIwMTMzODozMzNzdTotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 18 Apr 2023 16:06:41
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
SUBROUTINE REDO.VI.FT.FTTC
****************************************************
*  Company   Name    :Asociacion Popular de Ahorros y Prestamos
*  Developed By      :DHAMU.S
*  Program   Name    :REDO.VI.FT.FTTC

*-----------------------------------------------------------------------------
* Incoming/Outgoing Parameters
*-------------------------------
* In  : --N/A--
* Out : --R.REDO.APAP.H.PARAMETER(FTTC.APAP.CR.CARD),R.REDO.APAP.H.PARAMETER(FTTC.APAP.BANK.CARD)--
*-----------------------------------------------------------------------------
* DESCRIPTION       :This routine is to change the FTTC based on card transacted
*                    is APAP CREDIT Card or Other ban Credit CARD

*------------------------------------------------------------------------------
* Modification History :
*-----------------------
*  DATE            WHO             REFERENCE         DESCRIPTION
*  22-Nov-2010     Dhamu.S                       INITIAL CREATION
*Modification History:
*DATE                 WHO                  REFERENCE                     DESCRIPTION
*18/04/2023      CONVERSION TOOL     AUTO R22 CODE CONVERSION             NOCHANGE
*18/04/2023         SURESH           MANUAL R22 CODE CONVERSION           NOCHANGE
*-------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_AT.ISO.COMMON
    $INSERT I_F.REDO.APAP.H.PARAMETER
    $INSERT I_F.REDO.CARD.BIN
    $INSERT I_F.FUNDS.TRANSFER

    GOSUB INIT
    GOSUB PROCESS
    R.NEW(FT.TRANSACTION.TYPE)=RET.VAL

RETURN

******
INIT:
******

    FN.REDO.APAP.H.PARAMETER = 'F.REDO.APAP.H.PARAMETER'

    FN.REDO.CARD.BIN = 'F.REDO.CARD.BIN'
    F.REDO.CARD.BIN = ''
    CALL OPF(FN.REDO.CARD.BIN,F.REDO.CARD.BIN)

RETURN

*******
PROCESS:
********

    SEL.ID = 'SYSTEM'

    CALL CACHE.READ(FN.REDO.APAP.H.PARAMETER,SEL.ID,R.REDO.APAP.H.PARAMETER,PARAM.ERR)
*   CR.CARD = R.REDO.APAP.H.PARAMETER<PARAM.FTTC.APAP.CR.CARD>
*    BANK.CARD= R.REDO.APAP.H.PARAMETER<PARAM.FTTC.OTHRBANK.CARD>

    BIN.ID = AT$INCOMING.ISO.REQ(2)[1,6]

    CALL F.READ(FN.REDO.CARD.BIN,BIN.ID,R.REDO.CARD.BIN,F.REDO.CARD.BIN,BIN.ERR)

    IF R.REDO.CARD.BIN<REDO.CARD.BIN.BIN.TYPE> EQ 'CREDIT' AND R.REDO.CARD.BIN<REDO.CARD.BIN.BIN.OWNER> EQ 'APAP' THEN
        RET.VAL=R.REDO.APAP.H.PARAMETER<PARAM.FTTC.APAP.CR.CARD >
        RETURN
    END

*    IF R.REDO.CARD.BIN NE '' THEN
*        OUT.PARAM = R.REDO.APAP.H.PARAMETER<PARAM.FTTC.APAP.CR.CARD>
*    END ELSE
*        OUT.PARAM = R.REDO.APAP.H.PARAMETER<PARAM.FTTC.OTHRBANK.CARD>
*    END

    BEGIN CASE

        CASE AT$INCOMING.ISO.REQ(33) EQ 33


            RET.VAL=R.REDO.APAP.H.PARAMETER<PARAM.FTTC.ATHL.CR.CRD>



        CASE AT$INCOMING.ISO.REQ(33) EQ 34


            RET.VAL=R.REDO.APAP.H.PARAMETER<PARAM.FTTC.VISAL.CR.CRD>


    END CASE

RETURN
*************************************************************************

END
*----------------End of Program------------------------------------------------------------
