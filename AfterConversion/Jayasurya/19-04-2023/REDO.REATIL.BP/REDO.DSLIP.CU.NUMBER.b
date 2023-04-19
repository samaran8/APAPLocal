* @ValidationCode : MjotMTg2MDQwNTAwNjpDcDEyNTI6MTY4MTg4MTYwNTcxNjpJVFNTQk5HOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 19 Apr 2023 10:50:05
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.REDORETAIL
SUBROUTINE REDO.DSLIP.CU.NUMBER(DS.CRR2)
*------------------------------------------------------------------------------------------------------
* DESCRIPTION:
* Deal slip routine for displaying Customer Number in TT.FXSN.SLIP Deal Slip
*------------------------------------------------------------------------------------------------------
* Input / Output
* --------------
* IN     : DS.CRR2
* OUT    : DS.CRR2
*------------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : Temenos Application Management
* PROGRAM NAME : REDO.DSF.FXSN
* LINKED WITH  : TT.FXSN.SLIP Deal slip
* ----------------------------------------------------------------------------
* Modification History :
*-----------------------
*   DATE             WHO                  REFERENCE          DESCRIPTION
* 27.05.2010      A C Rajkumar        ODR-2010-01-0213     INITIAL CREATION
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*13-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*13-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES

*----------------------------------------------------------------------------
*
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.FOREX.SEQ.NUM
*
    FN.REDO.FOREX.SEQ.NUM = "F.REDO.FOREX.SEQ.NUM"
    F.REDO.FOREX.SEQ.NUM  = ""
    CALL OPF(FN.REDO.FOREX.SEQ.NUM, F.REDO.FOREX.SEQ.NUM)
*
    SEL.CMD.SESSION = "SELECT ":FN.REDO.FOREX.SEQ.NUM:" WITH FX.TXN.ID EQ ":DS.CRR2
    CALL EB.READLIST(SEL.CMD.SESSION,Y.FXSN.ID,'',NO.OF.SESSION.REC,SEL.ERR.SESSION)
*
    CALL F.READ(FN.REDO.FOREX.SEQ.NUM,Y.FXSN.ID, R.REC.REDO.FOREX.SEQ.NUM, F.REDO.FOREX.SEQ.NUM, Y.ERR.REDO.FOREX.SEQ.NUM)
*
    Y.CUSTOMER.NUMBER = R.REC.REDO.FOREX.SEQ.NUM<REDO.FXSN.CUSTOMER.NO>
*
    DS.CRR2 = Y.CUSTOMER.NUMBER
*
RETURN
END
