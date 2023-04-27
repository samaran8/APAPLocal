* @ValidationCode : MjotMjI1Mjk4NzI5OkNwMTI1MjoxNjgyNTExNTkwMTg1OklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 26 Apr 2023 17:49:50
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.BCR.REPORT.BUILD
*-----------------------------------------------------------------------------
*** Simple SUBROUTINE template
* @author youremail@temenos.com
* @stereotype subroutine
* @package infra.eb
*!
*-------------------------------------------------------------------------------------
*Modification
* Date                   who                   Reference
* 10-04-2023         CONVERSTION TOOL     R22 AUTO CONVERSTION FM TO @FM AND ++ TO += 1
* 10-04-2023          ANIL KUMAR B        R22 MANUAL CONVERSTION CALL RTN METHOD ADDED
*--------------------------------------------------------------------------------------
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.INTERFACE.PARAM
    $INSERT I_REDO.B.BCR.REPORT.BUILD.COMMON
    $USING APAP.REDOCHNLS
*-----------------------------------------------------------------------------

    GOSUB INITIALISE
    IF Y.RID.LIST EQ "" THEN    ;* Nothing to do
        RETURN
    END

    GOSUB PROCESS
RETURN
*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------

    K.INT.CODE=''
    K.INT.TYPE='BATCH'
    K.BAT.NO=1
    K.BAT.TOT=DCOUNT(Y.RID.LIST,@FM)
    K.INFO.OR=''
    K.INFO.DE=''
    K.ID.PROC=K.BAT.NO
    K.MON.TP=''
    K.DESC=''
    K.REC.CON=''
    K.EX.USER='OPERATOR'
    K.EX.PC='TNO'

    LOOP
        REMOVE Y.REDO.INT.PARAM.ID FROM Y.RID.LIST SETTING Y.POS
    WHILE Y.POS : Y.REDO.INT.PARAM.ID
        CALL F.READ(FN.REDO.INT.PARAM        , Y.REDO.INT.PARAM.ID, R.REDO.INT.PARAM, F.REDO.INT.PARAM, Y.ERR)

        IF Y.ERR NE '' THEN
            TEXT = "ERROR AL PROCESAR BURO CREDITO " : Y.ERR
            CALL FATAL.ERROR('REDO.B.BCR.REPORT.BUILD' : Y.REDO.INT.PARAM.ID)
        END
        E=''
        CALL APAP.LAPAP.REDO.R.BCR.REPORT.BUILD(Y.REDO.INT.PARAM.ID,'BATCH',R.REDO.INT.PARAM) ;* MANUAL R22 CODE CONVERSION
        IF E NE '' THEN
            K.INT.CODE=Y.REDO.INT.PARAM.ID
*CALL APAP.CHNLS.REDO.INTERFACE.REC.ACT(K.INT.CODE,K.INT.TYPE,K.BAT.NO,K.BAT.TOT,K.INFO.OR,K.INFO.DE,K.ID.PROC,K.MON.TP,K.DESC,K.REC.CON,K.EX.USER,K.EX.PC)
            CALL APAP.REDOCHNLS.redoInterfaceRecAct(K.INT.CODE,K.INT.TYPE,K.BAT.NO,K.BAT.TOT,K.INFO.OR,K.INFO.DE,K.ID.PROC,K.MON.TP,K.DESC,K.REC.CON,K.EX.USER,K.EX.PC) ;*MANUAL R22 CODE CONVERSION
        END
    
        K.BAT.NO += 1
    REPEAT

RETURN

*-----------------------------------------------------------------------------
INITIALISE:
*-----------------------------------------------------------------------------

    Y.RID.LIST = ''   ;* List of the records into REDO.INTERFACE.PARAM to process
* Check if there are some to process
    CALL APAP.LAPAP.REDO.R.BCR.REPORT.GEN.LIST.GET(Y.RID.LIST) ;* MANUAL R22 CODE CONVERSION
    IF Y.RID.LIST EQ "" THEN
        RETURN          ;* Process must not be continued
    END

    FN.REDO.INT.PARAM = 'F.REDO.INTERFACE.PARAM'
    F.REDO.INT.PARAM = ''
    CALL OPF(FN.REDO.INT.PARAM, F.REDO.INT.PARAM)


RETURN

END
