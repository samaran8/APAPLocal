* @ValidationCode : Mjo0NzcxNTUzMDM6Q3AxMjUyOjE2ODQ4NTQzODEzNTE6SVRTUzotMTotMToxODI6MTpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 23 May 2023 20:36:21
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 182
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.BCR.REPORT.PARAM.UPD
*-----------------------------------------------------------------------------
* INTERFACE : REDO.BCR.REPORT : Buro de Credito
* This is a last job routine, its objetive is to update the next date where
* the process (generate and delivery) must be don,
* The update is done in REDO.INTERFACE.PARAM
* @author hpasquel@temenos.com
* @stereotype subroutine
* @package REDO.BCR
*!
*-------------------------------------------------------------------------------------
*Modification
* Date                   who                   Reference              
* 10-04-2023         CONVERSTION TOOL     R22 AUTO CONVERSTION - NO CHANGES
* 10-04-2023          ANIL KUMAR B        R22 MANUAL CONVERSTION -NO CHANGES
*--------------------------------------------------------------------------------------
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.INTERFACE.PARAM
*-----------------------------------------------------------------------------
    GOSUB INITIALISE
    GOSUB PROCESS
RETURN

*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------

    CALL REDO.R.BCR.REPORT.GEN.LIST.GET(Y.LIST)     ;* Get the list of REDO.INTERFACE.PARAM to process

    LOOP
        REMOVE Y.RIP.ID FROM Y.LIST SETTING Y.POS
    WHILE Y.RIP.ID : Y.POS
        Y.CONTINUE = 0
        RETRY = 'E'
        Y.ERR = ''
        R.RIP = ''
        CALL F.READU(FN.REDO.INTERFACE.PARAM, Y.RIP.ID, R.RIP, F.REDO.INTERFACE.PARAM, Y.ERR, RETRY)
        IF Y.ERR NE '' THEN
            TEXT = 'ERROR AL TRATAR DE BLOQUEAR REGISTRO, ' : Y.ERR
            CALL FATAL.ERROR('REDO.B.BCR.REPORT.PARAM.UPD')
            RETURN
        END
        COMI = R.RIP<REDO.INT.PARAM.AUTOM.EXEC.FREC>
        CALL CFQ
        R.RIP<REDO.INT.PARAM.AUTOM.EXEC.FREC> = COMI

*       CALL F.WRITE(FN.REDO.INTERFACE.PARAM, Y.RIP.ID, R.RIP)
*       CALL JOURNAL.UPDATE('REDO.B.BCR.REPORT.PARAM.UPD -' : Y.RIP.ID)
*   Call OFS Message to execute update

        Y.OFS.MESSAGE = "REDO.INTERFACE.PARAM,BCR,,":Y.RIP.ID:",AUTOM.EXEC.FREC=":R.RIP<REDO.INT.PARAM.AUTOM.EXEC.FREC>
        Y.OFS.ID  = ""
        Y.OPTIONS = ""
        CALL OFS.POST.MESSAGE(Y.OFS.MESSAGE, Y.OFS.ID, "BCR.PARAM", Y.OPTIONS)

    REPEAT

RETURN
*-----------------------------------------------------------------------------
INITIALISE:
*-----------------------------------------------------------------------------

    FN.REDO.INTERFACE.PARAM = 'F.REDO.INTERFACE.PARAM'
    F.REDO.INTERFACE.PARAM = ''
    CALL OPF(FN.REDO.INTERFACE.PARAM, F.REDO.INTERFACE.PARAM)

RETURN

END
