* @ValidationCode : MjoxNTQ4MTMzODE3OkNwMTI1MjoxNjgxMjc2NDQ4MTM0OklUU1M6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 12 Apr 2023 10:44:08
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
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.LY.GET.TXN.M.PRE
*-------------------------------------------------------------------------------------------
*DESCRIPTION:
* This routine is attached as batch job in the batch record BNK/REDO.B.LY.GET.TXN.M
* This routine updates REDO.LY.TXN.BY.MOD file with information such as TRANSACTION ID, ACCOUNT NO
* CURRENCY & AMOUNT of transaction in local currency
* ------------------------------------------------------------------------------------------
* Input/Output:
*--------------
* IN  :
* ACCT.ENT.TODAY.ID - @ID of the file ACCT.ENT.TODAY
* OUT : -NA-
*
* Dependencies:
*---------------
* CALLS     : -NA-
* CALLED BY : -NA-
*
* Revision History:
*------------------
*   Date               who           Reference            Description
* 26-DEC-2013      RMONDRAGON     ODR-2011-06-0243       First Version
* Date                  who                   Reference              
* 12-04-2023        �CONVERSTION TOOL   �  R22 AUTO CONVERSTION - No Change
* 12-04-2023          ANIL KUMAR B         R22 MANUAL CONVERSTION -NO CHANGES
*---------------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE

    $INSERT I_F.REDO.LY.MODALITY
    $INSERT I_F.REDO.LY.PROGRAM
    $INSERT I_F.REDO.LY.PDT.TYPE
    $INSERT I_REDO.B.LY.GET.TXN.M.COMMON

    GOSUB INIT
    GOSUB OPEN.FILES
    GOSUB GET.MODALITY.DET

RETURN

*----
INIT:
*----
*-------------------------------------------------------------------------
* This section initialises necessary local common variables
*-------------------------------------------------------------------------

    MODALITY.CCY.LIST = ''
    MODALITY.ID.LIST = ''
    MODALITY.APP.TXN.LIST = ''
    MODALITY.TXN.CODE.LIST = ''
    MODALITY.GEN.FACT.LIST = ''
    MODALITY.FORM.GEN.LIST = ''
    MODALITY.PROD.TYPE.LIST = ''
    MODALITY.PROD.LIST = ''
    MODALITY.CHANNEL.LIST = ''
    MODALITY.MINDISAMT.LIST = ''
    MODALITY.GEN.AMT.LIST = ''
    PROGRAM.EXCCONDTYPE.ESP.LIST = ''
    PROGRAM.APPEXC.LIST = ''
    PROGRAM.EXCCONDTYPE.LIST = ''
    PROGRAM.APPINC.LIST = ''
    PROGRAM.INCCONDTYPE.ESP.LIST = ''
    PROGRAM.INCCONDTYPE.LIST = ''

RETURN

*----------
OPEN.FILES:
*----------
*-------------------------------------------------------------------------
* This section opens the necessary files and directories
*-------------------------------------------------------------------------

    FN.REDO.LY.MODALITY = 'F.REDO.LY.MODALITY'
    F.REDO.LY.MODALITY = ''
    CALL OPF(FN.REDO.LY.MODALITY,F.REDO.LY.MODALITY)

    FN.REDO.LY.PROGRAM = 'F.REDO.LY.PROGRAM'
    F.REDO.LY.PROGRAM = ''
    CALL OPF(FN.REDO.LY.PROGRAM,F.REDO.LY.PROGRAM)

    FN.REDO.LY.PDT.TYPE = 'F.REDO.LY.PDT.TYPE'
    F.REDO.LY.PDT.TYPE = ''
    CALL OPF(FN.REDO.LY.PDT.TYPE,F.REDO.LY.PDT.TYPE)

    FN.TEMP.LY.GET.TXN.M = 'F.TEMP.LY.GET.TXN.M'
    F.TEMP.LY.GET.TXN.M = ''
    OPEN FN.TEMP.LY.GET.TXN.M TO F.TEMP.LY.GET.TXN.M ELSE

        TEXT = 'Error in opening : ':FN.TEMP.LY.GET.TXN.M
        CALL FATAL.ERROR('REDO.B.LY.GET.TXN.M.PRE')
    END
    CLEARFILE F.TEMP.LY.GET.TXN.M

RETURN

*----------------
GET.MODALITY.DET:
*----------------
*-------------------------------------------------------------------------------------------------
* This section reads all the REDO.LY.MODALITY record which is of type 1 and
*   stores the value in the local common variables to be used in the record routine
*-------------------------------------------------------------------------------------------------

    SEL.MODALITY.CMD = "SSELECT ":FN.REDO.LY.MODALITY:" WITH TYPE EQ 1"
    MOD.LST = ''
    CALL EB.READLIST(SEL.MODALITY.CMD,MOD.LST,'',ID.CNT,'')

    LOOP
        REMOVE MODALITY.ID FROM MOD.LST SETTING MODALITY.ID.POS
    WHILE MODALITY.ID:MODALITY.ID.POS
        GOSUB GET.PROGRAM.DET
        R.REDO.LY.MODALITY = ''
        CALL F.READ(FN.REDO.LY.MODALITY,MODALITY.ID,R.REDO.LY.MODALITY,F.REDO.LY.MODALITY,MOD.ERR)
        MODALITY.ID.LIST<-1> = MODALITY.ID
        MODALITY.CCY.LIST<-1> = R.REDO.LY.MODALITY<REDO.MOD.CURRENCY>
        MODALITY.APP.TXN.LIST<-1> = R.REDO.LY.MODALITY<REDO.MOD.APP.TXN>
        MODALITY.TXN.CODE.LIST<-1> = R.REDO.LY.MODALITY<REDO.MOD.TXN.CODE>
        MODALITY.FORM.GEN.LIST<-1> = R.REDO.LY.MODALITY<REDO.MOD.FORM.GENERATION>
        MODALITY.CHANNEL.LIST<-1> = R.REDO.LY.MODALITY<REDO.MOD.CHANNEL>

        IF R.REDO.LY.MODALITY<REDO.MOD.MIN.DISBURSE.AMT> EQ '' THEN
            MODALITY.MINDISAMT.LIST<-1> = 'NULL'
        END ELSE
            MODALITY.MINDISAMT.LIST<-1> = R.REDO.LY.MODALITY<REDO.MOD.MIN.DISBURSE.AMT>
        END

        MODALITY.GEN.AMT.LIST<-1> = R.REDO.LY.MODALITY<REDO.MOD.GEN.AMT>

        Y.PRD.GRP.ID = ''
        Y.PRD.GRP.ID = R.REDO.LY.MODALITY<REDO.MOD.PRODUCT.GROUP>
        GOSUB GET.PROD.TYPE

        IF R.REDO.LY.MODALITY<REDO.MOD.FORM.GENERATION> EQ 1 THEN
            MODALITY.GEN.FACT.LIST<-1> = 0
        END

        IF R.REDO.LY.MODALITY<REDO.MOD.FORM.GENERATION> EQ 2 AND R.REDO.LY.MODALITY<REDO.MOD.GEN.AMT> EQ 'Interes' THEN
            MODALITY.GEN.FACT.LIST<-1> = R.REDO.LY.MODALITY<REDO.MOD.INT.GEN.FACTOR>
        END ELSE
            MODALITY.GEN.FACT.LIST<-1> = R.REDO.LY.MODALITY<REDO.MOD.GEN.FACTOR>
        END

    REPEAT


*  WRITE PROGRAM.EXCCONDTYPE.ESP.LIST TO F.TEMP.LY.GET.TXN.M,'VARLY1' ;*Tus Start
    CALL F.WRITE(FN.TEMP.LY.GET.TXN.M,'VARLY1',PROGRAM.EXCCONDTYPE.ESP.LIST) ; * Tus End

*  WRITE PROGRAM.APPEXC.LIST TO F.TEMP.LY.GET.TXN.M,'VARLY2' ;*Tus Start
    CALL F.WRITE(FN.TEMP.LY.GET.TXN.M,'VARLY2',PROGRAM.APPEXC.LIST) ; * Tus End

*  WRITE PROGRAM.EXCCONDTYPE.LIST TO F.TEMP.LY.GET.TXN.M,'VARLY3' ;*Tus Start
    CALL F.WRITE(FN.TEMP.LY.GET.TXN.M,'VARLY3',PROGRAM.EXCCONDTYPE.LIST) ; * Tus End

*  WRITE PROGRAM.INCCONDTYPE.ESP.LIST TO F.TEMP.LY.GET.TXN.M,'VARLY4' ;*Tus Start
    CALL F.WRITE(FN.TEMP.LY.GET.TXN.M,'VARLY4',PROGRAM.INCCONDTYPE.ESP.LIST) ; * Tus End

*  WRITE PROGRAM.APPINC.LIST TO F.TEMP.LY.GET.TXN.M,'VARLY5' ;*Tus Start
    CALL F.WRITE(FN.TEMP.LY.GET.TXN.M,'VARLY5',PROGRAM.APPINC.LIST) ; * Tus End

*  WRITE PROGRAM.INCCONDTYPE.LIST TO F.TEMP.LY.GET.TXN.M,'VARLY6' ;*Tus Start
    CALL F.WRITE(FN.TEMP.LY.GET.TXN.M,'VARLY6',PROGRAM.INCCONDTYPE.LIST) ; * Tus End

*  WRITE MODALITY.CCY.LIST TO F.TEMP.LY.GET.TXN.M,'VARLY7' ;*Tus Start
    CALL F.WRITE(FN.TEMP.LY.GET.TXN.M,'VARLY7',MODALITY.CCY.LIST) ; * Tus End

*  WRITE MODALITY.ID.LIST TO F.TEMP.LY.GET.TXN.M,'VARLY8' ;*Tus Start
    CALL F.WRITE(FN.TEMP.LY.GET.TXN.M,'VARLY8',MODALITY.ID.LIST) ; * Tus End

*  WRITE MODALITY.APP.TXN.LIST TO F.TEMP.LY.GET.TXN.M,'VARLY9' ;*Tus Start
    CALL F.WRITE(FN.TEMP.LY.GET.TXN.M,'VARLY9',MODALITY.APP.TXN.LIST) ;* Tus End

*  WRITE MODALITY.TXN.CODE.LIST TO F.TEMP.LY.GET.TXN.M,'VARLY10' ;*Tus Start
    CALL F.WRITE(FN.TEMP.LY.GET.TXN.M,'VARLY10',MODALITY.TXN.CODE.LIST) ;* Tus End

*  WRITE MODALITY.FORM.GEN.LIST TO F.TEMP.LY.GET.TXN.M,'VARLY11' ;*Tus Start
    CALL F.WRITE(FN.TEMP.LY.GET.TXN.M,'VARLY11',MODALITY.FORM.GEN.LIST) ;* Tus End

*  WRITE MODALITY.CHANNEL.LIST TO F.TEMP.LY.GET.TXN.M,'VARLY12' ;*Tus Start
    CALL F.WRITE(FN.TEMP.LY.GET.TXN.M,'VARLY12',MODALITY.CHANNEL.LIST) ; * Tus End

*  WRITE MODALITY.MINDISAMT.LIST TO F.TEMP.LY.GET.TXN.M,'VARLY13' ;*Tus Start
    CALL F.WRITE(FN.TEMP.LY.GET.TXN.M,'VARLY13',MODALITY.MINDISAMT.LIST) ; * Tus End

*  WRITE MODALITY.GEN.AMT.LIST TO F.TEMP.LY.GET.TXN.M,'VARLY14' ;*Tus Start
    CALL F.WRITE(FN.TEMP.LY.GET.TXN.M,'VARLY14',MODALITY.GEN.AMT.LIST) ; * Tus End

*  WRITE MODALITY.PROD.TYPE.LIST TO F.TEMP.LY.GET.TXN.M,'VARLY15' ;*Tus Start
    CALL F.WRITE(FN.TEMP.LY.GET.TXN.M,'VARLY15',MODALITY.PROD.TYPE.LIST) ; * Tus End

*  WRITE MODALITY.PROD.LIST TO F.TEMP.LY.GET.TXN.M,'VARLY16' ;*Tus Start
    CALL F.WRITE(FN.TEMP.LY.GET.TXN.M,'VARLY16',MODALITY.PROD.LIST) ; * Tus End

*  WRITE MODALITY.GEN.FACT.LIST TO F.TEMP.LY.GET.TXN.M,'VARLY17' ;*Tus Start
    CALL F.WRITE(FN.TEMP.LY.GET.TXN.M,'VARLY17',MODALITY.GEN.FACT.LIST) ; * Tus End

RETURN

*----------------
GET.PROGRAM.DET:
*----------------
*-------------------------------------------------------------------------------------------------
* This section reads all the REDO.LY.PROGRAM record which is of type 1 and
*   stores the value in the local common variables to be used in the record routine
*-------------------------------------------------------------------------------------------------

    ASSIGNE = 'N'; ASSIGNI = 'N'
    SEL.PROGRAM.CMD = "SELECT ":FN.REDO.LY.PROGRAM:" WITH MODALITY EQ ":MODALITY.ID

    CALL EB.READLIST(SEL.PROGRAM.CMD,PROGRAM.ID.LIST,'',PROG.ID.CNT,'')

    IF PROGRAM.ID.LIST THEN
        CALL F.READ(FN.REDO.LY.PROGRAM,PROGRAM.ID.LIST,R.REDO.LY.PROGRAM,F.REDO.LY.PROGRAM,PRG.ERR)
        IF R.REDO.LY.PROGRAM THEN
            Y.COND.TYPE = R.REDO.LY.PROGRAM<REDO.PROG.COND.TYPE.EXINC>
            Y.APP.EXC = R.REDO.LY.PROGRAM<REDO.PROG.APP.EXC.COND>
            IF Y.APP.EXC EQ 'ESPECIFICA' THEN
                IF Y.COND.TYPE EQ 'ESTADO.CUENTA' THEN
                    Y.PROGRAM.COND = R.REDO.LY.PROGRAM<REDO.PROG.EXC.EST.ACCT>
                    PROGRAM.EXCCONDTYPE.ESP.LIST<-1> = Y.COND.TYPE
                    PROGRAM.APPEXC.LIST<-1> = Y.APP.EXC
                    PROGRAM.EXCCONDTYPE.LIST<-1> = Y.PROGRAM.COND
                    ASSIGNE = 'Y'
                END
                IF Y.COND.TYPE EQ 'MCC.TDEBITO' THEN
                    Y.PROGRAM.COND = R.REDO.LY.PROGRAM<REDO.PROG.EXC.COND>
                    PROGRAM.EXCCONDTYPE.ESP.LIST<-1> = Y.COND.TYPE
                    PROGRAM.APPEXC.LIST<-1> = Y.APP.EXC
                    PROGRAM.EXCCONDTYPE.LIST<-1> = Y.PROGRAM.COND
                    ASSIGNE = 'Y'
                END
                IF Y.COND.TYPE EQ 'MERCHANTID.TDEBITO' THEN
                    Y.PROGRAM.COND = R.REDO.LY.PROGRAM<REDO.PROG.EXC.COND>
                    PROGRAM.EXCCONDTYPE.ESP.LIST<-1> = Y.COND.TYPE
                    PROGRAM.APPEXC.LIST<-1> = Y.APP.EXC
                    PROGRAM.EXCCONDTYPE.LIST<-1> = Y.PROGRAM.COND
                    ASSIGNE = 'Y'
                END
            END
            Y.APP.INC = R.REDO.LY.PROGRAM<REDO.PROG.APP.INC.COND>
            IF Y.APP.INC EQ 'ESPECIFICA' THEN
                IF Y.COND.TYPE EQ 'ESTADO.CUENTA' THEN
                    Y.PROGRAM.COND = R.REDO.LY.PROGRAM<REDO.PROG.INC.EST.ACCT>
                    PROGRAM.INCCONDTYPE.ESP.LIST<-1> = Y.COND.TYPE
                    PROGRAM.APPINC.LIST<-1> = Y.APP.INC
                    PROGRAM.INCCONDTYPE.LIST<-1> = Y.PROGRAM.COND
                    ASSIGNI = 'Y'
                END
                IF Y.COND.TYPE EQ 'MCC.TDEBITO' THEN
                    Y.PROGRAM.COND = R.REDO.LY.PROGRAM<REDO.PROG.INC.COND>
                    PROGRAM.INCCONDTYPE.ESP.LIST<-1> = Y.COND.TYPE
                    PROGRAM.APPINC.LIST<-1> = Y.APP.EXC
                    PROGRAM.INCCONDTYPE.LIST<-1> = Y.PROGRAM.COND
                    ASSIGNI = 'Y'
                END
                IF Y.COND.TYPE EQ 'MERCHANTID.TDEBITO' THEN
                    Y.PROGRAM.COND = R.REDO.LY.PROGRAM<REDO.PROG.INC.COND>
                    PROGRAM.INCCONDTYPE.ESP.LIST<-1> = Y.COND.TYPE
                    PROGRAM.APPINC.LIST<-1> = Y.APP.EXC
                    PROGRAM.INCCONDTYPE.LIST<-1> = Y.PROGRAM.COND
                    ASSIGNI = 'Y'
                END
            END
        END
    END

    IF ASSIGNE EQ 'N' THEN
        PROGRAM.EXCCONDTYPE.ESP.LIST<-1> = 'NULL'
        PROGRAM.APPEXC.LIST<-1> = 'NULL'
        PROGRAM.EXCCONDTYPE.LIST<-1> = 'NULL'
    END

    IF ASSIGNI EQ 'N' THEN
        PROGRAM.INCCONDTYPE.ESP.LIST<-1> = 'NULL'
        PROGRAM.APPINC.LIST<-1> = 'NULL'
        PROGRAM.INCCONDTYPE.LIST<-1> = 'NULL'
    END

RETURN

*-------------
GET.PROD.TYPE:
*-------------

    R.PDT.TYPE = ''
    CALL F.READ(FN.REDO.LY.PDT.TYPE,Y.PRD.GRP.ID,R.PDT.TYPE,F.REDO.LY.PDT.TYPE,PDT.TYPE.ERR)
    IF R.PDT.TYPE THEN
        MODALITY.PROD.TYPE.LIST<-1> = R.PDT.TYPE<REDO.PDT.PRODUCT.TYPE>
        MODALITY.PROD.LIST<-1> = R.PDT.TYPE<REDO.PDT.PRODUCT>
    END

RETURN

END
