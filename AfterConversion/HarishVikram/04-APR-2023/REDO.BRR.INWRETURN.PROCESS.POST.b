* @ValidationCode : MjoxNjU1ODE0MDc0OkNwMTI1MjoxNjgwNjA2MzQ3MTgzOkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 04 Apr 2023 16:35:47
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.BRR.INWRETURN.PROCESS.POST
*------------------------------------------------------------------------.
*Description: This routine is to form the header of the return files.

*------------------------------------------------------------------------

*MODIFICATION HISTORY:
*
* DATE              WHO                REFERENCE                 DESCRIPTION
* 04-APR-2023     Conversion tool   R22 Auto conversion           ++ to +=
* 04-APR-2023      Harishvikram C   Manual R22 conversion      No changes

*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.CLEARING.PROCESS
    $INSERT I_F.REDO.APAP.CLEARING.INWARD
    $INSERT I_REDO.BRR.INWRETURN.PROCESS.COMMON
    GOSUB OPEN.FILES
    GOSUB PROCESS

RETURN
*------------------------------------------------------------------------
OPEN.FILES:
*------------------------------------------------------------------------
    FN.REDO.CLEARING.PROCESS = 'F.REDO.CLEARING.PROCESS'
    F.REDO.CLEARING.PROCESS  = ''
    CALL OPF(FN.REDO.CLEARING.PROCESS,F.REDO.CLEARING.PROCESS)

    FN.REDO.APAP.CLEARING.INWARD = 'F.REDO.APAP.CLEARING.INWARD'
    F.REDO.APAP.CLEARING.INWARD  = ''
    CALL OPF(FN.REDO.APAP.CLEARING.INWARD,F.REDO.APAP.CLEARING.INWARD)

RETURN
*------------------------------------------------------------------------
PROCESS:
*------------------------------------------------------------------------

    CALL F.READ(FN.REDO.CLEARING.PROCESS,'B143.PROCESS',R.REDO.CLEARING.PROCESS,F.REDO.CLEARING.PROCESS,PARAM.ERR)
    IF PARAM.ERR EQ '' THEN
        CCY.FILE.NAME = R.REDO.CLEARING.PROCESS<PRE.PROCESS.IN.RETURN.NAME>
        CCY.OUT.PATH = R.REDO.CLEARING.PROCESS<PRE.PROCESS.IN.RETURN.PATH>
    END

    SEL.CMD = 'SELECT ':CCY.OUT.PATH:' LIKE TEMP...'
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',Y.SEL.CNT,Y.ERR)
    Y.VAR1 = 1
    LOOP
    WHILE Y.VAR1 LE Y.SEL.CNT
        Y.FILE.NAME = SEL.LIST<Y.VAR1>

        GOSUB PROCESS.FILE

        Y.VAR1 += 1
    REPEAT

RETURN
*---------------------------------------------------
PROCESS.FILE:
*---------------------------------------------------

    Y.TOTAL.AMOUNT = 0
    Y.TOTAL.CHECK  = 0
    Y.FIRST.LINE   = ''
    Y.ERROR        = ''

    OPENSEQ CCY.OUT.PATH,Y.FILE.NAME TO F.FILE.PATH ELSE
        CALL OCOMO(Y.FILE.NAME:"FILE NOT AVAILABLE IN PATH ":CCY.OUT.PATH)
        RETURN
    END

    FILE.BRK=1
    LOOP
    WHILE FILE.BRK

        READSEQ READ.LINE FROM F.FILE.PATH ELSE
*GOSUB PROCESS.CALC
            FILE.BRK = 0
        END
        IF FILE.BRK THEN
            GOSUB PROCESS.CALC
        END

    REPEAT
    IF Y.TOTAL.AMOUNT AND Y.TOTAL.CHECK THEN
        GOSUB FORM.HEADER
    END
    IF Y.ERROR ELSE
        DAEMON.CMD = "DELETE ":CCY.OUT.PATH:" ":Y.FILE.NAME
        EXECUTE DAEMON.CMD
    END

RETURN
*---------------------------------------------------
PROCESS.CALC:
*---------------------------------------------------
    IF Y.FIRST.LINE ELSE
        Y.FIRST.LINE = READ.LINE
    END

    Y.TOTAL.AMOUNT = Y.TOTAL.AMOUNT + READ.LINE[61,15]
    Y.TOTAL.CHECK +=1
RETURN
*---------------------------------------------------
FORM.HEADER:
*---------------------------------------------------
    REDO.APAP.CLEARING.INWARD.ID = 'HEADER.00000000000.':Y.FIRST.LINE[7,4]:Y.FIRST.LINE[5,2]:Y.FIRST.LINE[3,2]
    CALL F.READ(FN.REDO.APAP.CLEARING.INWARD,REDO.APAP.CLEARING.INWARD.ID,R.REDO.CLEARING.INWARD,F.REDO.APAP.CLEARING.INWARD,REDO.CLEAR.ERR)

    Y.TASK  = Y.FIRST.LINE[1,2]
    Y.DATE  = Y.FIRST.LINE[3,8]
    Y.LOTE  = FMT(R.REDO.CLEARING.INWARD<CLEAR.CHQ.LOTE>,'10"0"R')
    Y.DIN   = FMT(R.REDO.CLEARING.INWARD<CLEAR.CHQ.DIN>,'10"0"R')
    Y.ACC   = '00000000000'
    Y.SERIAL= '0000000000'
    Y.ROUTE = FMT(R.REDO.CLEARING.INWARD<CLEAR.CHQ.BANK.CODE>,'9"0"R')
    Y.AMT   = FMT(Y.TOTAL.AMOUNT,'R%15')
    Y.CATEG = '0303'
    Y.DR.CNT= FMT(Y.TOTAL.CHECK,'4"0"R')
    Y.CR.CNT= '0000'
    Y.FILLER= '00000000000'
    Y.DV    = '00'
    Y.IMAGE = FMT(R.REDO.CLEARING.INWARD<CLEAR.CHQ.IMAGE.REFERENCE>,'10"0"R'):'.TIF'

    Y.HEADER = Y.TASK:Y.DATE:Y.LOTE:Y.DIN:Y.ACC:Y.SERIAL:Y.ROUTE:Y.AMT:Y.CATEG:Y.DR.CNT:Y.CR.CNT:Y.FILLER:Y.DV:Y.IMAGE

    GOSUB UPDATE.FILE

RETURN
*----------------------------------------------
UPDATE.FILE:
*----------------------------------------------
    OPEN.ERR =  ''
    OPEN CCY.OUT.PATH TO F.FILE.PATH ELSE

        CALL OCOMO(Y.FILE.NAME:"FILE NOT AVAILABLE IN PATH ":F.FILE.PATH)
        RETURN
    END
    READ Y.REC.ARRAY FROM F.FILE.PATH,Y.FILE.NAME ELSE

        CALL OCOMO("File could not be read")
        Y.ERROR = 1
    END

    Y.NEW.FILE.NAME = FIELD(Y.FILE.NAME,'TEMP.',2)
    OPENSEQ CCY.OUT.PATH,Y.NEW.FILE.NAME TO FILE.PTR ELSE
        CREATE FILE.PTR THEN
            Y.FINAL.ARRAY = Y.HEADER
        END ELSE
            OPEN.ERR = 'Unable to Open / Create ':CCY.TEMPORARY.FILE
            Y.ERROR = 1
        END
    END
    IF OPEN.ERR EQ '' THEN
        Y.FINAL.ARRAY<-1> = Y.REC.ARRAY
        WRITESEQ Y.FINAL.ARRAY APPEND TO FILE.PTR ELSE
            WRITE.ERR = 'Unable to Write ':CCY.OUT.PATH:" ":CCY.TEMPORARY.FILE
            Y.ERROR = 1
        END

    END
RETURN
END
