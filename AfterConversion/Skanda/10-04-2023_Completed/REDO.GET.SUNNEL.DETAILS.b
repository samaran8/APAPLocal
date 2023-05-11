$PACKAGE APAP.TAM

** 10-04-2023 R22 Auto Conversion - FM TO @FM, VM to @VM, SM to @SM
** 10-04-2023 Skanda R22 Manual Conversion - No changes

SUBROUTINE REDO.GET.SUNNEL.DETAILS(Y.CARD.NO,Y.CARD.TYPE)

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.SUNNEL.METHOD
    $INSERT I_F.REDO.SUNNEL.PARAMETER
    $INSERT JBC.h

    GOSUB OPENFILE
    GOSUB PROCESS
RETURN

OPENFILE:
*Opening the Files

    FN.SUN.PARAM = 'F.REDO.SUNNEL.PARAMETER'
    F.SUN.PARAM  = ''
    CALL OPF(FN.SUN.PARAM,F.SUN.PARAM)

    FN.SUN.METHOD = 'F.REDO.SUNNEL.METHOD'
    F.SUN.METHOD = ''
    CALL OPF(FN.SUN.METHOD,F.SUN.METHOD)

RETURN

PROCESS:
*Reading the Parameter tables and getting the values


    CALL CACHE.READ(FN.SUN.PARAM,'SYSTEM',R.SUN.PARAM,PARAM.ERR)

    SUNNEL.ID  = "BE_K_TC.BE_P_RESCUENTATC_A"
    CALL F.READ(FN.SUN.METHOD,SUNNEL.ID,R.SUN.METHOD,F.SUN.METHOD,SUN.ERR)

    Y.ACTIVATION.KEY=R.SUN.PARAM<SP.ACT.KEY>
    SEC.MARK = R.SUN.PARAM<SP.SEC.MARKER>
    IN.MARK  = R.SUN.PARAM<SP.IN.FLD.MARKER>
    SF.MARK  = R.SUN.PARAM<SP.IN.SF.MARKER>
    Y.INPUT  = ''
    Y.OUTPUT = ''

    GOSUB GET.IN.ARRAY
    GOSUB GET.OUT.ARRAY

*Calling Sunnel interface

    Y.RESPONSE=CALLJEE(Y.ACTIVATION.KEY,Y.OUT.DATA)
    Y.MESSAGE = FIELD(Y.RESPONSE,SEC.MARK,1)
    IF Y.MESSAGE EQ 'SUCCESS' THEN
        Y.CARD.MESSAGE = FIELD(Y.RESPONSE,SEC.MARK,4)
        Y.CARD.TYPE    = FIELD(Y.RESPONSE,SF.MARK,3)
    END
    ELSE
        Y.CARD.TYPE = ''
    END
RETURN

GET.IN.ARRAY:
*Get the In Array Values

    Y.INP.FLD.NAME = R.SUN.METHOD<RE.GE.FLD.NAME>
    Y.INP.FLD.CNT  = DCOUNT(Y.INP.FLD.NAME,@SM)
    Y.INP.INIT = 1
    LOOP
        REMOVE Y.INP.ID FROM Y.INP.FLD.NAME SETTING Y.INP.POS
    WHILE Y.INP.INIT LE Y.INP.FLD.CNT
        Y.INP.1 = R.SUN.METHOD<RE.GE.FLD.NAME,1,Y.INP.INIT>
        Y.INP.2 = R.SUN.METHOD<RE.GE.FLD.TYPE,1,Y.INP.INIT>
        GOSUB CHECK.DETAILS
        IF Y.ACCT.VALUE THEN
            Y.INPUT := Y.INP.1:SF.MARK:Y.INP.2:IN.MARK:Y.ACCT.VALUE:SF.MARK
        END
        ELSE
            Y.INPUT := Y.INP.1:SF.MARK:Y.INP.2
        END
        Y.INP.INIT += 1 ;* R22 Auto conversion
    REPEAT

RETURN

GET.OUT.ARRAY:
*Getting Out Array Values

    Y.OUT.FLD.NAME     = R.SUN.METHOD<RE.GE.OT.NAME>
    Y.OUT.FLD.CNT      = DCOUNT(Y.OUT.FLD.NAME,@SM)
    Y.OUT.INIT = 1
    LOOP
        REMOVE Y.OUT.ID FROM Y.OUT.FLD.NAME SETTING Y.OUT.POS
    WHILE Y.OUT.INIT LE Y.OUT.FLD.CNT
        Y.OUT.1  = R.SUN.METHOD<RE.GE.OT.NAME,1,Y.OUT.INIT>
        Y.OUT.2  = R.SUN.METHOD<RE.GE.OT.TYPE,1,Y.OUT.INIT>
        Y.OUT.3 = Y.OUT.1:SF.MARK:Y.OUT.2
        IF Y.OUT.INIT LT Y.OUT.FLD.CNT THEN
            Y.OUTPUT := Y.OUT.1:SF.MARK:Y.OUT.2:IN.MARK
        END
        ELSE
            Y.OUTPUT := Y.OUT.1:SF.MARK:Y.OUT.2
        END
        Y.OUT.INIT += 1 ;* R22 Auto conversion
    REPEAT
    Y.OUT.DATA = SUNNEL.ID:SEC.MARK:1:SF.MARK:Y.INPUT:SEC.MARK:Y.OUTPUT

RETURN

CHECK.DETAILS:
    Y.ACCT.VALUE = Y.CARD.NO<Y.INP.INIT>
RETURN
