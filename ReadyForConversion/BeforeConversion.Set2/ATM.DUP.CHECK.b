*-----------------------------------------------------------------------------
* <Rating>-40</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE ATM.DUP.CHECK(UNIQUE.ID,OUTGOING)
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ATM.REVERSAL

*

    GOSUB OPEN.FILES
    GOSUB PROCESS
    RETURN


OPEN.FILES:

    FN.ATM.REVERSAL = 'F.ATM.REVERSAL'
    CALL OPF(FN.ATM.REVERSAL,F.ATM.REVERSAL)


    RETURN

PROCESS:
    UNIQUEID = UNIQUE.ID
    CALL F.READ(FN.ATM.REVERSAL,UNIQUEID,R.ATM.REVERSAL,F.ATM.REVERSAL,ERR.REV)
    IF R.ATM.REVERSAL THEN

        GOSUB FORM.OUTGOING
    END ELSE
        GOSUB FORM.OUTGOING1
    END
    RETURN

FORM.OUTGOING:
*-------------*
    OUTGOING = ''
    OUTGOING = 'gOFSUtilName:1:1=FUNDS.TRANSFER,ATM.DUP'
    OUTGOING:= '$':'gOFSFunction:1:1=I'
    FT.ID = ""
    OUTGOING := '$':'gOFSId:1:1=':FT.ID:'$'


    RETURN          ;*From FORM.OUTGOING

FORM.OUTGOING1:
*-------------*
    OUTGOING = ''
    OUTGOING = 'gOFSUtilName:1:1=FUNDS.TRANSFER,ATM.FP'
    OUTGOING:= '$':'gOFSFunction:1:1=I'
    FT.ID = ""
    OUTGOING := '$':'gOFSId:1:1=':FT.ID:'$'


    RETURN          ;*From FORM.OUTGOING
