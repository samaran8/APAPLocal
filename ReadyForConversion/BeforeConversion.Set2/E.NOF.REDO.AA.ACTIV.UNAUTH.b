*-----------------------------------------------------------------------------
* <Rating>27</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE E.NOF.REDO.AA.ACTIV.UNAUTH(ENQ.DATA)
*--------------------------------------------------------------------------------------------------
*  M O D I F I C A T I O N S
* ***************************
*--------------------------------------------------------------------------------------------------
* Defect Reference       Modified By                    Date of Change        Change Details
*
*                       Ashokkumar.V.P                  07/09/2015      .     Enquiry to get all the unauthorised Lending AA activities
*--------------------------------------------------------------------------------------------------
*
    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_EQUATE
    $INCLUDE T24.BP I_ENQUIRY.COMMON
    $INCLUDE T24.BP I_F.AA.ARRANGEMENT
    $INCLUDE T24.BP I_F.AA.ARRANGEMENT.ACTIVITY
    $INCLUDE T24.BP I_F.FUNDS.TRANSFER
    $INCLUDE T24.BP I_F.AA.REFERENCE.DETAILS


    GOSUB INIT
    GOSUB MAIN.PROCESS
    ENQ.DATA = YARRY
    RETURN

INIT:
*****
    YTODAY.DAT = TODAY; YARRY = ''
    FN.AA.ARR = 'F.AA.ARRANGEMENT'
    F.AA.ARR = ''
    CALL OPF(FN.AA.ARR,F.AA.ARR)

    FN.AA.ARRANGEMENT.ACTIVITY.U = "F.AA.ARRANGEMENT.ACTIVITY$NAU"
    F.AA.ARRANGEMENT.ACTIVITY.U = ''
    CALL OPF(FN.AA.ARRANGEMENT.ACTIVITY.U,F.AA.ARRANGEMENT.ACTIVITY.U)

    FN.AA.ARRANGEMENT.ACTIVITY = 'F.AA.ARRANGEMENT.ACTIVITY'
    F.AA.ARRANGEMENT.ACTIVITY = ''
    CALL OPF(FN.AA.ARRANGEMENT.ACTIVITY,F.AA.ARRANGEMENT.ACTIVITY)

    FN.FUNDS.TRANS.U = 'F.FUNDS.TRANSFER$NAU'
    F.FUNDS.TRANS.U = ''
    CALL OPF(FN.FUNDS.TRANS.U,F.FUNDS.TRANS.U)

    FN.FUNDS.TRANS = 'F.FUNDS.TRANSFER'
    F.FUNDS.TRANS = ''
    CALL OPF(FN.FUNDS.TRANS,F.FUNDS.TRANS)

    FN.AA.REFERENCE.DETAILS = 'F.AA.REFERENCE.DETAILS'; F.AA.REFERENCE.DETAILS = ''
    CALL OPF(FN.AA.REFERENCE.DETAILS,F.AA.REFERENCE.DETAILS)
    RETURN

MAIN.PROCESS:
*************
    SEL.AA.ACT = ''; SEL.LISTA =''; SEL.CNTA = ''; ERR.SELA = ''
    SEL.AA.ACT = "SSELECT ":FN.AA.ARRANGEMENT.ACTIVITY.U:" BY ARRANGEMENT"
    CALL EB.READLIST(SEL.AA.ACT,SEL.LISTA,'',SEL.CNTA,ERR.SELA)
    LOOP
        REMOVE AA.ACT.ID FROM SEL.LISTA SETTING AA.POSN
    WHILE AA.ACT.ID:AA.POSN
        TEMP.AA.ACT.ID = AA.ACT.ID
        YRECORD.STAT = ''; MASTER.AAA = ''; YEFFECT.DTE = ''; YORIG.TXN.AMT = ''; YCONTRACT.VAL = ''
        GOSUB READ.AA.ACT.NAU
        YRECORD.STAT = R.AA.ACT<AA.ARR.ACT.RECORD.STATUS>
        YCONTRACT.VAL = R.AA.ACT<AA.ARR.ACT.TXN.CONTRACT.ID>
        YARRANGEMENT = R.AA.ACT<AA.ARR.ACT.ARRANGEMENT>
        YACTIVITY = R.AA.ACT<AA.ARR.ACT.ACTIVITY>
        IF YACTIVITY[1,17] EQ 'INTERNET.SERVICES' THEN CONTINUE

        YMASTER.AAA = R.AA.ACT<AA.ARR.ACT.MASTER.AAA>
        YORIG.TXN.AMT = R.AA.ACT<AA.ARR.ACT.TXN.AMOUNT>
        YEFFECT.DTE = R.AA.ACT<AA.ARR.ACT.EFFECTIVE.DATE>
        YAA.RECORD.STATUS = R.AA.ACT<AA.ARR.ACT.RECORD.STATUS>
*        IF YRECORD.STAT[1,1] EQ 'R' THEN
*            CONTINUE
*        END

        IF YCONTRACT.VAL[1,5] EQ 'AAACT' THEN
            YTEMP.R.AA = R.AA.ACT
            AA.ACT.ID = YMASTER.AAA
            GOSUB READ.AA.ARR.ACT
        END
        IF NOT(YCONTRACT.VAL) THEN
            GOSUB READ.AA.REF.DET
        END

        YFT.RECSTAT = ''
        IF YCONTRACT.VAL[1,2] EQ 'FT' THEN
            GOSUB READ.FT
        END

        IF YMASTER.AAA EQ TEMP.AA.ACT.ID THEN
            YMASTER.AAA = ''
        END
*        IF YORIG.TXN.AMT NE 0 AND YORIG.TXN.AMT THEN
        YARRY<-1> = YARRANGEMENT:"*":YACTIVITY:"*":YORIG.TXN.AMT:"*":TEMP.AA.ACT.ID:"*":YAA.RECORD.STATUS:"*":YCONTRACT.VAL:"*":YFT.RECSTAT:"*":YMASTER.AAA:"*":YEFFECT.DTE
*        END
    REPEAT
    RETURN

READ.AA.ARR.ACT:
****************
    ERR.AA.ACT = ''; R.AA.ACT = ''
    CALL F.READ(FN.AA.ARRANGEMENT.ACTIVITY,AA.ACT.ID,R.AA.ACT,F.AA.ARRANGEMENT.ACTIVITY,ERR.AA.ACT)
    IF ERR.AA.ACT THEN
        GOSUB READ.AA.ACT.NAU
    END
    YCONTRACT.VAL = R.AA.ACT<AA.ARR.ACT.TXN.CONTRACT.ID>
    RETURN

READ.AA.REF.DET:
****************
    R.AA.TXN.DET = ''; TXN.REF.ERR= ''; Y.ACTIV.REF = ''
    CALL F.READ(FN.AA.REFERENCE.DETAILS,AA.ACT.ID,R.AA.TXN.DET,F.AA.REFERENCE.DETAILS,TXN.REF.ERR)
    IF R.AA.TXN.DET THEN
        Y.ACTIV.REF = R.AA.TXN.DET<AA.REF.AAA.ID>
        LOCATE YARRANGEMENT IN Y.ACTIV.REF<1,1> SETTING YPOSN THEN
            YCONTRACT.VAL = R.AA.TXN.DET<AA.REF.TRANS.REF,YPOSN>
        END
    END
    RETURN

READ.AA.ACT.NAU:
*****************
    ERR.AA.ACT = ''; R.AA.ACT = ''
    CALL F.READ(FN.AA.ARRANGEMENT.ACTIVITY.U,AA.ACT.ID,R.AA.ACT,F.AA.ARRANGEMENT.ACTIVITY.U,ERR.AA.ACT)
    RETURN

READ.FT:
********
    R.FUNDS.TRANS = ''; ERR.FUNDS.TRANS = ''; YFT.RECSTAT = ''
    CALL F.READ(FN.FUNDS.TRANS,YCONTRACT.VAL,R.FUNDS.TRANS,F.FUNDS.TRANS,ERR.FUNDS.TRANS)
    IF ERR.FUNDS.TRANS THEN
        GOSUB READ.FT.NAU
    END
    YFT.RECSTAT = R.FUNDS.TRANS<FT.RECORD.STATUS>
    RETURN

READ.FT.NAU:
*************
    R.FUNDS.TRANS = ''; ERR.FUNDS.TRANS = ''
    CALL F.READ(FN.FUNDS.TRANS.U,YCONTRACT.VAL,R.FUNDS.TRANS,F.FUNDS.TRANS.U,ERR.FUNDS.TRANS)
    RETURN

END
