*-----------------------------------------------------------------------------
* <Rating>-1</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE REDO.PART.DISB.E.AUTHOR.IN2(ENQ.DATA)

    $INCLUDE T24.BP I_EQUATE
    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_ENQUIRY.COMMON
    $INCLUDE TAM.BP I_F.REDO.AA.PART.DISBURSE.FC
    $INCLUDE TAM.BP I_F.REDO.DISB.CHAIN

*REDO.AA.PART.DISBURSE.FC

    FN.REDO.DISB.CHAIN = 'F.REDO.DISB.CHAIN'; F.REDO.DISB.CHAIN = ''
    CALL OPF(FN.REDO.DISB.CHAIN,F.REDO.DISB.CHAIN)
    FN.REDO.AA.PART.DISBURSE.FC = 'F.REDO.AA.PART.DISBURSE.FC'; F.REDO.AA.PART.DISBURSE.FC = ''
    CALL OPF(FN.REDO.AA.PART.DISBURSE.FC,F.REDO.AA.PART.DISBURSE.FC)
*DEBUG
    ID.CC = ID.COMPANY
    SEL.DET = ''; SEL.CMD = ''; SEL.LIST = ''
    SEL.CMD="SELECT ":FN.REDO.DISB.CHAIN:" WITH DISB.STATUS EQ 'UP' AND BRANCH.ID EQ " : ID.CC
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,ERR.SLT)
    LOOP
        REMOVE SEL.ID FROM SEL.LIST SETTING SEL.POSN
    WHILE SEL.ID:SEL.POSN
        ERR.REDO.DISB.CHAIN = ''; R.REDO.DISB.CHAIN = ''
        CALL F.READ(FN.REDO.DISB.CHAIN ,SEL.ID,R.REDO.DISB.CHAIN,F.REDO.DISB.CHAIN,ERR.REDO.DISB.CHAIN)
        ARR.ID = R.REDO.DISB.CHAIN<DS.CH.RPD.ID>
        R.REDO.AA.PART.DISBURSE.FC = ''; ERR.REDO.AA.PART.DISBURSE.FC = ''
        CALL F.READ (FN.REDO.AA.PART.DISBURSE.FC,ARR.ID,R.REDO.AA.PART.DISBURSE.FC,F.REDO.AA.PART.DISBURSE.FC,ERR.REDO.AA.PART.DISBURSE.FC)

        A.AMOUNT = R.REDO.AA.PART.DISBURSE.FC<REDO.PDIS.DIS.AMT.TOT>

        IF A.AMOUNT LE 200000 THEN
            SEL.DET<-1> = SEL.ID
        END

    REPEAT

    CHANGE FM TO SM IN SEL.DET
    IF SEL.DET THEN
        ENQ.DATA<2,-1>='@ID'
        ENQ.DATA<3,-1>='EQ'
        ENQ.DATA<4,-1>=SEL.DET
    END
    RETURN
END
