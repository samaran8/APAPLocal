*-----------------------------------------------------------------------------
* <Rating>-1</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE REDO.DISB.E.AUTH.INAO.IN(ENQ.DATA)

    $INCLUDE T24.BP I_EQUATE
    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_ENQUIRY.COMMON
    $INCLUDE TAM.BP I_F.REDO.CREATE.ARRANGEMENT
    $INCLUDE TAM.BP I_F.REDO.DISB.CHAIN

    FN.REDO.DISB.CHAIN = 'F.REDO.DISB.CHAIN'; F.REDO.DISB.CHAIN = ''
    CALL OPF(FN.REDO.DISB.CHAIN,F.REDO.DISB.CHAIN)
    FN.REDO.CREATE.ARRANGEMENT = 'F.REDO.CREATE.ARRANGEMENT'; F.REDO.CREATE.ARRANGEMENT = ''
    CALL OPF(FN.REDO.CREATE.ARRANGEMENT,F.REDO.CREATE.ARRANGEMENT)
*DEBUG
    ID.CC = ID.COMPANY
    SEL.DET = ''; SEL.CMD = ''; SEL.LIST = ''
    SEL.CMD="SELECT ":FN.REDO.DISB.CHAIN:" WITH DISB.STATUS EQ 'AP' AND BRANCH.ID EQ " : ID.CC
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,ERR.SLT)
    LOOP
        REMOVE SEL.ID FROM SEL.LIST SETTING SEL.POSN
    WHILE SEL.ID:SEL.POSN
        ERR.REDO.DISB.CHAIN = ''; R.REDO.DISB.CHAIN = ''
        CALL F.READ(FN.REDO.DISB.CHAIN ,SEL.ID,R.REDO.DISB.CHAIN,F.REDO.DISB.CHAIN,ERR.REDO.DISB.CHAIN)
        ARR.ID = R.REDO.DISB.CHAIN<DS.CH.RCA.ID>
        R.REDO.CREATE.ARRANGEMENT = ''; ERR.REDO.CREATE.ARRANGEMENT = ''
        CALL F.READ (FN.REDO.CREATE.ARRANGEMENT,ARR.ID,R.REDO.CREATE.ARRANGEMENT,F.REDO.CREATE.ARRANGEMENT,ERR.REDO.CREATE.ARRANGEMENT)

        A.AMOUNT = R.REDO.CREATE.ARRANGEMENT<REDO.FC.DIS.AMT.TOT>

        IF A.AMOUNT LE 300000 THEN
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
