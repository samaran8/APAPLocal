*-----------------------------------------------------------------------------
* <Rating>-21</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE REDO.E.NOF.BUILD.AZ.INT.FWD(ENQ.DATA)
*
* Description: This routine is attached to the Enquiry 'REDO.AZ.INT.FWD.RATE.PROB'
* Dev by: V.P.Ashokkumar
*
    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_EQUATE
    $INCLUDE T24.BP I_F.AZ.ACCOUNT

    GOSUB INIT
    GOSUB INIT.PROC
    RETURN

INIT:
****
    FN.AZ.ACCOUNT = 'F.AZ.ACCOUNT'; F.AZ.ACCOUNT = ''
    CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)
    SEL.CMD = ''; SEL.LST =''; SEL.REC = ''
    ERR.SEL = ''; DEP.POSN = ''
    RETURN

INIT.PROC:
**********
    SEL.CMD = "SELECT ":FN.AZ.ACCOUNT:" WITH TYPE.OF.SCHDLE EQ 'R' AND SCH.FIXED.RATE NE ''"
    CALL EB.READLIST(SEL.CMD,SEL.LST,'',SEL.REC,ERR.SEL)
    LOOP
        REMOVE DEPOS.ID FROM SEL.LST SETTING DEP.POSN
    WHILE DEPOS.ID:DEP.POSN

        ERR.AZ = ''; R.AZ.ACCOUNT = ''; YINT.CNT = 0; FLD1 = ''; FLD2 = ''; YCNT = 0
        CALL F.READ(FN.AZ.ACCOUNT,DEPOS.ID,R.AZ.ACCOUNT,F.AZ.ACCOUNT,ERR.AZ)
        FLD1 = R.AZ.ACCOUNT<AZ.CATEGORY>
        FLD2 = R.AZ.ACCOUNT<AZ.TYPE.OF.SCHDLE>
        YINT.CNT = DCOUNT(FLD2,VM)
        LOOP
        UNTIL YINT.CNT EQ 0
            YCNT = YCNT + 1
            FLD.2 = ''; FLD3 = ''
            FLD.2 = R.AZ.ACCOUNT<AZ.TYPE.OF.SCHDLE,YCNT>
            FLD3 = R.AZ.ACCOUNT<AZ.SCH.FIXED.RATE,YCNT>
            IF YCNT NE 1 THEN
                DEPOS.ID = ''; FLD1 = ''
            END
            ENQ.DATA<-1> = DEPOS.ID:'|':FLD.2:'|':FLD3:'|':FLD1

            YINT.CNT --
        REPEAT
    REPEAT
    RETURN

END
