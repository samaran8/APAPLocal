*-----------------------------------------------------------------------------
* <Rating>-21</Rating>
*-----------------------------------------------------------------------------

    SUBROUTINE DR.REG.FD01.CONCAT.UPD
*-----------------------------------------------------------------------------
* Company Name   : APAP
* Description	:
*------------
* This COB routine will copy the last working date TT / FT from POS.MVMT.TODAY
* before deleting from the file to overcome core functionality for FD01 & Divisas reports.
*----------------------------------------------------------------------------
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.DATES
    $INSERT T24.BP I_F.POS.MVMT.TODAY


    GOSUB INIT
    GOSUB PROCESS
    RETURN

INIT:
*****
    R.DR.REG.FD01.CONCAT = ''
    FN.POS.MVMT.TDY = 'F.POS.MVMT.TODAY'; F.POS.MVMT.TDY = ''
    CALL OPF(FN.POS.MVMT.TDY,F.POS.MVMT.TDY)
    Y.LAST.WRK.DAY = R.DATES(EB.DAT.LAST.WORKING.DAY)
    FN.DR.REG.FD01.CONCAT = 'F.DR.REG.FD01.CONCAT'; F.DR.REG.FD01.CONCAT = ''
    CALL OPF(FN.DR.REG.FD01.CONCAT,F.DR.REG.FD01.CONCAT)
    YLCCY = LCCY
    CALL EB.CLEAR.FILE(FN.DR.REG.FD01.CONCAT, F.DR.REG.FD01.CONCAT)
    RETURN

PROCESS:
********

    SEL.CMD = ''; PMVMT.IDS = ''; PMVMT.LIST = '';  SEL.STS = ''
    SEL.CMD = "SELECT ":FN.POS.MVMT.TDY:" WITH SYSTEM.ID EQ 'TT' 'FT' AND BOOKING.DATE EQ ":Y.LAST.WRK.DAY:" AND CURRENCY NE ":YLCCY
    CALL EB.READLIST(SEL.CMD,PMVMT.IDS,'',PMVMT.LIST,SEL.STS)
    LOOP
        REMOVE POSITION.ID FROM PMVMT.IDS SETTING POSN
    WHILE POSITION.ID:POSN

        R.POS.MVMT.TODAY = ''; ERR.POS.MVMT.TODAY = ''
        CALL F.READ(FN.POS.MVMT.TDY,POSITION.ID,R.POS.MVMT.TODAY,F.POS.MVMT.TDY,ERR.POS.MVMT.TODAY)
        Y.TT.CCY = R.POS.MVMT.TODAY<PSE.CURRENCY>
        IF Y.TT.CCY EQ YLCCY THEN
            CONTINUE
        END
        REFERN.ID = R.POS.MVMT.TODAY<PSE.OUR.REFERENCE>
        R.DR.REG.FD01.CONCAT<-1> = REFERN.ID
    REPEAT
    YCON.ID = Y.LAST.WRK.DAY
    WRITE R.DR.REG.FD01.CONCAT TO F.DR.REG.FD01.CONCAT,YCON.ID ON ERROR NULL
    RETURN

END
