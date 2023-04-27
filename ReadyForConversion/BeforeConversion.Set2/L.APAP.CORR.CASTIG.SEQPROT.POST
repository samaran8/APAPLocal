*-----------------------------------------------------------------------------
* <Rating>-1</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.CORR.CASTIG.SEQPROT.POST
*
* Client Name   : APAP
* Develop By    : Ashokkumar
* Description   : The routine to adjust the Insurance (SEGPROTFIN1) amount for the castigado prestamos.
*
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_BATCH.FILES


    FN.SAVE.LST = '&SAVEDLISTS&'
    F.SAVE.LST = ''
    CALL OPF(FN.SAVE.LST,F.SAVE.LST)

    C.TABLE.ID = "CASTIGADO_SEGPROTFIN1.PRESTAMOS.txt"
    YGRP.ARR = "ARRANGEMENT.ID,BILL.ID,SEGPROTFIN1 AMOUNT"
    SAVE.ERR = ''; R.SAVE.LST = ''
    CALL F.READ(FN.SAVE.LST,C.TABLE.ID,R.SAVE.LST,F.SAVE.LST,SAVE.ERR)
    IF R.SAVE.LST THEN
        DELETE F.SAVE.LST,C.TABLE.ID
    END

    SEL.LIST = ''; SEL.REC = ''; SEL.CMD = ''
    SEL.CMD = "SELECT ":FN.SAVE.LST:" LIKE AK_CASTIGADO..."
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',SEL.REC,SEL.ERR)
    LOOP
        REMOVE SEL.ID FROM SEL.LIST SETTING SL.POSN
    WHILE SEL.ID:SL.POSN
        SAVE.ERR = ''; R.SAVE.LST = ''
        CALL F.READ(FN.SAVE.LST,SEL.ID,R.SAVE.LST,F.SAVE.LST,SAVE.ERR)
        DELETE F.SAVE.LST,SEL.ID
        YGRP.ARR<-1> = R.SAVE.LST
    REPEAT

    WRITE YGRP.ARR ON F.SAVE.LST,C.TABLE.ID

    RETURN
END
