*-----------------------------------------------------------------------------
* <Rating>279</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE REDO.L.INTRANSIT.LOCK.CORR

**********************************************************************************
* Developed By : Ashokkumar
* Client Name  : APAP
* Description  : Routine to delete the unused or wrong locked record from the REDO.INTRANSIT.LOCK table.
**********************************************************************************

    $INSERT I_COMMON
    $INSERT I_EQUATE


    GOSUB INIT
    GOSUB PROCESS
    RETURN

INIT:
******
    FN.SAVELIST = '&SAVEDLISTS&'; F.SAVELIST = ''
    OPEN FN.SAVELIST TO F.SAVELIST ELSE RETURN
    FN.REDO.INTRANSIT.LOCK = 'F.REDO.INTRANSIT.LOCK'; F.REDO.INTRANSIT.LOCK = ''
    CALL OPF(FN.REDO.INTRANSIT.LOCK,F.REDO.INTRANSIT.LOCK)
    SAVE.ID = 'REGISTER.TO.DELETE.csv'
    READ R.SAVELIST FROM F.SAVELIST,SAVE.ID ELSE RETURN
    RETURN

PROCESS:
********
    LOOP
        REMOVE INTRANSIT.LOCK.ID FROM R.SAVELIST SETTING INT.POSN
    WHILE INTRANSIT.LOCK.ID:INT.POSN
        CRT INTRANSIT.LOCK.ID
        YINTRANSIT.LOCK.ID = FIELD(INTRANSIT.LOCK.ID,',',1)
        YAC.LOCK.ID = FIELD(INTRANSIT.LOCK.ID,',',2)
        ERR.REDO.INTRANSIT.LOCK = ''; R.REDO.INTRANSIT.LOCK = ''
        CALL F.READ(FN.REDO.INTRANSIT.LOCK,YINTRANSIT.LOCK.ID,R.REDO.INTRANSIT.LOCK,F.REDO.INTRANSIT.LOCK,ERR.REDO.INTRANSIT.LOCK)
        IF R.REDO.INTRANSIT.LOCK THEN
            LOCATE YAC.LOCK.ID IN R.REDO.INTRANSIT.LOCK SETTING YFM.POSN THEN
                DEL R.REDO.INTRANSIT.LOCK<YFM.POSN>
                YCNT = 0; YCNT = DCOUNT(R.REDO.INTRANSIT.LOCK,FM)
                IF YCNT GE 1 THEN
                    CALL F.WRITE(FN.REDO.INTRANSIT.LOCK,YINTRANSIT.LOCK.ID,R.REDO.INTRANSIT.LOCK)
                END ELSE
                    CALL F.DELETE(FN.REDO.INTRANSIT.LOCK,YINTRANSIT.LOCK.ID)
                END
                CALL JOURNAL.UPDATE('')
            END
        END
    REPEAT

    RETURN

END
