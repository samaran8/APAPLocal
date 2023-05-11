*-----------------------------------------------------------------------------
* <Rating>-21</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE REDO.APAP.REPORTS.PASSW.RESET

    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_EQUATE
    $INCLUDE T24.BP I_F.USER
    $INCLUDE T24.BP I_TSA.COMMON
    $INCLUDE LAPAP.BP I_F.REDO.APAP.USER.PASSW.PARAM

    GOSUB INIT
    GOSUB PROCESS
    RETURN

INIT:
*****
    FN.USER = 'F.USER'; F.USER = ''
    CALL OPF(FN.USER,F.USER)
    FN.REDO.APAP.PASSW.RESET = 'F.REDO.APAP.USER.PASSW.PARAM'; F.REDO.APAP.PASSW.RESET = ''
    CALL OPF(FN.REDO.APAP.PASSW.RESET,F.REDO.APAP.PASSW.RESET)
    YSERVER.NAME = SERVER.NAME
    YRPT.SERV.NME = ''
    RETURN

PROCESS:
********
    ERR.REDO.APAP.PASSW.RESET = ''; R.REDO.APAP.PASSW.RESET = ''
    CALL CACHE.READ(FN.REDO.APAP.PASSW.RESET,'SYSTEM',R.REDO.APAP.PASSW.RESET,ERR.REDO.APAP.PASSW.RESET)
    YRPT.USER.ID = R.REDO.APAP.PASSW.RESET<REDO.USER.PARM.REPORT.USER>
    YRPT.USER.MENU = R.REDO.APAP.PASSW.RESET<REDO.USER.PARM.USER.MENU>
    YRPT.USER.SMS = R.REDO.APAP.PASSW.RESET<REDO.USER.PARM.USER.SMS.GRP>
    YRPT.SERV.NME = R.REDO.APAP.PASSW.RESET<REDO.USER.PARM.SERVER.NAME>
    IF YRPT.SERV.NME NE YSERVER.NAME THEN
        PRINT "Nombre del servidor que faltan"
        RETURN
    END

    YUSER.REC = YRPT.USER.ID
    CHANGE VM TO FM IN YRPT.USER.ID
    ERR.REDO.APAP.PASSW.RESET = ''; R.REDO.APAP.PASSW.RESET = ''
    CALL CACHE.READ(FN.REDO.APAP.PASSW.RESET,'SYSTEM',R.REDO.APAP.PASSW.RESET,ERR.REDO.APAP.PASSW.RESET)
    SEL.ERR =''; SEL.REC = ''; SEL.CNT = ''
    SEL.CMD = "SSELECT ":FN.USER
    CALL EB.READLIST(SEL.CMD,SEL.REC,'',SEL.CNT,SEL.ERR)
    LOOP
        REMOVE SEL.ID FROM SEL.REC SETTING SPOSN
    WHILE SEL.ID:SPOSN
        ERR.USER = ''; R.USER = ''
        CALL F.READ(FN.USER,SEL.ID,R.USER,F.USER,ERR.USER)
        YRPT.MENU.VAL = ''; YRPT.USERSMS.VAL = ''
        LOCATE SEL.ID IN YRPT.USER.ID SETTING POSN THEN
            YRPT.MENU.VAL = YRPT.USER.MENU<1,POSN>
            YRPT.USERSMS.VAL = YRPT.USER.SMS<1,POSN>

            IF YRPT.MENU.VAL THEN
                R.USER<EB.USE.INIT.APPLICATION> = "?":YRPT.MENU.VAL
            END ELSE
                PRINT "Men� faltante en el par�metro ":SEL.ID
            END
            IF YRPT.USERSMS.VAL THEN
                R.USER<EB.USE.APPLICATION> = "@":YRPT.USERSMS.VAL
            END ELSE
                R.USER<EB.USE.FUNCTION> = "S L"
                PRINT "El permiso que falta en el par�metro ":SEL.ID
            END
            IF NOT(YRPT.MENU.VAL) AND NOT(YRPT.USERSMS.VAL) THEN
                ER<EB.USE.END.TIME> = '0002'
            END
        END ELSE
            R.USER<EB.USE.END.TIME> = '0002'
        END
        CALL F.WRITE(FN.USER,SEL.ID,R.USER)
        CALL JOURNAL.UPDATE('')
    REPEAT
    PRINT "Listo"
    RETURN
END
