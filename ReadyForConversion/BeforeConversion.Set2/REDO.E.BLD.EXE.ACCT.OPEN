*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE REDO.E.BLD.EXE.ACCT.OPEN(ENQ.DATA)

    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_EQUATE
    $INCLUDE T24.BP I_ENQUIRY.COMMON
    $INCLUDE T24.BP I_F.DATES


    GOSUB INIT
    GOSUB PROCESS
    RETURN

INIT:
*****
    YL.TODAY = ''; SYSD.POS = ''; YTP.COMI = ''; SYSD.POS = ''
    LAST.WORK.DAY = R.DATES(EB.DAT.LAST.WORKING.DAY)
    RETURN

PROCESS:
********
    LOCATE "OPENING.DATE" IN ENQ.DATA<2,1> SETTING SYSD.POS1 THEN
        IF ENQ.DATA<4,SYSD.POS1> EQ '' THEN
            ENQ.DATA<4,SYSD.POS1> = LAST.WORK.DAY
        END
    END

    IF RUNNING.UNDER.BATCH EQ 1 THEN
        SYSD.POS = 0
        SYSD.POS = DCOUNT(ENQ.DATA<2>,VM)
        IF SYSD.POS EQ 0 THEN
            SYSD.POS = 1
        END ELSE
            SYSD.POS++
        END
        ENQ.DATA<2,SYSD.POS> = "SYSTEM.DATE"
        ENQ.DATA<3,SYSD.POS> = "EQ"
        ENQ.DATA<4,SYSD.POS> = LAST.WORK.DAY
    END
    RETURN

END
