*-----------------------------------------------------------------------------
* <Rating>-41</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE REDO.E.BLD.LAST.REC.DAY(ENQ.DATA)

*******************************************************************
* Description: The Build routine to default the last calendar date
* Attached to: ENQ 'REDO.BALANZACOMPROBACION'
* Dev By     : V.P.Ashokkumar
********************************************************************
* Date         Author                Description
* ==========   =================     ============
* 01-Dec-2017  Ashokkumar            CN005317- The value should be checked even for the field name is left blank
* Fecha modificacion       Author                Description
* ==========            =================     ============
* 11-feb-2022             APAP                MDP-2502 Mejorar el tiempo del proceso el mismo dura 11 hora
******************************************************************************************
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
    LOCATE "SYSTEM.DATE" IN ENQ.DATA<2,1> SETTING SYSD.POS THEN
        IF ENQ.DATA<2,SYSD.POS> EQ "SYSTEM.DATE" THEN
            YL.TODAY = ENQ.DATA<4,SYSD.POS>
            IF YL.TODAY EQ '' THEN
                GOSUB GET.DATE.VALUE
            END
        END
    END ELSE
        GOSUB GET.DATE.VALUE
    END
    ENQ.DATA<2> = ''
    ENQ.DATA<3> = ''
    ENQ.DATA<4> = ''
    ENQ.DATA<2> = '@ID'
    ENQ.DATA<3> = 'LK'
    ENQ.DATA<4> = 'MBGL...':YL.TODAY:'...':' ':'MBPL...':YL.TODAY:'...'         ;* Replace the report name with whatever report you want to generate.
    RETURN
GET.DATE.VALUE:
***************
    YTP.COMI = COMI
    YL.TODAY = TODAY
    CALL CDT('',YL.TODAY,'-1C')
    IF LAST.WORK.DAY[5,2] NE YL.TODAY[5,2] THEN
        COMI = LAST.WORK.DAY[1,6]:'01'
        CALL LAST.DAY.OF.THIS.MONTH
        YL.TODAY = COMI
    END
    COMI = YTP.COMI
    RETURN

END
