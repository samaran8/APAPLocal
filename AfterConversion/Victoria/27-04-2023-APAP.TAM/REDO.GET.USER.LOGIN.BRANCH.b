$PACKAGE APAP.TAM
SUBROUTINE REDO.GET.USER.LOGIN.BRANCH
*------------------------------------------------------------------------------
* Description: This routine is used to get the user login branch for Rbhp issue PACS00247789
** 10-04-2023 R22 Auto Conversion - FM TO @FM, VM to @VM, SM to @SM
** 10-04-2023 Skanda R22 Manual Conversion - No changes
*------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_System
    $INSERT I_F.REDO.EXCEP.REC.PARAM

    GOSUB OPEN.PROCESS

    GOSUB GET.PARAM.VALUE

    IF Y.APP.FOUND EQ 'Y' THEN

        GOSUB PROCESS

    END

RETURN
*-----------------------------
OPEN.PROCESS:
*--------------------------------

    FN.REDO.EXCEP.REC.PARAM = 'F.REDO.EXCEP.REC.PARAM'

    Y.APP.FOUND = ''

RETURN

*---------------
GET.PARAM.VALUE:
*---------------

    PARAM.ID = 'SYSTEM'

    CALL CACHE.READ(FN.REDO.EXCEP.REC.PARAM,PARAM.ID,R.REDO.EXCEP.REC.PARAM,PARAM.ERR)

    Y.RBHP.APP = R.REDO.EXCEP.REC.PARAM<EXCEP.RBHP.APPS>

    CHANGE @VM TO @FM IN Y.RBHP.APP

    LOCATE APPLICATION IN Y.RBHP.APP SETTING POS THEN
        Y.APP.FOUND = 'Y'
    END ELSE
        Y.APP.FOUND = 'N'
    END

RETURN

*------------------------------
PROCESS:
*------------------------------

    CURRNT.COMP = System.getVariable("CURRENT.USER.BRANCH")
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN ;* R22 Auto conversion
        CURRNT.COMP = "" ;* R22 Auto conversion
    END ;* R22 Auto conversion



    IF CURRNT.COMP EQ "CURRENT.USER.BRANCH" THEN
        LOCATE 'EB-UNKNOWN.VARIABLE' IN E<1,1> SETTING POS THEN
            E = ''
        END
        VAR.BRANCH.ID = ID.COMPANY
        CALL System.setVariable("CURRENT.USER.BRANCH",VAR.BRANCH.ID)
    END

RETURN
*---------------
END
