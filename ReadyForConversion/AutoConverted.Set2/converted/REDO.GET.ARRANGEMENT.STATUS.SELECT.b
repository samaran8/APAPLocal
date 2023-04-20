SUBROUTINE REDO.GET.ARRANGEMENT.STATUS.SELECT

*DESCRIPTION:
*------------
* This is the COB routine for the B51 development and this is Select routine
*
* Input/Output:
*--------------
* IN : -NA-
* OUT : -NA-
*---------------
*-----------------------------------------------------------------------------------------------------------------
* Modification History :
*   Date            Who                   Reference               Description
*   ------         ------               -------------            -------------
* 02 Sep 2010    Ravikiran AV              B.51                  Initial Creation
*
*-------------------------------------------------------------------------------------------------------------------

* All File INSERTS done here
*
    $INSERT I_COMMON
    $INSERT I_EQUATE

*------------------------------------------------------------------------------------------------------------------
*Main Logic of the routine
*
MAIN.LOGIC:

    GOSUB OPEN.FILES
    GOSUB PROCESS

RETURN
*------------------------------------------------------------------------------------------------------------------
* Files to be opened for processing
*
OPEN.FILES:

    FN.REDO.AA.OVERDUE.LOAN.STATUS = 'F.REDO.AA.OVERDUE.LOAN.STATUS'
    F.REDO.AA.OVERDUE.LOAN.STATUS = ''
    CALL OPF(FN.REDO.AA.OVERDUE.LOAN.STATUS, F.REDO.AA.OVERDUE.LOAN.STATUS)

RETURN
*------------------------------------------------------------------------------------------------------------------
* Load the Arrangement ids for Multi-Threaded Processing
*
PROCESS:

    SELECT.CMD = "SELECT ":FN.REDO.AA.OVERDUE.LOAN.STATUS
    CALL EB.READLIST(SELECT.CMD,SEL.LIST,'',NO.REC,PGM.ERR)

    CALL BATCH.BUILD.LIST('', SEL.LIST)

RETURN
*------------------------------------------------------------------------------------------------------------------
END
