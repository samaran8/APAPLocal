SUBROUTINE REDO.GET.ARRANGEMENT.STATUS.LOAD
*DESCRIPTION:
*------------
* This is the COB routine for the B51 development and this is Load routine
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
    $INSERT I_GTS.COMMON
    $INSERT I_REDO.GET.ARRANGEMENT.STATUS.COMMON

*------------------------------------------------------------------------------------------------------------------
*Main Logic of the routine
*
MAIN.LOGIC:

    GOSUB OPEN.FILES
    GOSUB GET.LOCAL.FIELDS

RETURN
*------------------------------------------------------------------------------------------------------------------
*List of files to be opened
*
OPEN.FILES:

    FN.AA.ARR.OVERDUE = 'F.AA.ARR.OVERDUE'
    F.AA.ARR.OVERDUE = ''
    CALL OPF(FN.AA.ARR.OVERDUE, F.AA.ARR.OVERDUE)

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT, F.ACCOUNT)

    FN.AA.ARRANGEMENT = 'F.AA.ARRANGEMENT'
    F.AA.ARRANGEMENT = ''
    CALL OPF(FN.AA.ARRANGEMENT, F.AA.ARRANGEMENT)

RETURN
*------------------------------------------------------------------------------------------------------------------
* Get the position of the Local fields
*
GET.LOCAL.FIELDS:

    LOC.REF.APP = "AA.ARR.OVERDUE":@FM:"ACCOUNT"
    LOC.REF.FIELDS = 'L.LOAN.STATUS.1':@FM:'L.LOAN.STATUS'

    CALL MULTI.GET.LOC.REF(LOC.REF.APP,LOC.REF.FIELDS,LOC.REF.POS)

    Y.OD.LOAN.STATUS.POS = LOC.REF.POS<1,1>
    Y.AC.LOAN.STATUS.POS = LOC.REF.POS<2,1>

RETURN
*------------------------------------------------------------------------------------------------------------------
END
