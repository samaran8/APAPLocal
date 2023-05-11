SUBROUTINE LAPAP.ENQ.DOP80017.RT
*--------------------------------------------------------------------------------------------------------
*--------------------------------------------------------------------------------------------------------
*Description  : This is a ID routine to from ENQUIRY.SERVICE
*Linked With version  : ENQUIRY.REPORT,DOP80017
*In Parameter : N/A
*Out Parameter: N/A
*--------------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
*    Date            Who                  Reference               Description
*   ------         ------               -------------            -------------
*  23/09/2020    Juan Garcia               MDP-1200             Initial Creation
*--------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_F.ENQUIRY.REPORT

    GOSUB PROCESS

RETURN

PROCESS:
********

    COMI = 'LAPAP.STMT.ENTRY.DOP80017'

RETURN

*--------------------------------------------------------------------------------------------------------
