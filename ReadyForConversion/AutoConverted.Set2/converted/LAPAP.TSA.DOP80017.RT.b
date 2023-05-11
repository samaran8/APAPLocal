SUBROUTINE LAPAP.TSA.DOP80017.RT
*--------------------------------------------------------------------------------------------------------
*--------------------------------------------------------------------------------------------------------
*Description  : This is a ID routine to from ENQUIRY.SERVICE
*Linked With version  : TSA.SERVICE,DOP80017
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

    GOSUB PROCESS

RETURN

PROCESS:
********

    COMI = 'BNK/LAPAP.STMT.ENTRY.DOPACC'

RETURN

*--------------------------------------------------------------------------------------------------------
