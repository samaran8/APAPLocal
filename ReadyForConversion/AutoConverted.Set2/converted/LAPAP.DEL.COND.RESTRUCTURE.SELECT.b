SUBROUTINE LAPAP.DEL.COND.RESTRUCTURE.SELECT
*----------------------------------------------------------------------------------------------------------------------
*DESCRIPTION:
* This routine is the SELECT routine of the batch job REDO.B.LOAN.STATUS.ELIMINATION
*   which updates the local reference fields LOAN.STATUS and LOAN.CONDITIN based on the conditions
* This routine selects AA.ARR.OVERDUE application ids in which either the Loan Status or
*   Loan Condition field has value and passes the arrangement id alone to the record routine
* This routine removes the duplicate arrangement id if more than one OVERDUE record is present for the same arrangement
*
* ----------------------------------------------------------------------------------------------------------------------
* Input/Output:
*--------------
* IN  : -NA-
* OUT : -NA-
*
* Dependencies:
*---------------
* CALLS     : -NA-
* CALLED BY : -NA-
*
* Revision History:
*------------------
*   Date               who           Reference                     Description
* 03-JUN-2010   N.Satheesh Kumar  TAM-ODR-2009-10-0331           Initial Creation
*---------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_LOAN.STATUS.ELIMINATION.COMMON

    GOSUB SELECT.OD.IDS
RETURN

*-------------
SELECT.OD.IDS:
*-------------
*-------------------------------------------------------------------------------------------------------
* This section selects AA.ARR.OVERDUE records which has value either in Loan Status or Loan conditon field
*-------------------------------------------------------------------------------------------------------

    Y.CNTR = 0
    LOOP
    WHILE Y.CNTR LT 1 DO
        SEL.OFS = 'SELECT ':FN.OFS
        CALL EB.READLIST(SEL.OFS,SEL.LIS,'',NO.OFS,OF.ERR)
        IF NO.OFS EQ 0  THEN
            Y.CNTR = 2
        END
    REPEAT

    OD.SEL.CMD = 'SELECT ':FN.AA.ARRANGEMENT: ' WITH ARR.STATUS EQ CURRENT OR WITH ARR.STATUS EQ EXPIRED '
    CALL EB.READLIST(OD.SEL.CMD,OD.SEL.LST,'',NO.REC,SEL.ERR)
    PROCESSED.ARR.ID = OD.SEL.LST
    CALL BATCH.BUILD.LIST('',PROCESSED.ARR.ID)
RETURN

END
