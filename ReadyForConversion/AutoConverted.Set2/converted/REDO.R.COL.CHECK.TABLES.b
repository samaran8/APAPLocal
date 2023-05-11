SUBROUTINE REDO.R.COL.CHECK.TABLES(P.TABLES.TO.PROCESS)
*-----------------------------------------------------------------------------
* Name : REDO.ROUTINE.COLLECTOR.CHECK.TABLES
*      : On Reprocessing stage, it allows to find out if there are tables that were not processed
*
*------------------------------------------------------------------------------
* @author hpasquel@temenos.com
* @stereotype subroutine
* @package REDO.COL
*
* @Parameters:
* ----------------------------------------------------------------------------
*             P.TABLES.TO.PROCESS   (out)  list of tables that must be reprocessed
*-----------------------------------------------------------------------------
*  HISTORY CHANGES:
*                  2012-05-15 - Code Review by performance issues
*                               hpasquel@temenos.com
*
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
*
    GOSUB INITIALISE
    GOSUB PROCESS
RETURN
*-----------------------------------------------------------------------------
INITIALISE:
*-----------------------------------------------------------------------------
    FN.REDO.COL.MSG.QUEUE='F.REDO.MSG.COL.QUEUE'
    F.REDO.COL.MSG.QUEUE = ''
    CALL OPF(FN.REDO.COL.MSG.QUEUE,F.REDO.COL.MSG.QUEUE)
*                     1                         2                             3                          4                     5
    C.TABLES  = 'TMPCLIENTES' : @VM :  'TMPTELEFONOSCLIENTE' : @VM : 'TMPDIRECCIONESCLIENTE' : @VM : 'TMPMOVIMIENTOS' : @VM : 'TMPCREDITO'
    P.TABLES.TO.PROCESS = C.TABLES
RETURN
*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------

    LOOP
        REMOVE Y.TABLE FROM C.TABLES SETTING Y.POS
    WHILE Y.TABLE
        GOSUB CHECK.TABLE
        IF NO.REC.SEL LE 0 THEN
            LOCATE Y.TABLE IN C.TABLES<1,1> SETTING Y.TAB.POS THEN
                P.TABLES.TO.PROCESS<1,Y.TAB.POS> = ''
            END
        END
    REPEAT

RETURN
*-----------------------------------------------------------------------------
CHECK.TABLE:
*-----------------------------------------------------------------------------

    Y.SELECT.TABLES = ""
    Y.SELECT.TABLES := 'SELECT ':FN.REDO.COL.MSG.QUEUE
    Y.SELECT.TABLES := ' WITH @ID LIKE ':Y.TABLE:'.':TODAY:'.':ID.COMPANY:"... SAMPLE 1"
    Y.ERR = ''
    Y.F.REDO.MSG.QUEUE = ''
    Y.LIST.MSG = ''
    NO.REC.SEL = 0
    CALL EB.READLIST(Y.SELECT.TABLES,Y.F.REDO.MSG.QUEUE,Y.LIST.MSG,NO.REC.SEL,Y.ERR)

RETURN
*-----------------------------------------------------------------------------
END
