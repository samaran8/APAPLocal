SUBROUTINE REDO.DEL.CL.BALANCE (AA.ID,COLL.ID)
*
* ====================================================================================
*
*
* ====================================================================================
*
* Subroutine Type :ROUTINE
* Attached to     :CURRENCY HOLD.FIELD IN REDO.CREATE.ARRANGEMENT TEMPLATE
* Attached as     :HOLD.FIELD in CURRENCY field OF REDO.CREATE.ARRANGEMENT TEMPLATE
* Primary Purpose :VALIDATE WHAT KIND OF CURRENCY IS POSIBLE TO INPUT OF HEADER TO REDO.CREATE.ARRANGEMENT TEMPLATE
*
*
* Incoming:
* ---------
*
*
*
* Outgoing:

* ---------
*
*
*-----------------------------------------------------------------------------------
* Modification History:
*
* Development for : Asociacion Popular de Ahorros y Prestamos
* Development by  : Jorge Valarezo - TAM Latin America
* Date            : Feb 6 2013
*=======================================================================

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.COLLATERAL
    $INSERT I_F.REDO.FC.CL.BALANCE


*
* -------------------------------------------------------------------------------


    GOSUB INITIALISE
    GOSUB OPEN.FILES
    GOSUB PROCESS

*
RETURN


*
* ======
PROCESS:
* ======
    GOSUB VERIFICA.TEMPLATE

    IF NOT(VAR.BANDERA)  THEN
        RETURN
    END
    CALL F.READ(FN.REDO.FC.CL.BALANCE,Y.AA.ID,R.REDO.FC.CL.BALANCE,F.REDO.FC.CL.BALANCE,Y.BALANCE.ERR.MSJ)

    LOCATE Y.COLL.ID IN R.REDO.FC.CL.BALANCE<FC.CL.COLLATERAL.ID,1> SETTING Y.COLL.POS THEN
        DEL R.REDO.FC.CL.BALANCE<FC.CL.COLLATERAL.RIGHT,Y.COLL.POS>
        DEL R.REDO.FC.CL.BALANCE<FC.CL.COLLATERAL.ID,Y.COLL.POS>
        DEL R.REDO.FC.CL.BALANCE<FC.CL.MG.ACTUAL,Y.COLL.POS>
        DEL R.REDO.FC.CL.BALANCE<FC.CL.MG.ORIGINAL,Y.COLL.POS>
        CALL F.WRITE(FN.REDO.FC.CL.BALANCE,Y.AA.ID,R.REDO.FC.CL.BALANCE)
    END

RETURN

*
* =========
OPEN.FILES:
* =========
*
    CALL OPF(FN.REDO.FC.CL.BALANCE,F.REDO.FC.CL.BALANCE)
    CALL OPF(FN.TEMPLATE, F.TEMPLATE)
RETURN
*
* =========
INITIALISE:
* =========
*
    Y.COLL.ID = COLL.ID
    Y.COLL.POS = ''
    Y.AA.ID = AA.ID
*BALANCE
    FN.REDO.FC.CL.BALANCE = 'F.REDO.FC.CL.BALANCE'
    F.REDO.FC.CL.BALANCE  = ''
    R.REDO.FC.CL.BALANCE  = ''

*GET TEMPLATE INFORMATION
    FN.TEMPLATE = 'F.REDO.CREATE.ARRANGEMENT'
    F.TEMPLATE  = ''
    R.TEMPALTE  = ''
RETURN

*---------------
VERIFICA.TEMPLATE:
*=================

    VAR.BANDERA = ''

    SELECT.STATEMENT = 'SELECT ':FN.TEMPLATE:' WITH ID.ARRANGEMENT EQ ':Y.AA.ID
    LOCK.LIST = ''
    LIST.NAME = ''
    SELECTED = ''
    SYSTEM.RETURN.CODE = ''
    Y.ID.AA.PRD = ''
    CALL EB.READLIST(SELECT.STATEMENT,LOCK.LIST,LIST.NAME,SELECTED,SYSTEM.RETURN.CODE)

    IF SELECTED EQ 1 THEN
        VAR.BANDERA = 'TRUE'
        RETURN
    END

*GET ALL VALUES FROM THE LIST AND ADD THE LOCK VALUE
*      LOOP
*         REMOVE Y.ID.AA.PRD FROM LOCK.LIST SETTING POS
*      WHILE Y.ID.AA.PRD:POS
*         VAR.BANDERA = 'TRUE'
*      REPEAT

RETURN
END
