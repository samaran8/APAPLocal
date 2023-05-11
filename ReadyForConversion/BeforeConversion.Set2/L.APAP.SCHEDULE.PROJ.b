*-----------------------------------------------------------------------------
* <Rating>-42</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.SCHEDULE.PROJ(NEXT.ID)
*--------------------------------------------------------------------------------
* Company Name : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Program Name : L.APAP.SCHEDULE.PROJ
*--------------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
*
*  DATE             WHO               DESCRIPTION
*  20200530         ELMENDEZ              INITIAL CREATION
*  20210416         APAP               Optimizar el servicio para mejor perfomance
*
*-----------------------------------------------------------------------------
    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_EQUATE
    $INCLUDE LAPAP.BP I_L.APAP.SCHEDULE.PROJ.COMMON
    $INCLUDE T24.BP I_F.AA.ARRANGEMENT
    $INCLUDE BP I_F.EB.L.APAP.SCHEDULE.PROJET
*-----------------------------------------------------------------------------
    ID.SCHEDULE.PROJ = ''
    R.SCHEDULE.PROJ = ''
*CALL OCOMO("LPROCESANDO>" : NEXT.ID  : "*" : Y.TODAY)
    GOSUB INIT
    RETURN
INIT:
    GOSUB READFILE
    IF R.AA.ARRANGEMENT<AA.ARR.PRODUCT.LINE> EQ 'LENDING' THEN
        CALL OCOMO("LPROCESANDO>" : NEXT.ID  : "*" : Y.TODAY)
        GOSUB PROCESS
        GOSUB WRITEDATA

    END
    RETURN

READFILE:
    CALL F.READ(FN.AA.ARRANGEMENT,NEXT.ID,R.AA.ARRANGEMENT,F.AA.ARRANGEMENT,AA.ARRANGEMENT.ERR)
    RETURN

PROCESS:
    YR.MTH = 12
    YYR.MTH = 12
    SIMULATION.REF = '';
    NO.RESET = ''
    TOT.PAYMENT = '';
    DUE.DATES = '';
    DUE.TYPES = '';
    DUE.DEFER.DATES = '';
    DATE.RANGE = '';
*yTOT.PAYMENT = 0
    DUE.METHODS = '';
    DUE.TYPE.AMTS = '';
    DUE.PROPS = '';
    DUE.PROP.AMTS = '';
    DUE.OUTS = '';
    BALANCES.TO.CHECK = '';
    TOT.BALANCES = '';
    TOTAL.REVENU  = 0;
    CALL AA.SCHEDULE.PROJECTOR(NEXT.ID, SIMULATION.REF, NO.RESET, DATE.RANGE, TOT.PAYMENT, DUE.DATES, DUE.DEFER.DATES, DUE.TYPES, DUE.METHODS,DUE.TYPE.AMTS, DUE.PROPS, DUE.PROP.AMTS, DUE.OUTS)
    RETURN

WRITEDATA:
    CHANGE @VM TO @SM IN DUE.TYPES
    CHANGE @VM TO @SM IN DUE.METHODS
    CHANGE @VM TO @SM IN DUE.TYPE.AMTS
    CHANGE @VM TO @SM IN DUE.PROPS
    CHANGE @VM TO @SM IN DUE.PROP.AMTS
    CHANGE @FM TO @VM IN TOT.PAYMENT
    CHANGE @FM TO @VM IN DUE.DATES
    CHANGE @FM TO @VM IN DUE.DEFER.DATES
    CHANGE @FM TO @VM IN DUE.TYPES
    CHANGE @FM TO @VM IN DUE.METHODS
    CHANGE @FM TO @VM IN DUE.TYPE.AMTS
    CHANGE @FM TO @VM IN DUE.PROPS
    CHANGE @FM TO @VM IN DUE.PROP.AMTS
    CHANGE @FM TO @VM IN DUE.OUTS
    ID.SCHEDULE.PROJ = NEXT.ID : '*' : Y.TODAY
    R.SCHEDULE.PROJ<EB.SP.ARR.STATUS>       = R.AA.ARRANGEMENT<AA.ARR.ARR.STATUS>
    R.SCHEDULE.PROJ<EB.SP.PRODUCT.STATUS>   = R.AA.ARRANGEMENT<AA.ARR.PRODUCT.STATUS>
    R.SCHEDULE.PROJ<EB.SP.ACCOUNT>          = R.AA.ARRANGEMENT<AA.ARR.LINKED.APPL.ID>
    R.SCHEDULE.PROJ<EB.SP.TOT.PAYMENT>      = TOT.PAYMENT
    R.SCHEDULE.PROJ<EB.SP.DUE.DATES>        = DUE.DATES
    R.SCHEDULE.PROJ<EB.SP.DUE.DEFER.DATES>  = DUE.DEFER.DATES
    R.SCHEDULE.PROJ<EB.SP.DUE.TYPES>        = DUE.TYPES
    R.SCHEDULE.PROJ<EB.SP.DUE.METHODS>      = DUE.METHODS
    R.SCHEDULE.PROJ<EB.SP.DUE.TYPE.AMTS>    = DUE.TYPE.AMTS
    R.SCHEDULE.PROJ<EB.SP.DUE.PROPS>        = DUE.PROPS
    R.SCHEDULE.PROJ<EB.SP.DUE.PROP.AMTS>    = DUE.PROP.AMTS
    R.SCHEDULE.PROJ<EB.SP.DUE.OUTS>         = DUE.OUTS
    R.SCHEDULE.PROJ<EB.SP.CO.CODE>          = ID.COMPANY
**----------------------------------------------------------------------todo
    CALL F.WRITE(FN.SCHEDULE.PROJ, ID.SCHEDULE.PROJ, R.SCHEDULE.PROJ)

    RETURN

END
