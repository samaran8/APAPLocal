*-----------------------------------------------------------------------------
* <Rating>-10</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.COB.PENDING
*----------------------------------------------------------------------------------------------------
0006      * Description           : Rutina MAIN para Habilitar autorización al servicio del COB de T24
0007      *
0008      * Developed On          : 15-02-2022
0009      * Developed By          : APAP
0010      * Development Reference : MDR-1381
0011      *----------------------------------------------------------------------------------------------------

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.VERSION
    $INSERT T24.BP I_F.TSA.SERVICE
    GOSUB MAIN.PROCESS
    RETURN


MAIN.PROCESS:
    Y.ID = ID.NEW
    *Y.CURR.NO= DCOUNT(R.NEW(TS.TSM.CURR.NO),VM) 
    Y.CURR.NO=0
    IF Y.ID EQ 'COB'  AND V$FUNCTION EQ 'I' THEN
    *TEXT = "EL COB REQUIERE AUTORIZACION DE OTRO USUARIO"
    *TEXT = "L.APAP.REPRECIO.OVR.MSG"
        R.VERSION(EB.VER.NO.OF.AUTH) = 1
        *CALL STORE.OVERRIDE(Y.CURR.NO)
    END

    RETURN
