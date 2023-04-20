SUBROUTINE L.APAP.FACILIDAD.ID

* Date         : 2021-12-06
* Description  : Setea un valor al campo L.CR.FACILITY en la tabla : F.REDO.CREATE.ARRANGEMENT
*                 dependiendo el tipo de producto campo PRODUCT basada en la tabla de parametro:REDO.H.REPORTS.PARAM
*                 registro LAPAP.FAC.TEMPLEATE
*               Se llama la rutina : REDO.FC.S.CL.OVERDS, del proveedor TEMENOS antes de aplicar la logica para no perder
*               la funcionalidad actual
*
*------------------------------------------------------------------------------------------------------------------
* Input/Output:
* -------------
* In  :
* Out :
*------------------------------------------------------------------------------------------------------------------
* Revision History:
* -----------------
* Version          Date          Name
* -------          ----          ----
* 1.1              2021-12-06    APAP
*------------------------------------------------------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.CREATE.ARRANGEMENT
    $INSERT I_F.REDO.H.REPORTS.PARAM



    Y.PRODUCTO = R.NEW(REDO.FC.PRODUCT)


**Y.PRODUCTO = 'CONS.CG.CTA.AHORRO2'
    GOSUB  TABLAS
    GOSUB LEER.TABLA.PARAMETROS

RETURN

TABLAS:
    FN.REDO.H.REPORTS.PARAM = "F.REDO.H.REPORTS.PARAM"
    F.REDO.H.REPORTS.PARAM  = ""
    CALL OPF(FN.REDO.H.REPORTS.PARAM,F.REDO.H.REPORTS.PARAM)


RETURN


LEER.TABLA.PARAMETROS:
    ID.REPORT = 'LAPAP.FAC.TEMPLEATE'
    CALL F.READ(FN.REDO.H.REPORTS.PARAM,ID.REPORT,R.REDO.H.REPORTS.PARAM,F.REDO.H.REPORTS.PARAM,ERROR.REPORT.PARAM)
    Y.FIELD.NME = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.FIELD.NAME>
    Y.FIELD.VAL = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.FIELD.VALUE>
    LOCATE Y.PRODUCTO IN Y.FIELD.NME<1,1> SETTING COD.POS THEN
        Y.FACILIDAD.ID = Y.FIELD.VAL<1,COD.POS>
*** Recibir un valor en un campo local
        CALL GET.LOC.REF("REDO.CREATE.ARRANGEMENT","L.CR.FACILITY",Y.L.CR.FACILITY.POS)
        R.NEW(REDO.FC.LOCAL.REF)<1,Y.L.CR.FACILITY.POS> =  Y.FACILIDAD.ID

***Colocar un campo local como no input
        T.LOCREF<Y.L.CR.FACILITY.POS,7> = "NOINPUT"
    END

RETURN


END
