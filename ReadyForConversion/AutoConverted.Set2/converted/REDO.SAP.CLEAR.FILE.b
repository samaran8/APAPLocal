SUBROUTINE REDO.SAP.CLEAR.FILE

* Company Name : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By :
* Program Name :
*-----------------------------------------------------------------------------
* Description : For clearing the SAP directory during COB
* Linked with :
* In Parameter :
* Out Parameter : None
*
**DATE           ODR                   DEVELOPER               VERSION
*
*09/10/11       PACS00071961            PRABHU N                MODIFICAION
*
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_BATCH.FILES
    $INSERT I_F.REDO.GL.H.EXTRACT.PARAMETER


    GOSUB OPEN.FILE
    GOSUB READ.PARAM.FILE
    GOSUB TAKE.BACKUP
    GOSUB CLEAR.SAP.DIR

RETURN
*---------*
OPEN.FILE:
*---------*

    FN.REDO.GL.H.EXTRACT.PARAMETER = 'F.REDO.GL.H.EXTRACT.PARAMETER'
    F.REDO.GL.H.EXTRACT.PARAMETER  = ''
    CALL OPF(FN.REDO.GL.H.EXTRACT.PARAMETER,F.REDO.GL.H.EXTRACT.PARAMETER)

RETURN
*---------------*
READ.PARAM.FILE:
*----------------
    R.REDO.GL.H.EXTRACT.PARAMETER  = ''
    REDO.GL.H.EXTRACT.PARAMETER.ER = ''
    CALL CACHE.READ(FN.REDO.GL.H.EXTRACT.PARAMETER,'SYSTEM',R.REDO.GL.H.EXTRACT.PARAMETER,REDO.GL.H.EXTRACT.PARAMETER.ER)

RETURN
*--------------*
TAKE.BACKUP:
*--------------*

    BACKUP.DIR = R.REDO.GL.H.EXTRACT.PARAMETER<SAP.EP.BACKUP.PATH>
    SOURCE.DIR = R.REDO.GL.H.EXTRACT.PARAMETER<SAP.EP.EXTRACT.OUT.PATH,1>

    COPY.CMD = "COPY FROM ":SOURCE.DIR:" TO ":BACKUP.DIR:" ALL"
    EXECUTE COPY.CMD


RETURN

*--------------*
CLEAR.SAP.DIR:
*--------------*
    CLEAR.DIR= R.REDO.GL.H.EXTRACT.PARAMETER<SAP.EP.EXTRACT.OUT.PATH,1>
    EXE.CMD = 'CLEAR.FILE ':CLEAR.DIR
    EXECUTE EXE.CMD

RETURN

END
