* @ValidationCode : Mjo5MTg3NDE4NjE6Q3AxMjUyOjE2ODQ0MTI4NzkwMzA6c2FtYXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 18 May 2023 17:57:59
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : samar
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
*---------------------------------------------------------------------------------------
*MODIFICATION HISTORY:
*DATE           WHO                 REFERENCE               DESCRIPTION
*24-APR-2023    CONVERSION TOOL     R22 AUTO CONVERSION     NO CHANGE
*24-APR-2023    VICTORIA S          R22 MANUAL CONVERSION   NO CHANGE
*----------------------------------------------------------------------------------------

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
