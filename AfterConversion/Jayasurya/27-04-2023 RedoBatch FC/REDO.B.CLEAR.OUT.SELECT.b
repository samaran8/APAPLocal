* @ValidationCode : MjotMTI1NTQwOTQxOTpDcDEyNTI6MTY4MjUxMjg3MDA2NDpJVFNTQk5HOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 26 Apr 2023 18:11:10
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.CLEAR.OUT.SELECT
*****************************************************************************************
*----------------------------------------------------------------------------------------
* Company Name  : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS.
* Developed By  : Arulprakasam P
* Program Name  : REDO.B.CLEAR.OUT.SELECT
****-----------------------------------------------------------------------------------------
* Description:
* This routine is a multithreaded routine to select the records in the mentioned applns
****------------------------------------------------------------------------------------------
* Linked with:
* In parameter :
* out parameter : None
*------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
* DATE              REFERENCE            DESCRIPTION
* 23.11.2010        ODR-2010-09-0251     INITIAL CREATION
* Date                  who                   Reference
* 10-04-2023        �CONVERSTION TOOL   �  R22 AUTO CONVERSTION - No Change
* 10-04-2023          ANIL KUMAR B         R22 MANUAL CONVERSTION  CALL RTN METHOD ADDED
*------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_REDO.B.CLEAR.OUT.COMMON
    $USING APAP.REDOCHNLS

    SEL.CMD = "SELECT ":VAR.FILE.PATH
    CALL EB.READLIST(SEL.CMD,FILE.LIST,'',NO.OF.REC,RET.ERR)

    IF FILE.LIST THEN
        CALL BATCH.BUILD.LIST('',FILE.LIST)
    END ELSE
        INT.CODE ='APA002'
        INT.TYPE ='BATCH'
        BAT.NO =''
        BAT.TOT =''
        INFO.OR =''
        INFO.DE =''
        ID.PROC = FILE.LIST
        MON.TP ='03'
        DESC = RET.ERR
        Y.REC.CON = ''
        EX.USER = ''
        EX.PC = ''
*CALL APAP.REDOCHNLS.REDO.INTERFACE.REC.ACT(INT.CODE,INT.TYPE,BAT.NO,BAT.TOT,INFO.OR,INFO.DE,ID.PROC,MON.TP,DESC,Y.REC.CON,EX.USER,EX.PC) ;* MANUAL R22 CODE CONVERSION
        CALL APAP.REDOCHNLS.redoInterfaceRecAct(INT.CODE,INT.TYPE,BAT.NO,BAT.TOT,INFO.OR,INFO.DE,ID.PROC,MON.TP,DESC,Y.REC.CON,EX.USER,EX.PC) ;* MANUAL R22 CODE CONVERSION
    END

RETURN
*------------------------------------------------------------------------------------------
END
