* @ValidationCode : MjoxNDcyNDIxODI5OkNwMTI1MjoxNjgxMTA0OTg1NjMzOklUU1M6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 10 Apr 2023 11:06:25
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.CAPITALISE.INT.SELECT
*****************************************************************************************
*----------------------------------------------------------------------------------------
* Company Name  : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By  : Arulprakasam P
* Program Name  : REDO.B.CAPITALISE.INT.SELECT
*-----------------------------------------------------------------------------------------
* Description:
* This routine is a multithreaded routine to select the records in the mentioned applns
*------------------------------------------------------------------------------------------
* Linked with:
* In parameter :
* out parameter : None
*------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
* DATE              REFERENCE            DESCRIPTION
* 17.12.2010        ODR-2010-09-0251     INITIAL CREATION
* Date                  who                   Reference              
* 10-04-2023        �CONVERSTION TOOL   �  R22 AUTO CONVERSTION - No Change
* 10-04-2023          ANIL KUMAR B         R22 MANUAL CONVERSTION -NO CHANGES
*------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_REDO.B.CAPITALISE.COMMON


    SEL.CMD = ''
    LIST.INTEREST = ''
    BATCH.LIST.IDS = ''
    LOC.ID.VARIABLE = ''

    SEL.CMD = "SELECT ":FN.INT.REVERSE:" BY ACCOUNT"
    CALL EB.READLIST(SEL.CMD,LIST.INTEREST,'',NO.OF.REC,ERR)

    LOOP
        REMOVE INTEREST.ID FROM LIST.INTEREST SETTING INT.POS
    WHILE INTEREST.ID:INT.POS

        LOCATE INTEREST.ID IN LOC.ID.VARIABLE SETTING LOC.POS THEN

            BATCH.LIST.IDS<LOC.POS,-1> = INTEREST.ID
        END ELSE
            LOC.ID.VARIABLE<-1> = INTEREST.ID

            BATCH.LIST.IDS<-1> = INTEREST.ID
        END

    REPEAT

    CALL BATCH.BUILD.LIST('',BATCH.LIST.IDS)

RETURN
*------------------------------------------------------------------------------------------
END
