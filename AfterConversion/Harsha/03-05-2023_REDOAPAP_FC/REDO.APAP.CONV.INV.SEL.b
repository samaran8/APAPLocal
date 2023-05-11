* @ValidationCode : MjoxMTgwMDkwODk6Q3AxMjUyOjE2ODI1MDI2MjY0MzM6SVRTUzotMTotMTotMTQ6MTpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 26 Apr 2023 15:20:26
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : -14
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOAPAP
SUBROUTINE REDO.APAP.CONV.INV.SEL
*********************************************************************************************************
*Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Developed By      : Temenos Application Management
*Program   Name    : REDO.APAP.CONV.INV.SEL
*--------------------------------------------------------------------------------------------------------
*Description       : REDO.APAP.CONV.INV.SEL is a conversion routine attached to the ENQUIRY>
*                    REDO.APAP.INVST.RATE, the routine fetches the value from O.DATA delimited
*                    with stars and formats them according to the selection criteria and returns the value
*                     back to O.DATA
*Linked With       :
*In  Parameter     : N/A
*Out Parameter     : N/A
*--------------------------------------------------------------------------------------------------------
*Modification Details:
*=====================
*      Date                 Who                  Reference                 Description
*     ------               -----               -------------              -------------
* 20 OCT 2010              Dhamu S             ODR-2010-03-0098            Initial Creation
* Date                  who                   Reference              
* 05-04-2023        �CONVERSTION TOOL   �  R22 AUTO CONVERSTION - No Change
* 05-04-2023          ANIL KUMAR B         R22 MANUAL CONVERSTION -NO CHANGES
*
*********************************************************************************************************
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
*-------------------------------------------------------------------------------------------------------
**********
MAIN.PARA:
**********

    GOSUB PROCESS.PARA

RETURN
*------------------------------------------------------------------------------------------------------
*************
PROCESS.PARA:
*************

    Y.RESULT = ''
    O.DATA = ''

    LOCATE 'CO.CODE' IN ENQ.SELECTION<2,1> SETTING Y.CO.POS THEN
        Y.RESULT     := "Agencia - ":ENQ.SELECTION<4,Y.CO.POS>:'; '
    END

    LOCATE 'CATEGORY' IN ENQ.SELECTION<2,1> SETTING Y.CAT.POS THEN
        Y.RESULT     := " Tipo de Instrumento  - ":ENQ.SELECTION<4,Y.CAT.POS>:'; '
    END

    LOCATE 'CREATE.DATE' IN ENQ.SELECTION<2,1> SETTING Y.DATE.POS THEN
        Y.DATES = ENQ.SELECTION<4,Y.DATE.POS>
        IF INDEX(Y.DATES, ' ', 1) THEN
            Y.DATE1 = FIELD(Y.DATES, ' ', 1)
            Y.DATE2 = FIELD(Y.DATES, ' ', 2)

            CALL EB.DATE.FORMAT.DISPLAY(Y.DATE1, Y.FMT.DATE1, '', '')
            CALL EB.DATE.FORMAT.DISPLAY(Y.DATE2, Y.FMT.DATE2, '', '')

            Y.RESULT := " Fecha de creacion - ":Y.FMT.DATE1:" a ":Y.FMT.DATE2:"; "

        END ELSE

            CALL EB.DATE.FORMAT.DISPLAY(Y.DATES, Y.FMT.DATE, '', '')
            Y.RESULT := " Fecha de creacion - ":Y.FMT.DATE:'; '

        END
    END

    IF Y.RESULT ELSE
        Y.RESULT = 'TODOS'
    END
    O.DATA = Y.RESULT

RETURN
END
*------------------------------------------------------------------------------------------------------
