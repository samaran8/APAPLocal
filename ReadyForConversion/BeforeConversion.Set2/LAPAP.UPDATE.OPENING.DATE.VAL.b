*-----------------------------------------------------------------------------
* <Rating>-1</Rating>
*-----------------------------------------------------------------------------
 SUBROUTINE LAPAP.UPDATE.OPENING.DATE.VAL
*------------------------------------------------------------------------------------*
* Technical report:                                                                  *
* -----------------                                                                  *
* Company Name   : APAP                                                              *
* Program Name   : LAPAP.UPDATE.OPENING.DATE.VAL                                     *
* Date           : 2017-09-09                                                        *
* Author         : RichardHC                                                         *
* Item ID        : CN007111                                                          *
*------------------------------------------------------------------------------------*
* Description :                                                                      *
* ------------                                                                       *
* This program allow verify whethe the date is greater that today                    *
*------------------------------------------------------------------------------------*
* Modification History :                                                             *
* ----------------------                                                             *
* Date           Author            Modification Description                          *
* -------------  -----------       ---------------------------                       *
*                                                                                    *
*------------------------------------------------------------------------------------*
* Content summary :                                                                  *
* -----------------                                                                  *
* Table name     : ST.LAPAP.UPDATE.OPENING.DATE                                      *
* Auto Increment : LAPAP.UPDATE.OPENING.DATE                                         *
* Views/versions : ST.LAPAP.UPDATE.OPENING.DATE,INPUT/,DETAILS                       *
* EB record      : LAPAP.UPDATE.OPENING.DATE.VAL/LAPAP.UPDATE.OPENING.DATE           *
* Routines       : LAPAP.UPDATE.OPENING.DATE.VAL/LAPAP.UPDATE.OPENING.DATE           *
*------------------------------------------------------------------------------------*

*Importing the neccessary libraries and tables. 
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT BP I_F.ST.LAPAP.UPDATE.OPENING.DATE


*Capturing data from browser layer and assigning those values to beside variables.
*VAR.ACC2 = R.NEW(ST.LAP14.OPENING.DATE)

    VAR.ACC2 = COMI

*Validating the field imputted


    IF VAR.ACC2 > TODAY THEN

        ETEXT = "INTRODUZCA UNA FECHA MENOR AL DIA DE HOY"

        CALL STORE.END.ERROR

    END
