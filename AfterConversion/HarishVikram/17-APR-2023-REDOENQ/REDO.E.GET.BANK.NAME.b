* @ValidationCode : MjotOTE5MjY5MjY5OkNwMTI1MjoxNjgxNzIxNDA2MjY3OkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 17 Apr 2023 14:20:06
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.E.GET.BANK.NAME
*********************************************************************************************************
*Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Developed By      : Temenos Application Management
*Program   Name    : REDO.E.GET.BANK.NAME
*--------------------------------------------------------------------------------------------------------
*Description       : This is a CONVERSION routine attached to an enquiry, the routine fetches the value
*                    from O.DATA delimited with stars and formats them according to the selection criteria
*                    and returns the value back to O.DATA
*Linked With       : Enquiry REDO.REJECTED.CHEQUES
*In  Parameter     : N/A
*Out Parameter     : N/A
*Files  Used       : N/A
*--------------------------------------------------------------------------------------------------------
*Modification Details:
*=====================
*      Date                 Who                  Reference                 Description
*     ------               -----               -------------              -------------
*    19 04 2012           Ganesh R          ODR-2010-09-0148           Initial Creation
*
* 17-APR-2023     Conversion tool   R22 Auto conversion   		 ++ to +=
* 17-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*********************************************************************************************************
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
*-------------------------------------------------------------------------------------------------------
**********
MAIN.PARA:
**********
* This is the para from where the execution of the code starts
    GOSUB PROCESS.PARA

RETURN
*--------------------------------------------------------------------------------------------------------
*************
PROCESS.PARA:
*************
* This is the main processing para
    Y.BANK.ID = O.DATA

    Y.BANK.ID += 1
    Y.BANK.ID -= 1

    FN.BANK.NAME = 'F.REDO.OTH.BANK.NAME'
    F.BANK.NAME = ''
    CALL OPF(FN.BANK.NAME,F.BANK.NAME)

    CALL F.READ(FN.BANK.NAME,Y.BANK.ID,R.BANK.NAME,F.BANK.NAME,BANK.ERR)
    Y.GET.DETAILS = R.BANK.NAME<1>
    O.DATA = FIELD(Y.GET.DETAILS,'*',1)

RETURN
*--------------------------------------------------------------------------------------------------------
END       ;* End of program
