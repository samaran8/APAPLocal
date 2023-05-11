$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.E.BUILD.GET.OWN.CUSTOMER(ENQ.DATA)
************************************************************
*----------------------------------------------------------
* Company Name : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By : PRABHU N
* Program Name :REDO.E.BUILD.GET.OWN.CUSTOMER
*----------------------------------------------------------

* Description   : This subroutine is used to set selection for ARC Enquiries

* Linked with   : ARC ENQUIRIES
* In Parameter  : ENQ.DATA
* Out Parameter : ENQ.DATA
*-----------------------------------------------------------------------------
*---------------------------------------------------------------------------------
*MODIFICATION:
*---------------------------------------------------------------------------------
*DATE           ODR                   DEVELOPER               VERSION
*--------       ----------------      -------------           --------------------
*07.04.2011     PACS00036498           Prabhu N            INITIAL CREATION
* 10-APRIL-2023      Conversion Tool       R22 Auto Conversion  - VM to @VM and Added IF E EQ "EB-UNKNOWN.VARIABLE"
* 10-APRIL-2023      Harsha                R22 Manual Conversion - No changes 
*---------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_EB.EXTERNAL.COMMON
    $INSERT I_System

    GOSUB INIT
    GOSUB PROCESS
RETURN
*-----
INIT:
*-----
    Y.CUSTOMER= System.getVariable('EXT.SMS.CUSTOMERS')
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN     ;*R22 Auto Conversion  - Added IF E EQ "EB-UNKNOWN.VARIABLE"
        Y.CUSTOMER = ""
    END
RETURN
*-------
PROCESS:
*-------
    Y.FIELD.COUNT=DCOUNT(ENQ.DATA<2>,@VM)
    ENQ.DATA<2,Y.FIELD.COUNT+1>= 'OWNING.CUSTOMER'
    ENQ.DATA<3,Y.FIELD.COUNT+1>= 'EQ'
    ENQ.DATA<4,Y.FIELD.COUNT+1> = Y.CUSTOMER
RETURN
END
